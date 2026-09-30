# ============================================================
# Empirical validation: pertussis, full-year 2022 reconstruction
# ============================================================
#
# PURPOSE
# -------
# Comment 3 from the senior collaborator, same design as the dengue
# 2024 and malaria 2022 validations.
#
# WHY 2022
# --------
# Matches the malaria validation year, for consistency, and because
# a complete official state-level table was found (CDC "2022 Final
# Pertussis Surveillance Report"). Independent of the headline
# model's 2024-based construction.
#
# INCIDENCE METRIC
# -----------------
# Like malaria, WHO pertussis incidence is only available as an
# ANNUAL rate (per-country, not monthly), so this sums COR arrivals
# across all 12 months of 2022 and applies the single 2022 annual
# incidence rate directly (no /12 monthly approximation, unlike the
# headline model, which pairs a monthly-approximated rate with
# June-only arrivals).
#
# PREREQUISITE
# ------------
# Run importationRisk_main_with_uncertainty_rho_corrected.R first
# (through the point where arrivals_COR, t100_routing,
# population_of_world, under_rho_pertussis, p_travel_inf_pertussis,
# compute_importation_country_level(), and compute_mc_summary()
# exist in the session).
#
# REPORTED DATA SOURCE
# ----------------------
# CDC "2022 Final Pertussis Surveillance Report" (CS292279, May
# 2024), state-level incidence/case table, read directly from the
# PDF. As with the malaria NNDSS table, New York state (212) and
# New York City (107) are reported separately; 107 (NYC) is used
# below since our "New York" host city represents NYC specifically.
# ============================================================

# ---- 1. 2022 annual arrivals: sum all 12 months, then x T-100 ------
# (Identical construction to validation_malaria_2022_annual.R --
# rebuilt here so this script can run standalone.)
cols_2022 <- c(paste0("2022-", 1:9), "2022-10", "2022-11", "2022-12")

arrivals_2022_annual <- arrivals_COR %>%
  select(Country, World_region, all_of(cols_2022)) %>%
  mutate(across(all_of(cols_2022), ~ readr::parse_number(as.character(.)))) %>%
  rowwise() %>%
  mutate(arrivals_annual_2022 = sum(c_across(all_of(cols_2022)), na.rm = TRUE)) %>%
  ungroup() %>%
  select(Country, World_region, arrivals_annual_2022) %>%
  drop_na()

arrivals_2022 <- arrivals_2022_annual %>%
  left_join(t100_routing, by = "Country") %>%
  drop_na(venue_city) %>%
  mutate(
    arrivals_june_2026 = arrivals_annual_2022 * routing_fraction,  # name kept
    destination_city   = venue_city                                 # for compat
  ) %>%
  select(Country, destination_city, arrivals_june_2026)

# ---- 2. 2022 annual pertussis incidence (column 6 = year 2022;
#         see header note on file layout) ----------------------------
pertussis_2022_country_inc <- pertussis_data %>%
  select(Country = 1, incidence_per1M = 6) %>%
  drop_na() %>%
  mutate(incidence_per1M = as.numeric(incidence_per1M)) %>%
  mutate(total_inc = incidence_per1M / (1e6 * under_rho_pertussis)) %>%
  select(Country, total_inc)

# ---- 3. Central (deterministic) 2022 annual pertussis estimate -----
pertussis_2022_results <- compute_importation_country_level(
  arrivals_df    = arrivals_2022,
  country_inc_df = pertussis_2022_country_inc,
  p_travel_inf   = p_travel_inf_pertussis,
  title_text     = "Pertussis importation intensity -- annual 2022 (validation)"
)

# ---- 4. Monte Carlo uncertainty, same literature ranges as main ----
pertussis_2022_mc <- compute_mc_summary(
  pertussis_2022_results$importation$imp_intensity,
  pertussis_2022_results$importation$destination_city,
  under_rho_pertussis, p_travel_inf_pertussis,
  0.01, 0.10, 0.50, 0.90,
  n_mc = 5000
)

# ---- 5. Aggregate to state level ------------------------------------
city_to_state <- tibble(
  destination_city = c("New York", "Miami", "Atlanta", "Los Angeles",
                        "Houston", "Dallas", "San Francisco", "Seattle",
                        "Boston", "Philadelphia", "Kansas City"),
  state = c("NY", "FL", "GA", "CA",
            "TX", "TX", "CA", "WA",
            "MA", "PA", "MO")
)

pertussis_2022_by_state <- pertussis_2022_mc %>%
  left_join(city_to_state, by = "destination_city") %>%
  group_by(state) %>%
  summarise(
    lambda_median = sum(lambda_median),
    lambda_lo     = sum(lambda_lo),
    lambda_hi     = sum(lambda_hi),
    .groups = "drop"
  ) %>%
  arrange(desc(lambda_median))

# ---- 6. Compare against CDC 2022 Pertussis Surveillance Report -----
# NY uses the NYC-specific figure (107), not the whole-state total
# (319 = 212 rest-of-state + 107 NYC).
reported_pertussis_2022 <- tibble(
  state          = c("FL", "CA", "NY",  "TX",  "MA", "GA", "WA", "PA", "MO"),
  reported_cases = c(60,   291,  107,   193,   4,    88,   76,   99,   26)
)

validation_comparison_pertussis_2022 <- pertussis_2022_by_state %>%
  left_join(reported_pertussis_2022, by = "state") %>%
  mutate(implied_detection_fraction = reported_cases / lambda_median)

cat("\n=== PERTUSSIS VALIDATION: full year 2022, by state ===\n")
cat("Lambda = model-estimated TRUE infected arrivals (not reported cases)\n")
cat("reported_cases: NY is NYC-specific (107), not whole-state (319)\n")
cat("Note: pertussis is NOT travel-specific surveillance -- reported\n")
cat("cases include locally-circulating pertussis, not just imported\n")
cat("cases, unlike the dengue and malaria comparisons. This means\n")
cat("reported_cases here is likely to substantially OVER-state what\n")
cat("Lambda alone should explain, since most US pertussis is endemic\n")
cat("transmission, not importation. Interpret implied_detection_\n");
cat("fraction with that in mind -- a low value here does not\n")
cat("necessarily indicate the same kind of problem it would for\n")
cat("dengue or malaria, which are overwhelmingly travel-associated\n")
cat("in the US.\n\n")
print(validation_comparison_pertussis_2022, n = Inf)
