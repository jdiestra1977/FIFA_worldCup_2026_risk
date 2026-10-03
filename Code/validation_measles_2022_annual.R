# ============================================================
# Model evaluation: measles, full-year 2022 reconstruction (Table 2)
# ============================================================
#
# PURPOSE
# -------
# Compares an annual 2022 measles Lambda with NNDSS 2022 imported
# measles cases by state. NNDSS separates imported from indigenous
# cases, so Lambda (independent importation events) is compared with
# the imported count only, not with cases from local outbreak chains.
# 2022 matches the malaria and pertussis evaluation year. p_d uses
# T_d = 365.25 days (p_*_measles_annual, main script Section 5g).
#
# WHAT "0" MEANS HERE
# ---------------------
# The NNDSS table's "-" symbol is confirmed (via the table's own
# legend) to mean a genuine zero -- the state reported no cases --
# not suppressed or unavailable data. Only Washington had a nonzero
# imported count among our 9 states.
#
# PREREQUISITE
# ------------
# Run importationRisk_main.R first
# (through the point where arrivals_COR, t100_routing,
# population_of_world, under_rho_measles, p_travel_inf_measles_annual,
# compute_importation_country_level(), and compute_mc_summary()
# exist in the session).
#
# REPORTED DATA SOURCE
# ----------------------
# NNDSS Table 2k, "Measles Imported" column, 2022
# (https://stacks.cdc.gov/view/cdc/175693).
# ============================================================

# ---- 1. 2022 annual arrivals: sum all 12 months, then x T-100 ------
# (Identical construction to the malaria/pertussis 2022 scripts --
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

# ---- 2. 2022 annual measles incidence (column 6 = year 2022) -------
measles_2022_country_inc <- measles_data %>%
  select(Country = 1, incidence_per1M = 6) %>%
  drop_na() %>%
  mutate(incidence_per1M = as.numeric(incidence_per1M)) %>%
  mutate(total_inc = incidence_per1M / (1e6 * under_rho_measles)) %>%
  select(Country, total_inc)

# ---- 3. Central (deterministic) 2022 annual measles estimate -------
measles_2022_results <- compute_importation_country_level(
  arrivals_df    = arrivals_2022,
  country_inc_df = measles_2022_country_inc,
  p_travel_inf   = p_travel_inf_measles_annual,  # annual run: p_d = D_d / 365.25
  title_text     = "Measles importation intensity -- annual 2022 (validation)"
)

# ---- 4. Monte Carlo uncertainty, same literature ranges as main ----
measles_2022_mc <- compute_mc_summary(
  measles_2022_results$importation$imp_intensity,
  measles_2022_results$importation$destination_city,
  under_rho_measles, p_travel_inf_measles_annual,
  0.40,  0.80,  p_min_measles_annual,  p_max_measles_annual,
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

measles_2022_by_state <- measles_2022_mc %>%
  left_join(city_to_state, by = "destination_city") %>%
  group_by(state) %>%
  summarise(
    lambda_median = sum(lambda_median),
    lambda_lo     = sum(lambda_lo),
    lambda_hi     = sum(lambda_hi),
    .groups = "drop"
  ) %>%
  arrange(desc(lambda_median))

# ---- 6. Compare against NNDSS 2022 "Measles Imported" by state -----
# All zero except Washington (1) -- confirmed genuine zeros, not
# suppressed data (see header note).
reported_measles_2022 <- tibble(
  state          = c("FL", "CA", "NY", "TX", "MA", "GA", "WA", "PA", "MO"),
  reported_cases = c(0,    0,    0,    0,    0,    0,    1,    0,    0)
)

validation_comparison_measles_2022 <- measles_2022_by_state %>%
  left_join(reported_measles_2022, by = "state") %>%
  mutate(implied_detection_fraction = reported_cases / lambda_median)

cat("\n=== MEASLES VALIDATION: full year 2022, by state ===\n")
cat("Lambda = model-estimated TRUE independent seeding events\n")
cat("(not reported cases; also excludes outbreak chains by design,\n")
cat("same scope as reported_cases here since these are IMPORTED\n")
cat("counts only, not indigenous/outbreak-chain cases)\n\n")
cat("With reported cases at or near zero everywhere, the useful\n")
cat("check here is whether Lambda is ALSO small and not wildly\n")
cat("implausible, not a precise detection-fraction estimate --\n")
cat("implied_detection_fraction of exactly 0 is expected and is\n")
cat("not itself evidence of a problem.\n\n")
print(validation_comparison_measles_2022, n = Inf)
