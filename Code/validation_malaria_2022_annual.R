# ============================================================
# Model evaluation: malaria, full-year 2022 reconstruction
# ============================================================
#
# PURPOSE
# -------
# Compares an annual 2022 malaria Lambda with reported state-level
# cases. Reported in the Results text (not in Table 2): implied
# detection fractions exceeded 100% in four states, so the comparison
# was not used as an evaluation.
#
# WHY 2022
# --------
# The existing malaria incidence file (Data/Malaria_National_Unit_
# data.csv) only covers 2022-2024, and the headline model uses 2024
# specifically. 2022 is the earliest year available in that file and
# the most recent year with a published CDC state-level malaria
# table (NNDSS Table 2k). 2022 arrivals and 2022 incidence are both
# independent of the headline model's 2024-based construction.
#
# NOTE ON THE INCIDENCE METRIC
# ------------------------------
# Unlike dengue, WHO malaria incidence is only available as an
# ANNUAL rate (no monthly resolution), so this script sums COR
# arrivals across all 12 months of 2022 and applies the single 2022
# annual incidence rate directly -- it does NOT divide by 12 the way
# the headline model does (that /12 exists only to approximate a
# monthly rate for pairing with June-only arrivals; here we are
# matching a full year of arrivals to a full year of incidence, so
# no monthly approximation is needed). p_d uses T_d = 365.25 days
# (p_*_malaria_annual, main script Section 5g).
#
# 2022 COLUMN NAMING IN arrivals_COR
# -------------------------------------
# 2022 months are named "2022-1" through "2022-12" (no leading zero
# for Jan-Sep), unlike 2023/2024 ("2023-01" etc). Handled explicitly
# below -- do not generate these with sprintf("%02d").
#
# PREREQUISITE
# ------------
# Run importationRisk_main.R first
# (through the point where arrivals_COR, t100_routing,
# population_of_world, under_rho_malaria, p_travel_inf_malaria_annual,
# compute_importation_country_level(), and compute_mc_summary()
# exist in the session).
#
# REPORTED DATA SOURCE
# ----------------------
# NNDSS Table 2k, "Annual reported cases of notifiable diseases...
# 2022" (https://stacks.cdc.gov/view/cdc/175693). New York is
# reported as NYC (231) and rest-of-state (54) separately in this
# table; since our "New York" host city represents NYC specifically,
# 231 is used below rather than the 285 state total -- a more
# precise match than was possible for the dengue validation.
# ============================================================

# ---- 1. 2022 annual arrivals: sum all 12 months, then x T-100 ------
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

# ---- 2. 2022 annual malaria incidence (no /12 -- see header note) ---
malaria_2022_country_inc <- malaria_data_raw %>%
  filter(Year == 2022, Metric == "Incidence Rate") %>%
  select(Country = Name, cases_per1K = Value) %>%
  mutate(Country = recode(Country,
    "Democratic Republic of the Congo" = "Zaire (formerly DRC)")) %>%
  mutate(total_inc = cases_per1K / (1000 * under_rho_malaria)) %>%
  select(Country, total_inc)

# ---- 3. Central (deterministic) 2022 annual malaria estimate --------
malaria_2022_results <- compute_importation_country_level(
  arrivals_df    = arrivals_2022,
  country_inc_df = malaria_2022_country_inc,
  p_travel_inf   = p_travel_inf_malaria_annual,  # annual run: p_d = D_d / 365.25
  title_text     = "Malaria importation intensity -- annual 2022 (validation)"
)

# ---- 4. Monte Carlo uncertainty, same literature ranges as main ----
malaria_2022_mc <- compute_mc_summary(
  malaria_2022_results$importation$imp_intensity,
  malaria_2022_results$importation$destination_city,
  under_rho_malaria, p_travel_inf_malaria_annual,
  0.10,  0.35,  p_min_malaria_annual,  p_max_malaria_annual,
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

malaria_2022_by_state <- malaria_2022_mc %>%
  left_join(city_to_state, by = "destination_city") %>%
  group_by(state) %>%
  summarise(
    lambda_median = sum(lambda_median),
    lambda_lo     = sum(lambda_lo),
    lambda_hi     = sum(lambda_hi),
    .groups = "drop"
  ) %>%
  arrange(desc(lambda_median))

# ---- 6. Compare against NNDSS 2022 state totals ----------------------
# NY uses the NYC-specific figure (231), not the whole-state total
# (285 = 231 NYC + 54 rest-of-state) -- see header note.
reported_malaria_2022 <- tibble(
  state          = c("FL", "CA", "NY",  "TX",  "MA", "GA", "WA", "PA", "MO"),
  reported_cases = c(60,   115,  231,   166,   66,   69,   44,   95,   23)
)

validation_comparison_malaria_2022 <- malaria_2022_by_state %>%
  left_join(reported_malaria_2022, by = "state") %>%
  mutate(implied_detection_fraction = reported_cases / lambda_median)

cat("\n=== MALARIA VALIDATION: full year 2022, by state ===\n")
cat("Lambda = model-estimated TRUE infected arrivals (not reported cases)\n")
cat("reported_cases: NY is NYC-specific (231), not whole-state (285)\n")
cat("CAUTION: no 2023/2024 state-level malaria table was found this\n")
cat("session -- 2022 is the most recent available, and does not\n")
cat("overlap with the headline model's 2024 construction at all.\n\n")
print(validation_comparison_malaria_2022, n = Inf)
