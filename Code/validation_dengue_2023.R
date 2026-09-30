# ============================================================
# Empirical validation: dengue, held-out period (June 2023)
# ============================================================
#
# PURPOSE
# -------
# Comment 3 from the senior collaborator: compare model estimates
# against reported travel-associated cases for a period NOT used to
# build the model, to check whether Lambda (true infected arrivals)
# is plausible against real surveillance data. This is genuine
# empirical validation against external data — distinct from, and
# not a substitute for, the Monte Carlo uncertainty propagation or
# the p_d sensitivity analysis already in the paper.
#
# WHY JUNE 2023, NOT JUNE 2025
# -----------------------------
# June 2025 arrivals were not used to build M1 (which uses June 2024
# COR arrivals), but June 2025 dengue INCIDENCE was already folded
# into the model's disease-incidence parameter, which averages
# "June 2024-2025" (see dengue_june_country in the main script).
# Using June 2025 here would therefore be partially circular.
# June 2023 uses neither for arrivals nor incidence anywhere in the
# model, so it is a clean held-out test. (Note: T-100 routing
# fractions do average over 2023-2025 flight patterns; this is a
# structural/geographic input, not the incidence or arrival-volume
# being validated, but it means the routing fractions themselves are
# not fully independent of the validation period. Worth stating as a
# limitation, not a disqualifier.)
#
# PREREQUISITE
# ------------
# Run importationRisk_main_with_uncertainty_rho_corrected.R first
# (through the point where arrivals_COR, t100_routing,
# population_of_world, dengue_data_world_selected, under_rho_dengue,
# p_travel_inf_dengue, compute_importation_country_level(), and
# compute_mc_summary() exist in the session). This script only adds
# a June-2023 comparison on top of those already-loaded objects.
#
# WHAT THIS DOES NOT DO
# ----------------------
# It does not apply a US-side detection-fraction correction — Lambda
# here is still "true infected arrivals," not "expected reported
# cases." That gap is deliberate: rather than assuming a detection
# fraction, the script reports both Lambda and the reported case
# count side by side, and computes the IMPLIED detection fraction
# (reported / Lambda) so you can judge whether it's a defensible
# number for dengue given what's known about US travel-medicine
# surveillance, rather than baking in an assumption.
# ============================================================

# ---- 1. June 2023 dengue incidence (mirrors dengue_june_country,
#         but a single year, not the 2024-2025 average) ----------
dengue_june_2023_country <- dengue_data_world_selected %>%
  filter(month(date) == 6, year(date) == 2023) %>%
  drop_na() %>%
  group_by(country) %>%
  summarise(june_2023_cases = mean(cases, na.rm = TRUE), .groups = "drop") %>%
  rename(Country = country) %>%
  mutate(Country = recode(Country,
    "Venezuela (Bolivarian Republic of)" = "Venezuela",
    "Bolivia (Plurinational State of)"   = "Bolivia",
    "Iran (Islamic Republic of)"         = "Iran",
    "United Republic of Tanzania"        = "Tanzania")) %>%
  left_join(population_of_world, by = "Country") %>%
  drop_na() %>%
  mutate(total_inc = june_2023_cases / (population_country * under_rho_dengue)) %>%
  select(Country, total_inc)

# ---- 2. June 2023 arrivals: COR June 2023 x T-100 routing --------
# Same structure as arrivals_baseline (Section 6c of the main
# script), just pointed at the 2023-06 column instead of 2024-06.
cor_june_2023 <- arrivals_COR %>%
  select(Country, World_region, `2023-06`) %>%
  mutate(june_2023 = readr::parse_number(as.character(`2023-06`))) %>%
  select(Country, World_region, june_2023) %>%
  drop_na()

arrivals_2023 <- cor_june_2023 %>%
  left_join(t100_routing, by = "Country") %>%
  drop_na(venue_city) %>%
  mutate(
    arrivals_june_2026 = june_2023 * routing_fraction,  # column name kept
    destination_city   = venue_city                      # for compatibility
  ) %>%
  select(Country, destination_city, arrivals_june_2026)

# ---- 3. Central (deterministic) June 2023 dengue estimate --------
dengue_2023_results <- compute_importation_country_level(
  arrivals_df    = arrivals_2023,
  country_inc_df = dengue_june_2023_country,
  p_travel_inf   = p_travel_inf_dengue,
  title_text     = "Dengue importation intensity -- June 2023 (held-out validation)"
)

# ---- 4. Monte Carlo uncertainty, same literature ranges as the
#         main analysis (Table 1: rho 0.06-0.26, p 0.30-0.70) ------
dengue_2023_mc <- compute_mc_summary(
  dengue_2023_results$importation$imp_intensity,
  dengue_2023_results$importation$destination_city,
  under_rho_dengue, p_travel_inf_dengue,
  0.06, 0.26, 0.30, 0.70,
  n_mc = 5000
)

# ---- 5. Aggregate to state level (CDC case data is state-level,
#         not city-level) ------------------------------------------
city_to_state <- tibble(
  destination_city = c("New York", "Miami", "Atlanta", "Los Angeles",
                        "Houston", "Dallas", "San Francisco", "Seattle",
                        "Boston", "Philadelphia", "Kansas City"),
  state = c("NY", "FL", "GA", "CA",
            "TX", "TX", "CA", "WA",
            "MA", "PA", "MO")
)

dengue_2023_by_state <- dengue_2023_mc %>%
  left_join(city_to_state, by = "destination_city") %>%
  group_by(state) %>%
  summarise(
    lambda_median = sum(lambda_median),
    lambda_lo     = sum(lambda_lo),
    lambda_hi     = sum(lambda_hi),
    .groups = "drop"
  ) %>%
  arrange(desc(lambda_median))

cat("\n=== DENGUE VALIDATION: June 2023 (held-out), by state ===\n")
cat("Lambda = model-estimated TRUE infected arrivals (not reported cases)\n\n")
print(dengue_2023_by_state, n = Inf)

cat("\n11-state total Lambda (median):", sum(dengue_2023_by_state$lambda_median), "\n")

# ---- 6. PLACEHOLDER: fill in actual reported dengue cases --------
# Source: CDC historic dengue data (state-level, travel-associated),
# https://www.cdc.gov/dengue/data-research/facts-stats/historic-data.html
# Fill in June 2023 travel-associated case counts by state below.
# If only annual (not monthly) state totals are available, note that
# explicitly rather than silently comparing a month to a year.
reported_dengue_june_2023 <- tibble(
  state = c("NY", "FL", "GA", "CA", "TX", "WA", "MA", "PA", "MO"),
  reported_cases = c(NA, NA, NA, NA, NA, NA, NA, NA, NA)  # <-- fill in
)

validation_comparison <- dengue_2023_by_state %>%
  left_join(reported_dengue_june_2023, by = "state") %>%
  mutate(
    implied_detection_fraction = reported_cases / lambda_median
  )

cat("\n=== COMPARISON (fill in reported_cases above first) ===\n")
cat("implied_detection_fraction = reported / Lambda_median\n")
cat("This is NOT assumed -- it's solved for, so you can judge whether\n")
cat("it's a defensible number for dengue, not whether cases match exactly.\n\n")
print(validation_comparison, n = Inf)
