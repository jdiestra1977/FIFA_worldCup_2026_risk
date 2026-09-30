# ============================================================
# Empirical validation: dengue, full-year 2024 reconstruction
# ============================================================
#
# PURPOSE
# -------
# Comment 3 from the senior collaborator: compare model estimates
# against reported travel-associated cases for the same period.
# This compares an independently-built ANNUAL 2024 estimate against
# MMWR-published 2024 state-level travel-associated dengue totals.
#
# WHY 2024, AND THE LIMITATION THIS INTRODUCES
# -----------------------------------------------
# This is NOT a clean held-out test like the June-2023 script.
# June 2024 arrivals were used to build M1, and June 2024 incidence
# contributed to the model's "June 2024-2025 average" disease
# parameter. Of the 12 months reconstructed here, 1 (June) overlaps
# with data already used elsewhere in the model; the other 11 do
# not. This was chosen deliberately over further delay: MMWR already
# publishes full 2024 state-level annual totals, so no new data
# sourcing is required, unlike a genuinely clean held-out year
# (e.g. 2023), for which state-level annual totals were not found
# via public CDC pages in this session. STATE THIS LIMITATION
# EXPLICITLY if this validation is used in the manuscript.
#
# PREREQUISITE
# ------------
# Run importationRisk_main_with_uncertainty_rho_corrected.R first
# (through the point where arrivals_COR, t100_routing,
# population_of_world, dengue_data_world_selected, under_rho_dengue,
# p_travel_inf_dengue, compute_importation_country_level(), and
# compute_mc_summary() exist in the session).
#
# REPORTED DATA SOURCE
# ---------------------
# MMWR "Increase in Travel-Associated and Locally Acquired Dengue
# Cases -- United States, 2024" (https://www.cdc.gov/mmwr/volumes/
# 75/wr/mm7518a1.htm), Table 2. Two figure sets appeared during
# sourcing (959/702/338/239/143 vs. 1044/720/338/241 for
# FL/CA/NY/TX/MA) -- this was NOT a data error, they are two
# different columns of the same table: 1,044 is Florida's COMBINED
# total (travel-associated + locally acquired), 959 is the
# travel-associated subset specifically. Since Lambda in this model
# represents infected ARRIVALS (travel-associated), the
# travel-associated column is the correct comparison and is what is
# used below. Nationally, 97.2% of 2024 cases were travel-associated;
# only FL (85 local), CA (18 local), and TX (2 local) had any
# locally-acquired cases at all among these 9 states.
# ============================================================

# ---- 1. Loop over all 12 months of 2024, summing city-level
#         Lambda across months (arrivals and incidence both vary
#         by month; routing fractions do not) -----------------------
months_2024 <- sprintf("2024-%02d", 1:12)

dengue_2024_monthly <- map_dfr(1:12, function(m) {

  col_name <- months_2024[m]

  # That month's country-level arrivals x T-100 routing
  arrivals_month <- arrivals_COR %>%
    select(Country, World_region, all_of(col_name)) %>%
    mutate(arrivals_raw = readr::parse_number(as.character(.data[[col_name]]))) %>%
    select(Country, World_region, arrivals_raw) %>%
    drop_na() %>%
    left_join(t100_routing, by = "Country") %>%
    drop_na(venue_city) %>%
    mutate(
      arrivals_june_2026 = arrivals_raw * routing_fraction,  # name kept for
      destination_city   = venue_city                         # function compat
    ) %>%
    select(Country, destination_city, arrivals_june_2026)

  # That month's dengue incidence
  inc_month <- dengue_data_world_selected %>%
    filter(month(date) == m, year(date) == 2024) %>%
    drop_na() %>%
    group_by(country) %>%
    summarise(month_cases = mean(cases, na.rm = TRUE), .groups = "drop") %>%
    rename(Country = country) %>%
    mutate(Country = recode(Country,
      "Venezuela (Bolivarian Republic of)" = "Venezuela",
      "Bolivia (Plurinational State of)"   = "Bolivia",
      "Iran (Islamic Republic of)"         = "Iran",
      "United Republic of Tanzania"        = "Tanzania")) %>%
    left_join(population_of_world, by = "Country") %>%
    drop_na() %>%
    mutate(total_inc = month_cases / (population_country * under_rho_dengue)) %>%
    select(Country, total_inc)

  if (nrow(inc_month) == 0) {
    return(tibble(destination_city = character(), imp_intensity = double(), month = integer()))
  }

  res <- compute_importation_country_level(
    arrivals_df    = arrivals_month,
    country_inc_df = inc_month,
    p_travel_inf   = p_travel_inf_dengue,
    title_text     = paste("Dengue --", col_name)
  )

  res$importation %>%
    select(destination_city, imp_intensity) %>%
    mutate(month = m)
})

# ---- 2. Sum across months to get the central annual Lambda,
#         per city ---------------------------------------------------
dengue_2024_annual_central <- dengue_2024_monthly %>%
  group_by(destination_city) %>%
  summarise(imp_intensity = sum(imp_intensity), .groups = "drop")

# ---- 3. Monte Carlo uncertainty on the annual total, same
#         literature ranges as the main analysis --------------------
dengue_2024_annual_mc <- compute_mc_summary(
  dengue_2024_annual_central$imp_intensity,
  dengue_2024_annual_central$destination_city,
  under_rho_dengue, p_travel_inf_dengue,
  0.06, 0.26, 0.30, 0.70,
  n_mc = 5000
)

# ---- 4. Aggregate to state level -----------------------------------
city_to_state <- tibble(
  destination_city = c("New York", "Miami", "Atlanta", "Los Angeles",
                        "Houston", "Dallas", "San Francisco", "Seattle",
                        "Boston", "Philadelphia", "Kansas City"),
  state = c("NY", "FL", "GA", "CA",
            "TX", "TX", "CA", "WA",
            "MA", "PA", "MO")
)

dengue_2024_by_state <- dengue_2024_annual_mc %>%
  left_join(city_to_state, by = "destination_city") %>%
  group_by(state) %>%
  summarise(
    lambda_median = sum(lambda_median),
    lambda_lo     = sum(lambda_lo),
    lambda_hi     = sum(lambda_hi),
    .groups = "drop"
  ) %>%
  arrange(desc(lambda_median))

# ---- 5. Compare against MMWR 2024 state totals -----------------------
# Source: MMWR mm7518a1, Table 2, travel-associated column (confirmed
# directly from the live table; see header note).
reported_dengue_2024 <- tibble(
  state          = c("FL", "CA", "NY", "TX", "MA", "GA", "WA", "PA", "MO"),
  reported_cases = c(959,  702,  338,  239,  143,  56,   67,   66,   12)
)

validation_comparison_2024 <- dengue_2024_by_state %>%
  left_join(reported_dengue_2024, by = "state") %>%
  mutate(implied_detection_fraction = reported_cases / lambda_median)

cat("\n=== DENGUE VALIDATION: full year 2024, by state ===\n")
cat("Lambda = model-estimated TRUE infected arrivals (not reported cases)\n")
cat("reported_cases = travel-associated only (MMWR Table 2)\n")
cat("CAUTION: June 2024 partially overlaps with model-construction data\n")
cat("(11 of 12 months are independent).\n\n")
print(validation_comparison_2024, n = Inf)
