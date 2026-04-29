# ============================================================
# FIFA World Cup 2026 - Importation Risk Pipeline
# Author: Jose Herrera-Diestra
# ============================================================

# ============================================================
# 0. Packages
# ============================================================
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(cowplot)

# ============================================================
# 1. Working directory
# ============================================================
setwd("~/Documents/GitHub/FIFA_worldCup_2026_risk/")

# ============================================================
# 2. Reference data
# ============================================================

population_of_world <- read_csv("Data/population2020.csv") %>%
  rename(Country = COUNTRY, population_country = POPULATION) %>%
  mutate(Country = if_else(Country == "DR Congo", "Zaire (formerly DRC)", Country))

arrivals_COR <- read_csv("Data/Monthly_Arrivals_Country_of_Residence_COR_1.csv")

# Country -> broad world region, with population attached.
# Mexico and Canada are kept as their own regions.
correspondence_country_region <- arrivals_COR %>%
  select(Country, region = World_region) %>%
  drop_na() %>%
  distinct() %>%
  mutate(
    Country = if_else(
      Country == "Zaire ( formerly Congo, Democratic Republic of)",
      "Zaire (formerly DRC)", Country),
    new_region = case_when(
      region %in% c("Western Europe", "Eastern Europe") ~ "Europe",
      TRUE ~ region
    )
  ) %>%
  left_join(population_of_world, by = "Country") %>%
  drop_na() %>%
  mutate(new_region = case_when(
    Country == "Mexico" ~ "Mexico",
    Country == "Canada" ~ "Canada",
    TRUE ~ new_region
  ))

population_by_region <- correspondence_country_region %>%
  select(new_region, population_country) %>%
  group_by(new_region) %>%
  summarise(popu_region = sum(population_country), .groups = "drop")

# ============================================================
# 3. Travel volume: I-92 air arrivals by city and origin region
# https://www.trade.gov/us-international-air-travel-statistics-i-92-data
# ============================================================
# File naming convention: data_<region>_to_<city>.xlsx
# all_arrivals = foreign_originating + foreign_returning + us_citizen_returning
# (excludes us_citizen_originating — those are departures, not arrivals)

files <- Sys.glob("Data/Selected_cities_and_origins/*.xlsx")

read_arrivals_file <- function(f) {
  name <- basename(f)
  region <- name %>%
    str_extract("data_(.*)_to_") %>%
    str_remove("^data_") %>%
    str_remove("_to_$") %>%
    str_replace_all("_", " ") %>%
    str_to_title()
  destination <- name %>%
    str_extract("to_.*\\.xlsx") %>%
    str_remove("^to_") %>%
    str_remove("\\.xlsx$") %>%
    str_replace_all("_", " ") %>%
    str_to_title()
  read_excel(f) %>%
    clean_names() %>%
    mutate(region_origin = region, destination_city = destination) %>%
    filter(str_detect(as.character(date_year), "^(19|20)\\d{2}$"))
}

data_all <- map_dfr(files, read_arrivals_file)

# Shading rectangles for May/June/July (used in time series plots)
shade_df <- data_all %>%
  mutate(year_month = as.Date(paste(date_year, month_number, 1, sep = "-"))) %>%
  distinct(year_month) %>%
  filter(month(year_month) %in% c(5, 6, 7)) %>%
  mutate(xmin = year_month, xmax = year_month + months(1))

definite_data_arrivals <- data_all %>%
  mutate(
    all_arrivals = foreign_originating + foreign_returning + u_s_citizen_returning,
    year_month   = as.Date(paste(date_year, month_number, 1, sep = "-"))
  ) %>%
  select(date_year, month_number, date_month, region_origin,
         destination_city, all_arrivals, year_month) %>%
  drop_na() %>%
  filter(!destination_city %in% c("Austin")) %>%
  filter(!region_origin   %in% c("Oceania", "World"))

# Attach USA-total arrivals per region (used as routing denominator)
usa_arrivals <- definite_data_arrivals %>%
  filter(destination_city == "Usa") %>%
  select(region_origin, year_month, all_arrivals) %>%
  rename(all_arrivals_usa = all_arrivals)

definite_data_arrivals <- definite_data_arrivals %>%
  left_join(usa_arrivals, by = c("region_origin", "year_month")) %>%
  filter(destination_city != "Usa")

# Time series plot
arrivals_time_plot <- definite_data_arrivals %>%
  ggplot(aes(x = year_month, y = all_arrivals,
             color = destination_city, group = destination_city)) +
  theme_bw() +
  geom_rect(data = shade_df, inherit.aes = FALSE,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    fill = "gray80", alpha = 0.5) +
  geom_line() +
  facet_wrap(~region_origin, scales = "free_y")

print(arrivals_time_plot)
ggsave(filename = "Figures/temporal_arrivals_from_regions.png",
       plot = arrivals_time_plot, height = 6, width = 10)

# Mean June arrivals (2023-2025): the key travel-volume input for the importation model
arrivals_only_june <- definite_data_arrivals %>%
  filter(month_number == 6, as.numeric(date_year) >= 2023) %>%
  group_by(region_origin, destination_city) %>%
  summarise(arrivals_June = mean(all_arrivals, na.rm = TRUE), .groups = "drop")

# Mean June US-total arrivals by region (2023-2025).
# Used in Section 8 to compute city routing fractions:
#   routing_fraction = city_arrivals / usa_total
mean_arrivals_all_usa_june <- definite_data_arrivals %>%
  filter(month_number == 6, as.numeric(date_year) >= 2023) %>%
  group_by(region_origin) %>%
  summarise(mean_all_usa_June = mean(all_arrivals_usa, na.rm = TRUE), .groups = "drop")

# ============================================================
# 4. Disease data
# ============================================================

# -- Dengue (monthly WHO data) -----------------------------------
dengue_data_world <- read_xlsx("Data/dengue-global-data-2025-12-10.xlsx")

dengue_data_world_selected <- dengue_data_world %>%
  select(date, date_lab, who_region_long, country, cases) %>%
  mutate(country = recode(country,
    "Venezuela (Bolivarian Republic of)" = "Venezuela",
    "Bolivia (Plurinational State of)"   = "Bolivia",
    "Iran (Islamic Republic of)"         = "Iran",
    "United Republic of Tanzania"        = "Tanzania")) %>%
  left_join(correspondence_country_region %>%
              select(country = Country, new_region), by = "country")

# Optional diagnostic: dengue seasonality by region
dengue_time_plot <- dengue_data_world_selected %>%
  select(date, new_region, cases) %>%
  mutate(cases = replace_na(cases, 0)) %>%
  drop_na() %>%
  group_by(date, new_region) %>%
  summarise(total_cases = sum(cases), .groups = "drop") %>%
  filter(date > as.Date("2018-12-31"),
         !new_region %in% c("Canada", "Europe")) %>%
  ggplot(aes(x = date, y = total_cases)) +
  geom_col() + theme_bw() +
  geom_rect(data = shade_df, inherit.aes = FALSE,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    fill = "gray80", alpha = 0.5) +
  facet_wrap(~new_region, scales = "free_y")

print(dengue_time_plot)

# Mean regional dengue burden in June (2024-2025)
mean_dengue_cases_regions_june <- dengue_data_world_selected %>%
  filter(month(date) == 6, year(date) > 2023) %>%
  drop_na() %>%
  select(date, new_region, cases) %>%
  group_by(date, new_region) %>%
  summarise(cases_region = sum(cases), .groups = "drop") %>%
  group_by(new_region) %>%
  summarise(mean_cases_june = mean(cases_region, na.rm = TRUE), .groups = "drop") %>%
  mutate(new_region = if_else(new_region == "Middle East", "Mideast", new_region))

# -- Malaria (annual incidence per 1,000, 2024) ------------------
malaria_data_raw <- read_csv("Data/Malaria_National_Unit_data.csv")

malaria_cases <- malaria_data_raw %>%
  filter(Year == 2024, Metric == "Incidence Rate") %>%
  select(Country = Name, cases_per1K = Value)

# -- Measles (annual incidence per 1,000,000) --------------------
measles_data <- read_xlsx("Data/Measles reported cases and incidence 2025-09-12 14-18 UTC.xlsx")

measles_incidence <- measles_data %>%
  select(Country = 1, incidence_per1M = 4) %>%
  drop_na() %>%
  mutate(incidence_per1M = as.numeric(incidence_per1M))

# -- Pertussis (annual incidence per 1,000,000) ------------------
pertussis_data <- read_xlsx("Data/Pertussis reported cases and incidence 2025-22-12 14-46 UTC.xlsx")

pertussis_incidence <- pertussis_data %>%
  select(Country = 1, incidence_per1M = 4) %>%
  drop_na() %>%
  mutate(incidence_per1M = as.numeric(incidence_per1M))

# ============================================================
# 5. Model functions
# ============================================================

# Bar chart of importation intensity by destination city
plot_importation <- function(df, title_text) {
  ggplot(df, aes(x = reorder(destination_city, -imp_intensity), y = imp_intensity)) +
    geom_col(fill = "steelblue") +
    geom_text(aes(label = round(prob_at_least_one, 2)), vjust = -0.5, size = 5) +
    labs(x = "", y = "Importation intensity", title = title_text) +
    theme_bw() + theme(text = element_text(size = 20))
}

# Core importation model (Poisson):
#
#   lambda[r -> h] = arrivals_June[r,h] * total_inc[r] * p_travel_inf
#   P(>=1 importation) = 1 - exp(-sum_r lambda[r -> h])
#
# region_incidence_df must have columns: region_origin, total_inc
#   where total_inc = under_rho * (disease burden / regional population)
compute_importation_from_region_incidence <- function(arrivals_df,
                                                      region_incidence_df,
                                                      p_travel_inf = 1,
                                                      title_text   = "Estimated importation intensity") {
  expected_imports <- region_incidence_df %>%
    left_join(arrivals_df, by = "region_origin") %>%
    drop_na() %>%
    mutate(expected_c_to_h = arrivals_June * total_inc * p_travel_inf)

  importation_df <- expected_imports %>%
    group_by(destination_city) %>%
    summarise(imp_intensity = sum(expected_c_to_h, na.rm = TRUE), .groups = "drop") %>%
    mutate(prob_at_least_one = 1 - exp(-imp_intensity))

  list(
    expected_imports = expected_imports,
    importation      = importation_df,
    plot             = plot_importation(importation_df, title_text)
  )
}

# ============================================================
# 6. Importation estimates
# ============================================================

# -- Dengue ------------------------------------------------------
under_rho_dengue    <- 0.3
p_travel_inf_dengue <- 0.5

dengue_region_incidence <- mean_dengue_cases_regions_june %>%
  rename(region_origin = new_region) %>%
  left_join(
    population_by_region %>%
      mutate(new_region = if_else(new_region == "Middle East", "Mideast", new_region)) %>%
      rename(region_origin = new_region),
    by = "region_origin"
  ) %>%
  mutate(total_inc = under_rho_dengue * mean_cases_june / popu_region) %>%
  select(region_origin, total_inc)

dengue_results <- compute_importation_from_region_incidence(
  arrivals_df         = arrivals_only_june,
  region_incidence_df = dengue_region_incidence,
  p_travel_inf        = p_travel_inf_dengue,
  title_text          = "Estimated dengue importation intensity by destination city"
)

print(dengue_results$importation)
print(dengue_results$plot)

# -- Malaria -----------------------------------------------------
under_rho_malaria    <- 0.2
p_travel_inf_malaria <- 0.3

malaria_region_incidence <- malaria_cases %>%
  mutate(Country = recode(Country,
    "Democratic Republic of the Congo" = "Zaire (formerly DRC)")) %>%
  left_join(correspondence_country_region %>%
              select(Country, region_origin = new_region), by = "Country") %>%
  drop_na() %>%
  mutate(Incidence = under_rho_malaria * cases_per1K / (12 * 1000)) %>%
  group_by(region_origin) %>%
  summarise(total_inc = sum(Incidence, na.rm = TRUE), .groups = "drop")

malaria_results <- compute_importation_from_region_incidence(
  arrivals_df         = arrivals_only_june,
  region_incidence_df = malaria_region_incidence,
  p_travel_inf        = p_travel_inf_malaria,
  title_text          = "Estimated malaria importation intensity by destination city"
)

print(malaria_results$importation)
print(malaria_results$plot)

# -- Measles -----------------------------------------------------
under_rho_measles    <- 0.6
p_travel_inf_measles <- 0.05

measles_region_incidence <- measles_incidence %>%
  mutate(Country = recode(Country,
    "Democratic Republic of the Congo" = "Zaire (formerly DRC)")) %>%
  left_join(correspondence_country_region %>%
              select(Country, region_origin = new_region), by = "Country") %>%
  drop_na() %>%
  mutate(Incidence = under_rho_measles * incidence_per1M / (12 * 1000000)) %>%
  group_by(region_origin) %>%
  summarise(total_inc = sum(Incidence, na.rm = TRUE), .groups = "drop")

measles_results <- compute_importation_from_region_incidence(
  arrivals_df         = arrivals_only_june,
  region_incidence_df = measles_region_incidence,
  p_travel_inf        = p_travel_inf_measles,
  title_text          = "Estimated measles importation intensity by destination city"
)

print(measles_results$importation)
print(measles_results$plot)

# -- Pertussis ---------------------------------------------------
under_rho_pertussis    <- 0.3
p_travel_inf_pertussis <- 0.7

pertussis_region_incidence <- pertussis_incidence %>%
  mutate(Country = recode(Country,
    "Democratic Republic of the Congo" = "Zaire (formerly DRC)")) %>%
  left_join(correspondence_country_region %>%
              select(Country, region_origin = new_region), by = "Country") %>%
  drop_na() %>%
  mutate(Incidence = under_rho_pertussis * incidence_per1M / (12 * 1000000)) %>%
  group_by(region_origin) %>%
  summarise(total_inc = sum(Incidence, na.rm = TRUE), .groups = "drop")

pertussis_results <- compute_importation_from_region_incidence(
  arrivals_df         = arrivals_only_june,
  region_incidence_df = pertussis_region_incidence,
  p_travel_inf        = p_travel_inf_pertussis,
  title_text          = "Estimated pertussis importation intensity by destination city"
)

print(pertussis_results$importation)
print(pertussis_results$plot)

# ============================================================
# 7. Combined output
# ============================================================

panel_results_plots <- plot_grid(
  dengue_results$plot,
  malaria_results$plot,
  measles_results$plot,
  pertussis_results$plot
)

ggsave(panel_results_plots, file = "Figures/estimated_importations.png", height = 13, width = 20)

# ============================================================
# 8. WC-ADJUSTED IMPORTATION MODEL
# ============================================================
#
# MOTIVATION
# ----------
# Sections 3-7 use I-92 regional air arrivals averaged over 2023-2025.
# That captures normal travel patterns but ignores the World Cup surge.
# This section builds a second, parallel model that upgrades the travel
# volume to country-level 2026 projections.
#
# TWO-SOURCE TRAVEL VOLUME STRATEGY
# ----------------------------------
# (a) NTTO top-12 source markets (Data/ntto_forecast_2026.csv):
#     Official 2026 projections from the National Travel and Tourism
#     Office (2025-Forecast-Tables.pdf, trade.gov). These figures already
#     incorporate the WC effect — do NOT add WC visitors on top.
#
# (b) COR I-94 monthly data scaled by growth factors for all other countries:
#     - WC-qualified countries: scaled by the overall 2024→2026 growth
#       factor derived from NTTO totals (85,017 / 72,390 = 1.174).
#       This includes the WC tourism boost.
#     - Non-qualified countries: scaled by the estimated baseline growth
#       without WC (~6.5%/yr for 2 years ≈ 1.134).
#
# CITY ROUTING
# ------------
# We don't have country-level city routing. Instead we reuse the I-92
# routing fractions already estimated in section 3:
#   routing_fraction[region, city] = arrivals_June[region,city] / arrivals_June[region,USA]
# Each country inherits its region's routing fraction.
#
# DISEASE BURDEN
# --------------
# Upgraded from region-level to country-level incidence, which is more
# precise. The same under_rho and p_travel_inf parameters from section 6
# are reused to keep the two models comparable.
# ============================================================

# ============================================================
# 8a. Load supporting data
# ============================================================

# NTTO 2026 projections: top 12 source markets (in thousands of visitors)
# growth_factor_2024_2026 = visitors_2026 / visitors_2024
ntto_2026 <- read_csv("Data/ntto_forecast_2026.csv", show_col_types = FALSE) %>%
  # country_ntto names already match COR conventions ("South Korea", "United Kingdom", etc.)
  # Note: "United Kingdom" covers England + Scotland (both WC-qualified) as a combined entry
  rename(country_cor = country_ntto)

# WC 2026 qualified teams: 48 countries + confederation + host flag
wc_teams <- read_csv("Data/wc2026_qualified_teams.csv", show_col_types = FALSE)

# ============================================================
# 8b. Build country-level June 2026 travel volume
# ============================================================

# Overall NTTO growth factors (2024 → 2026):
#   Total international: 85,017 / 72,390 = 1.174 (WC-boosted)
#   Baseline (no WC):    estimated ~1.134 (two years at ~6.5%/yr)
growth_wc_total  <- 85017 / 72390  # 1.174 — includes WC tourism uplift
growth_baseline  <- 1.134          # counterfactual without WC

# Extract June 2024 from COR monthly data (most recent complete summer)
cor_june_2024 <- arrivals_COR %>%
  select(Country, World_region, `2024-06`) %>%
  mutate(june_2024 = readr::parse_number(as.character(`2024-06`))) %>%
  select(Country, World_region, june_2024) %>%
  drop_na()

# Assign a growth factor to every country:
#   Priority 1 — NTTO country-specific factor (most accurate)
#   Priority 2 — WC-boosted global factor  (qualified, not in NTTO)
#   Priority 3 — Baseline global factor    (not qualified, not in NTTO)
travel_volume_june_2026 <- cor_june_2024 %>%
  left_join(
    ntto_2026 %>% select(country_cor, growth_factor_2024_2026),
    by = c("Country" = "country_cor")
  ) %>%
  left_join(
    wc_teams %>% select(country, host),
    by = c("Country" = "country")
  ) %>%
  mutate(
    growth_factor = case_when(
      !is.na(growth_factor_2024_2026) ~ growth_factor_2024_2026, # NTTO estimate
      !is.na(host)                    ~ growth_wc_total,          # WC team, not in NTTO
      TRUE                            ~ growth_baseline            # no WC team, not in NTTO
    ),
    june_2026 = june_2024 * growth_factor
  ) %>%
  select(Country, World_region, june_2024, growth_factor, june_2026)

# ============================================================
# 8c. City routing fractions from I-92 (reused from section 3)
# ============================================================
# routing_fraction[region, city] answers:
#   "Of all arrivals to the US from region r, what share land in city h?"
# We already have arrivals_only_june and mean_arrivals_all_usa_june from section 3.

routing_fractions <- arrivals_only_june %>%
  left_join(mean_arrivals_all_usa_june, by = "region_origin") %>%
  mutate(routing_fraction = arrivals_June / mean_all_usa_June) %>%
  select(region_origin, destination_city, routing_fraction)

# ============================================================
# 8d. Country × city arrivals matrix
# ============================================================
# Combine country-level 2026 volume with city routing:
#   arrivals[country, city] = june_2026[country] × routing_fraction[region(country), city]

arrivals_country_city_2026 <- travel_volume_june_2026 %>%
  left_join(
    correspondence_country_region %>% select(Country, region_origin = new_region),
    by = "Country"
  ) %>%
  drop_na(region_origin) %>%
  left_join(routing_fractions, by = "region_origin") %>%
  drop_na(destination_city) %>%
  mutate(arrivals_june_2026 = june_2026 * routing_fraction) %>%
  select(Country, region_origin, destination_city, arrivals_june_2026)

# ============================================================
# 8e. Country-level disease incidence tables
# ============================================================
# For each disease, compute total_inc[country]:
#   total_inc = under_rho * (cases / population_denominator)
# This is the per-traveller infection probability used in the Poisson model.
# The same under_rho values from section 6 are used so results are comparable.

# -- Dengue: mean June cases (2024-2025) from WHO monthly data
#    Dengue data is in raw cases, so we need to divide by population
dengue_june_country <- dengue_data_world_selected %>%
  filter(month(date) == 6, year(date) > 2023) %>%
  drop_na() %>%
  group_by(country) %>%
  summarise(mean_june_cases = mean(cases, na.rm = TRUE), .groups = "drop") %>%
  rename(Country = country) %>%
  mutate(Country = recode(Country,
    "Venezuela (Bolivarian Republic of)" = "Venezuela",
    "Bolivia (Plurinational State of)"   = "Bolivia",
    "Iran (Islamic Republic of)"         = "Iran",
    "United Republic of Tanzania"        = "Tanzania")) %>%
  left_join(population_of_world, by = "Country") %>%
  drop_na() %>%
  # total_inc = under_rho * (cases / population)
  mutate(total_inc = under_rho_dengue * mean_june_cases / population_country) %>%
  select(Country, total_inc)

# -- Malaria: annual incidence per 1,000; divide by 12 for monthly rate
malaria_country_inc <- malaria_cases %>%
  mutate(Country = recode(Country,
    "Democratic Republic of the Congo" = "Zaire (formerly DRC)")) %>%
  mutate(total_inc = under_rho_malaria * cases_per1K / (12 * 1000)) %>%
  select(Country, total_inc)

# -- Measles: annual incidence per 1,000,000; divide by 12 for monthly rate
measles_country_inc <- measles_incidence %>%
  mutate(Country = recode(Country,
    "Democratic Republic of the Congo" = "Zaire (formerly DRC)")) %>%
  mutate(total_inc = under_rho_measles * incidence_per1M / (12 * 1e6)) %>%
  select(Country, total_inc)

# -- Pertussis: annual incidence per 1,000,000; divide by 12 for monthly rate
pertussis_country_inc <- pertussis_incidence %>%
  mutate(Country = recode(Country,
    "Democratic Republic of the Congo" = "Zaire (formerly DRC)")) %>%
  mutate(total_inc = under_rho_pertussis * incidence_per1M / (12 * 1e6)) %>%
  select(Country, total_inc)

# ============================================================
# 8f. WC-adjusted model function
# ============================================================
# Same Poisson logic as compute_importation_from_region_incidence() in section 5,
# but now operating at country level:
#
#   lambda[c, h] = arrivals_june_2026[c,h] * total_inc[c] * p_travel_inf
#   P(>=1 importation into city h) = 1 - exp( -sum_c lambda[c,h] )
#
# country_inc_df must have columns: Country, total_inc

compute_importation_country_level <- function(arrivals_df,
                                              country_inc_df,
                                              p_travel_inf = 1,
                                              title_text   = "Estimated importation intensity (WC-adjusted)") {
  importation_df <- arrivals_df %>%
    left_join(country_inc_df, by = "Country") %>%
    drop_na(total_inc) %>%
    mutate(expected_c_to_h = arrivals_june_2026 * total_inc * p_travel_inf) %>%
    group_by(destination_city) %>%
    summarise(imp_intensity = sum(expected_c_to_h, na.rm = TRUE), .groups = "drop") %>%
    mutate(prob_at_least_one = 1 - exp(-imp_intensity))

  list(
    importation = importation_df,
    plot        = plot_importation(importation_df, title_text)
  )
}

# ============================================================
# 8g. WC-adjusted importation estimates
# ============================================================
# Parameters are kept identical to section 6 to allow direct comparison
# between the baseline model (sections 3-7) and this WC-adjusted model.

# -- Dengue
dengue_wc_results <- compute_importation_country_level(
  arrivals_df    = arrivals_country_city_2026,
  country_inc_df = dengue_june_country,
  p_travel_inf   = p_travel_inf_dengue,
  title_text     = "Dengue importation intensity — WC-adjusted (June 2026)"
)

print(dengue_wc_results$importation)
print(dengue_wc_results$plot)

# -- Malaria
malaria_wc_results <- compute_importation_country_level(
  arrivals_df    = arrivals_country_city_2026,
  country_inc_df = malaria_country_inc,
  p_travel_inf   = p_travel_inf_malaria,
  title_text     = "Malaria importation intensity — WC-adjusted (June 2026)"
)

print(malaria_wc_results$importation)
print(malaria_wc_results$plot)

# -- Measles
measles_wc_results <- compute_importation_country_level(
  arrivals_df    = arrivals_country_city_2026,
  country_inc_df = measles_country_inc,
  p_travel_inf   = p_travel_inf_measles,
  title_text     = "Measles importation intensity — WC-adjusted (June 2026)"
)

print(measles_wc_results$importation)
print(measles_wc_results$plot)

# -- Pertussis
pertussis_wc_results <- compute_importation_country_level(
  arrivals_df    = arrivals_country_city_2026,
  country_inc_df = pertussis_country_inc,
  p_travel_inf   = p_travel_inf_pertussis,
  title_text     = "Pertussis importation intensity — WC-adjusted (June 2026)"
)

print(pertussis_wc_results$importation)
print(pertussis_wc_results$plot)

# ============================================================
# 8h. Combined WC-adjusted panel
# ============================================================

panel_wc_adjusted <- plot_grid(
  dengue_wc_results$plot,
  malaria_wc_results$plot,
  measles_wc_results$plot,
  pertussis_wc_results$plot,
  labels = "AUTO", label_size = 20
)

ggsave(panel_wc_adjusted,
       file   = "Figures/estimated_importations_wc_adjusted.png",
       height = 13, width = 20)

# ============================================================
# 9. SCHEDULE-DRIVEN VENUE ROUTING
# ============================================================
#
# MOTIVATION
# ----------
# Sections 6–8 allocate all international arrivals to US gateway cities
# using I-92 routing fractions — which capture where international
# travelers normally land. But World Cup fans travel to the specific
# cities where their national team plays, a fundamentally different
# spatial logic. A supporter from Brazil attending a group-stage match
# in Houston is unlikely to distribute themselves to Boston or
# Philadelphia in the same proportion as a regular tourist.
#
# This section introduces a schedule-based routing model that splits
# June 2026 travel into two streams:
#
#   (1) WC-FAN STREAM
#       The marginal arrivals attributable to the World Cup — the
#       increment above what would have come without the tournament.
#       These fans are routed to venue cities in proportion to the
#       number of matches their national team plays there.
#
#   (2) BACKGROUND STREAM
#       The arrivals that would have occurred regardless of the WC
#       (scaled by the baseline growth factor phi_base = 1.134).
#       These use the established I-92 routing fractions from Section 8c.
#
# DECOMPOSITION FORMULA
# ---------------------
# For each source country c (using travel_volume_june_2026 from Section 8b):
#
#   june_wc[c] = june_2024[c] * max(0, phi[c] - phi_base)
#             [= WC-specific fan arrivals; can be 0 when phi[c] <= phi_base]
#
#   june_bg[c] = june_2026[c] - june_wc[c]
#             [= background arrivals; always positive]
#
# Example: For a WC-qualified country with phi = 1.174 and phi_base = 1.134:
#   june_wc = june_2024 * (1.174 - 1.134) = june_2024 * 0.040
#   june_bg = june_2026 - june_wc           [= june_2024 * 1.134]
#
# For NTTO countries where phi < phi_base (e.g., UK at 1.107):
#   june_wc = 0 (no WC increment — all travel treated as background)
#
# WC-FAN ROUTING FORMULA
# ----------------------
#   wc_routing[c, v] = n_games_in_v[c] / total_games[c]
#   arrivals_wc[c, v] = june_wc[c] * wc_routing[c, v]
#
# Note on multi-team countries (England + Scotland → United Kingdom):
#   Both are combined under "United Kingdom" before computing routing
#   fractions. The UK's WC fans are apportioned across all venues
#   where either team plays. Because the fractions sum to 1, there is
#   no double-counting of UK arrivals.
#
# IMPORTATION MODEL (applied to each stream separately, then combined)
# --------------------------------------------------------------------
#   lambda_wc[c, v] = arrivals_wc[c, v] * I_{c,d} * p_d
#   lambda_bg[c, h] = arrivals_bg[c, h] * I_{c,d} * p_d
#
#   Lambda[city] = sum_c lambda_wc[c, city] + sum_c lambda_bg[c, city]
#   P(>=1 | city) = 1 - exp(-Lambda[city])
#
# For cities that appear in both the WC venue list and the I-92 set
# (Boston, Dallas, Houston, New York, Philadelphia), both streams
# contribute to the total Lambda.
# ============================================================

# ============================================================
# 9a. Parse match schedule
# ============================================================
# The template has one row per match. We pivot to one row per team per
# match because both teams in a match generate fan travel to that venue.
# "TBD" entries (unresolved knockout-stage opponents) are dropped.

games_schedule <- read_excel("Data/WorldCup2026_games_template.xlsx") %>%
  clean_names() %>%
  pivot_longer(
    cols      = c(team_1, team_2),
    names_to  = "slot",
    values_to = "team"
  ) %>%
  filter(!is.na(team), team != "TBD") %>%
  select(team, match_date, stadium, city) %>%
  # --- Recode FIFA team names → COR country naming conventions ---
  # Maps schedule team names to the exact strings used in the COR
  # arrivals data and disease incidence tables.
  mutate(team = recode(team,
    "Korea Republic"  = "South Korea",
    "IR Iran"         = "Iran",
    "Cabo Verde"      = "Cape Verde",
    "Cote d'Ivore"    = "Côte d'Ivoire",   # schedule typo (Ivore → Ivoire)
    "Cote d'Ivoire"   = "Côte d'Ivoire",
    "Ivory Coast"     = "Côte d'Ivoire",
    "Belguim"         = "Belgium",          # schedule typo (u ↔ i)
    # England and Scotland both map to "United Kingdom" — the single COR
    # entry for UK residents. Their combined WC games are used to allocate
    # UK WC fans across all venues where either team plays (fractions sum
    # to 1, so there is no double-counting of UK arrivals).
    "England"         = "United Kingdom",
    "Scotland"        = "United Kingdom",
    "USA"             = "United States"     # US is host; likely absent from COR
                                            # foreign-arrival records — join will
                                            # produce no match (intentional)
  )) %>%
  # --- Standardise venue city names ---
  # Maps stadium host cities to the canonical venue-city names used in
  # Sections 9–11. Note:
  #   East Rutherford → New York  (MetLife Stadium, NYC metro)
  #   Foxborough      → Boston    (Gillette Stadium)
  #   Arlington       → Dallas    (AT&T Stadium)
  #   Inglewood       → Los Angeles (SoFi Stadium)
  #   Santa Clara     → San Francisco (Levi's Stadium, SF Bay Area)
  #   Miami Gardens   → Miami     (Hard Rock Stadium)
  #   Zapopan         → Guadalajara (Estadio Akron, GDL metro area)
  #   Guadalupe       → Monterrey (Estadio BBVA, MTY metro area)
  #   Cuidad de Mexico → Mexico City (Estadio Banorte)
  mutate(venue_city = recode(city,
    "East Rutherford"  = "New York",
    "Foxborough"       = "Boston",
    "Arlington"        = "Dallas",
    "Inglewood"        = "Los Angeles",
    "Santa Clara"      = "San Francisco",
    "Miami Gardens"    = "Miami",
    "Zapopan"          = "Guadalajara",
    "Guadalupe"        = "Monterrey",
    "Cuidad de Mexico" = "Mexico City"
    # Atlanta, Houston, Kansas City, Philadelphia, Seattle,
    # Toronto, Vancouver: kept as-is
  ))

# Count matches per team × venue city
team_venue_games <- games_schedule %>%
  group_by(team, venue_city) %>%
  summarise(n_games = n(), .groups = "drop")

# Total games per team (denominator for routing fraction)
total_games_per_team <- team_venue_games %>%
  group_by(team) %>%
  summarise(total_games = sum(n_games), .groups = "drop")

# Schedule-based WC fan routing fraction:
#   wc_routing[team, venue] = n_games_in_venue / total_games
# Interpretation: fraction of a team's WC matches played at each venue.
# We assume this equals the fraction of WC fans traveling to that city.
schedule_routing <- team_venue_games %>%
  left_join(total_games_per_team, by = "team") %>%
  mutate(wc_routing = n_games / total_games) %>%
  select(team, venue_city, wc_routing)

# ============================================================
# 9b. Decompose June 2026 travel into WC-fan vs. background streams
# ============================================================
# travel_volume_june_2026 was built in Section 8b.
# Columns used: Country, june_2024, growth_factor, june_2026
#
# WC-fan increment:
#   june_wc[c] = june_2024[c] * max(0, growth_factor[c] - growth_baseline)
#
# Background:
#   june_bg[c] = june_2026[c] - june_wc[c]  (always >= 0)

travel_decomposed <- travel_volume_june_2026 %>%
  mutate(
    june_wc = june_2024 * pmax(0, growth_factor - growth_baseline),
    june_bg = june_2026 - june_wc
  ) %>%
  select(Country, World_region, june_wc, june_bg, june_2026)

# ============================================================
# 9c. WC-fan stream: country × venue-city arrival matrix
# ============================================================
# Match each WC-qualified country to its team's match venues.
# Non-qualified countries have no entry in schedule_routing, so the
# left_join naturally produces NA which is then filtered out.

arrivals_wc_venue <- travel_decomposed %>%
  left_join(
    schedule_routing %>% rename(Country = team),
    by = "Country"
  ) %>%
  filter(!is.na(venue_city)) %>%
  mutate(arrivals_wc_city = june_wc * wc_routing) %>%
  select(Country, venue_city, arrivals_wc_city)

# ============================================================
# 9d. Background stream: country × I-92-city arrival matrix
# ============================================================
# Background travelers (june_bg) use I-92 routing fractions from
# Section 8c. These apply to ALL countries (qualified and non-qualified).
# I-92 city names are then remapped to the canonical venue-city names
# so that both streams share the same city-name convention for combining.
#
# Remapping: Newark → New York, Ny → New York
# (MetLife Stadium is in East Rutherford, NJ — served by Newark and
#  John F. Kennedy/LaGuardia airports — both are now labelled New York.)

arrivals_bg_city <- travel_decomposed %>%
  left_join(
    correspondence_country_region %>% select(Country, region_origin = new_region),
    by = "Country"
  ) %>%
  drop_na(region_origin) %>%
  left_join(routing_fractions, by = "region_origin") %>%
  drop_na(destination_city) %>%
  mutate(arrivals_bg = june_bg * routing_fraction) %>%
  select(Country, destination_city, arrivals_bg) %>%
  # Recode I-92 city names → canonical venue-city names
  mutate(venue_city = case_when(
    destination_city == "Newark"       ~ "New York",
    destination_city == "Ny"           ~ "New York",
    destination_city == "Boston"       ~ "Boston",
    destination_city == "Dallas"       ~ "Dallas",
    destination_city == "Houston"      ~ "Houston",
    destination_city == "Philadelphia" ~ "Philadelphia",
    TRUE                               ~ destination_city
  )) %>%
  select(Country, venue_city, arrivals_bg)

# I-92 → venue-city recode table (also used in Section 10 for comparison)
i92_to_venue_city <- c(
  "Newark"       = "New York",
  "Ny"           = "New York",
  "Boston"       = "Boston",
  "Dallas"       = "Dallas",
  "Houston"      = "Houston",
  "Philadelphia" = "Philadelphia"
)

# ============================================================
# 9e. Schedule-driven importation model function
# ============================================================
# Applies the Poisson importation framework (Section 5) to the two
# travel streams. Also returns per-country contributions needed by
# the ranking analysis in Section 11.
#
# Inputs:
#   arrivals_wc_df  — Country, venue_city, arrivals_wc_city
#   arrivals_bg_df  — Country, venue_city, arrivals_bg
#   country_inc_df  — Country, total_inc
#   p_travel_inf    — scalar: travel-while-infectious probability
#
# Output (list):
#   $importation           — city-level Lambda and P(>=1)
#   $country_contributions — per-country, per-city expected imports
#   $plot                  — bar chart of importation intensity

compute_importation_schedule <- function(arrivals_wc_df,
                                         arrivals_bg_df,
                                         country_inc_df,
                                         p_travel_inf = 1,
                                         title_text   = "Importation intensity (schedule-driven)") {

  # -- WC fan stream: lambda_wc[c, venue] = arrivals_wc * I_c * p_d
  wc_stream <- arrivals_wc_df %>%
    left_join(country_inc_df, by = "Country") %>%
    drop_na(total_inc) %>%
    mutate(
      expected_imports = arrivals_wc_city * total_inc * p_travel_inf,
      stream           = "WC fans"
    ) %>%
    rename(city = venue_city) %>%
    select(Country, city, expected_imports, stream)

  # -- Background stream: lambda_bg[c, city] = arrivals_bg * I_c * p_d
  bg_stream <- arrivals_bg_df %>%
    left_join(country_inc_df, by = "Country") %>%
    drop_na(total_inc) %>%
    mutate(
      expected_imports = arrivals_bg * total_inc * p_travel_inf,
      stream           = "Background"
    ) %>%
    rename(city = venue_city) %>%
    select(Country, city, expected_imports, stream)

  # -- Combine both streams; aggregate by city
  combined <- bind_rows(wc_stream, bg_stream)

  importation_df <- combined %>%
    group_by(city) %>%
    summarise(imp_intensity = sum(expected_imports, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      prob_at_least_one = 1 - exp(-imp_intensity),
      destination_city  = city   # required by plot_importation()
    )

  # Per-country contributions (used by Section 11 ranking and heatmaps)
  country_contributions <- combined %>%
    group_by(Country, city, stream) %>%
    summarise(expected_imports = sum(expected_imports, na.rm = TRUE), .groups = "drop")

  list(
    importation           = importation_df,
    country_contributions = country_contributions,
    plot                  = plot_importation(importation_df, title_text)
  )
}

# ============================================================
# 9f. Schedule-driven importation estimates for all four diseases
# ============================================================
# Parameters are identical to Sections 6 and 8 for direct comparability.

dengue_sched_results <- compute_importation_schedule(
  arrivals_wc_df  = arrivals_wc_venue,
  arrivals_bg_df  = arrivals_bg_city,
  country_inc_df  = dengue_june_country,
  p_travel_inf    = p_travel_inf_dengue,
  title_text      = "Dengue importation intensity — schedule-driven (June 2026)"
)

malaria_sched_results <- compute_importation_schedule(
  arrivals_wc_df  = arrivals_wc_venue,
  arrivals_bg_df  = arrivals_bg_city,
  country_inc_df  = malaria_country_inc,
  p_travel_inf    = p_travel_inf_malaria,
  title_text      = "Malaria importation intensity — schedule-driven (June 2026)"
)

measles_sched_results <- compute_importation_schedule(
  arrivals_wc_df  = arrivals_wc_venue,
  arrivals_bg_df  = arrivals_bg_city,
  country_inc_df  = measles_country_inc,
  p_travel_inf    = p_travel_inf_measles,
  title_text      = "Measles importation intensity — schedule-driven (June 2026)"
)

pertussis_sched_results <- compute_importation_schedule(
  arrivals_wc_df  = arrivals_wc_venue,
  arrivals_bg_df  = arrivals_bg_city,
  country_inc_df  = pertussis_country_inc,
  p_travel_inf    = p_travel_inf_pertussis,
  title_text      = "Pertussis importation intensity — schedule-driven (June 2026)"
)

print(dengue_sched_results$importation)
print(dengue_sched_results$plot)

# ============================================================
# 9g. Combined schedule-driven panel
# ============================================================

panel_schedule_driven <- plot_grid(
  dengue_sched_results$plot,
  malaria_sched_results$plot,
  measles_sched_results$plot,
  pertussis_sched_results$plot,
  labels = "AUTO", label_size = 20
)

ggsave(panel_schedule_driven,
       file   = "Figures/estimated_importations_schedule_driven.png",
       height = 13, width = 20)

# ============================================================
# 10. THREE-MODEL COMPARISON
# ============================================================
#
# Three nested models are compared side by side for each disease and
# destination city. They form a hierarchy of increasing resolution:
#
#   MODEL 1 — Baseline (Sections 3–6):
#     Travel:    I-92 regional arrivals, mean June 2023–2025
#     Incidence: Regional aggregates
#     Cities:    5 US I-92 gateways (Boston, Dallas, Houston,
#                Newark/NY → New York, Philadelphia)
#
#   MODEL 2 — WC-adjusted (Sections 8b–8h):
#     Travel:    NTTO + COR country-level June 2026 projections
#     Incidence: Country-level (more precise than regional)
#     Cities:    Same 5 I-92 gateway cities (routing fractions unchanged)
#
#   MODEL 3 — Schedule-driven (Section 9):
#     Travel:    Same 2026 projections, decomposed into WC-fan and
#                background streams
#     Incidence: Country-level (same as WC-adjusted)
#     Cities:    WC fans → up to 16 venue cities (schedule-based);
#                Background → 5 I-92 cities (merged with venue cities)
#
# Comparison shows:
#   - Model 1→2: effect of WC travel surge and country-level incidence
#   - Model 2→3: effect of schedule-based venue routing
#     (new cities like Atlanta, Miami, Seattle, LA appear in Model 3)
#
# Two metrics are compared:
#   (a) P(>=1 importation) — probability-scale; saturates to 1 for
#       high-risk cities, so less informative at the high end.
#   (b) Lambda (expected importations) — linear scale; always
#       informative; directly comparable across cities and models.
# ============================================================

# ============================================================
# 10a. Helper: recode I-92 city names and aggregate
# ============================================================
# Baseline and WC-adjusted results use I-92 city names (e.g., "Ny",
# "Newark"). This helper maps them to canonical venue-city names and
# sums any cities that merge (Newark + Ny → New York).

recode_i92_city <- function(df) {
  df %>%
    mutate(city = case_when(
      destination_city == "Newark"       ~ "New York",
      destination_city == "Ny"           ~ "New York",
      destination_city == "Boston"       ~ "Boston",
      destination_city == "Dallas"       ~ "Dallas",
      destination_city == "Houston"      ~ "Houston",
      destination_city == "Philadelphia" ~ "Philadelphia",
      TRUE                               ~ str_to_title(destination_city)
    )) %>%
    group_by(city) %>%
    summarise(imp_intensity = sum(imp_intensity, na.rm = TRUE), .groups = "drop") %>%
    mutate(prob_at_least_one = 1 - exp(-imp_intensity))
}

# ============================================================
# 10b. Assemble the three-model comparison table
# ============================================================

build_comparison <- function(baseline_res, wc_res, sched_res, disease_name) {
  bind_rows(
    recode_i92_city(baseline_res$importation) %>%
      mutate(model = "Baseline"),
    recode_i92_city(wc_res$importation) %>%
      mutate(model = "WC-adjusted"),
    sched_res$importation %>%
      mutate(city = str_to_title(city)) %>%
      select(city, imp_intensity, prob_at_least_one) %>%
      mutate(model = "Schedule-driven")
  ) %>%
    mutate(disease = disease_name)
}

comparison_all <- bind_rows(
  build_comparison(dengue_results,    dengue_wc_results,    dengue_sched_results,    "Dengue"),
  build_comparison(malaria_results,   malaria_wc_results,   malaria_sched_results,   "Malaria"),
  build_comparison(measles_results,   measles_wc_results,   measles_sched_results,   "Measles"),
  build_comparison(pertussis_results, pertussis_wc_results, pertussis_sched_results, "Pertussis")
)

# Colour palette consistent across Sections 10–11
model_colors <- c(
  "Baseline"        = "#4393c3",
  "WC-adjusted"     = "#d6604d",
  "Schedule-driven" = "#74c476"
)

# Order cities by their schedule-driven total importation intensity
# (so the most-at-risk cities appear first on the x-axis)
city_order <- comparison_all %>%
  filter(model == "Schedule-driven") %>%
  group_by(city) %>%
  summarise(total_imp = sum(imp_intensity, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_imp)) %>%
  pull(city)

# ============================================================
# 10c. Comparison plot: P(>=1 importation) by city and model
# ============================================================
# Cities from the schedule-driven model that are absent from the
# baseline/WC-adjusted (e.g., Atlanta, Miami) show P = 0 for Models 1-2.

prob_comparison_plot <- comparison_all %>%
  mutate(
    city  = factor(city, levels = city_order),
    model = factor(model, levels = c("Baseline", "WC-adjusted", "Schedule-driven"))
  ) %>%
  ggplot(aes(x = city, y = prob_at_least_one, fill = model)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  facet_wrap(~disease, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = model_colors) +
  labs(
    x     = "",
    y     = expression(P(X >= 1)),
    fill  = "Model",
    title = "Probability of at least one importation — three-model comparison"
  ) +
  theme_bw() +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 9),
    text         = element_text(size = 13),
    legend.position = "bottom"
  )

ggsave(prob_comparison_plot,
       file   = "Figures/model_comparison_probability.png",
       height = 12, width = 18)

# ============================================================
# 10d. Comparison plot: expected importation intensity (lambda)
# ============================================================
# Lambda stays on a linear scale even when P(>=1) → 1, making it
# more informative for quantifying relative risk between cities and models.

intensity_comparison_plot <- comparison_all %>%
  mutate(
    city  = factor(city, levels = city_order),
    model = factor(model, levels = c("Baseline", "WC-adjusted", "Schedule-driven"))
  ) %>%
  ggplot(aes(x = city, y = imp_intensity, fill = model)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  facet_wrap(~disease, scales = "free", ncol = 2) +
  scale_fill_manual(values = model_colors) +
  labs(
    x     = "",
    y     = expression(lambda ~ "(expected importations)"),
    fill  = "Model",
    title = "Expected importation intensity (λ) — three-model comparison"
  ) +
  theme_bw() +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 9),
    text         = element_text(size = 13),
    legend.position = "bottom"
  )

ggsave(intensity_comparison_plot,
       file   = "Figures/model_comparison_intensity.png",
       height = 12, width = 18)

# ============================================================
# 11. COUNTRY-LEVEL IMPORTATION CONTRIBUTIONS
# ============================================================
#
# The Poisson intensity decomposes additively across source countries:
#   Lambda[city, d] = sum_c lambda[c, city, d]
#
# compute_importation_schedule() already stores per-country, per-city
# contributions in $country_contributions. This section extracts them
# to produce:
#
#   (a) Top source countries overall — ranked by total expected
#       importation burden summed across all destination cities.
#
#   (b) WC-fan vs. background stream breakdown — shows what fraction
#       of each country's importation risk comes from WC-specific
#       versus routine background travel.
#
#   (c) Country × city importation heatmap — a matrix view showing
#       which source countries concentrate risk in specific venue cities
#       (typically those where their national team plays) versus those
#       with diffuse background-travel patterns.
# ============================================================

# ============================================================
# 11a. Aggregate contributions across all four diseases
# ============================================================

all_contributions <- bind_rows(
  dengue_sched_results$country_contributions    %>% mutate(disease = "Dengue"),
  malaria_sched_results$country_contributions   %>% mutate(disease = "Malaria"),
  measles_sched_results$country_contributions   %>% mutate(disease = "Measles"),
  pertussis_sched_results$country_contributions %>% mutate(disease = "Pertussis")
)

# ============================================================
# 11b. Top source countries by total expected importations
# ============================================================
# For each disease, rank countries by total lambda summed across all cities.
# The ordering uses the global (all-city) sum; within-facet reordering
# requires the tidytext package (reorder_within + scale_x_reordered).
# Here we use global ordering which gives a consistent cross-disease view.

top_countries <- all_contributions %>%
  group_by(disease, Country) %>%
  summarise(total_imports = sum(expected_imports, na.rm = TRUE), .groups = "drop") %>%
  group_by(disease) %>%
  slice_max(total_imports, n = 15, with_ties = FALSE) %>%
  ungroup()

country_ranking_plot <- top_countries %>%
  mutate(disease = factor(disease, levels = c("Dengue","Malaria","Measles","Pertussis"))) %>%
  ggplot(aes(x = reorder(Country, total_imports),
             y = total_imports,
             fill = disease)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~disease, scales = "free", ncol = 2) +
  labs(
    x     = "",
    y     = expression(lambda ~ "(expected importations)"),
    title = "Top 15 source countries by expected importation — schedule-driven model"
  ) +
  theme_bw() +
  theme(text = element_text(size = 12))

ggsave(country_ranking_plot,
       file   = "Figures/country_importation_ranking.png",
       height = 12, width = 16)

# ============================================================
# 11c. WC-fan vs. background stream breakdown (dengue example)
# ============================================================
# For the top 20 dengue-contributing countries, shows what fraction of
# importation risk comes from WC-specific fan travel vs. routine tourism.
# Countries with WC teams should show a meaningful WC-fan component;
# non-qualified countries will show only background.

stream_breakdown <- all_contributions %>%
  filter(disease == "Dengue") %>%
  group_by(Country, stream) %>%
  summarise(total_imports = sum(expected_imports, na.rm = TRUE), .groups = "drop") %>%
  group_by(Country) %>%
  mutate(country_total = sum(total_imports)) %>%
  ungroup() %>%
  # Keep the 20 countries with highest dengue importation burden
  filter(Country %in% (all_contributions %>%
    filter(disease == "Dengue") %>%
    group_by(Country) %>%
    summarise(s = sum(expected_imports), .groups = "drop") %>%
    slice_max(s, n = 20, with_ties = FALSE) %>%
    pull(Country)))

stream_plot <- stream_breakdown %>%
  ggplot(aes(x = reorder(Country, country_total),
             y = total_imports,
             fill = stream)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("WC fans" = "#e6550d", "Background" = "#3182bd")) +
  labs(
    x     = "",
    y     = expression(lambda ~ "(expected dengue importations)"),
    fill  = "Travel stream",
    title = "Dengue importation by travel stream: WC fans vs. background (top 20 source countries)"
  ) +
  theme_bw() +
  theme(text = element_text(size = 13))

ggsave(stream_plot,
       file   = "Figures/stream_breakdown_dengue.png",
       height = 9, width = 13)

# ============================================================
# 11d. Country × city importation heatmap
# ============================================================
# A matrix of expected importations by source country (rows) and
# destination city (columns). The top n_countries contributors are shown.
# Countries with WC teams will show importation concentrated in specific
# venue cities; non-WC countries will be spread across I-92 cities only.

make_heatmap <- function(disease_name, n_countries = 20) {

  # City-level totals per country for this disease
  dat <- all_contributions %>%
    filter(disease == disease_name) %>%
    group_by(Country, city) %>%
    summarise(imports = sum(expected_imports, na.rm = TRUE), .groups = "drop")

  # Top-n countries by total importation across all cities
  top_c <- dat %>%
    group_by(Country) %>%
    summarise(country_total = sum(imports), .groups = "drop") %>%
    slice_max(country_total, n = n_countries, with_ties = FALSE)

  dat %>%
    inner_join(top_c, by = "Country") %>%
    mutate(city = str_to_title(city)) %>%
    ggplot(aes(x = city,
               y = reorder(Country, country_total),
               fill = imports)) +
    geom_tile(color = "white", linewidth = 0.3) +
    scale_fill_viridis_c(option = "plasma", name = "Expected\nimportations") +
    labs(
      x     = "Destination city",
      y     = "Source country",
      title = paste0(disease_name, " importation matrix: country × city (schedule-driven)")
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      text        = element_text(size = 12)
    )
}

heatmap_dengue    <- make_heatmap("Dengue")
heatmap_malaria   <- make_heatmap("Malaria")
heatmap_measles   <- make_heatmap("Measles")
heatmap_pertussis <- make_heatmap("Pertussis")

ggsave(heatmap_dengue,    file = "Figures/heatmap_dengue.png",    height = 9, width = 13)
ggsave(heatmap_malaria,   file = "Figures/heatmap_malaria.png",   height = 9, width = 13)
ggsave(heatmap_measles,   file = "Figures/heatmap_measles.png",   height = 9, width = 13)
ggsave(heatmap_pertussis, file = "Figures/heatmap_pertussis.png", height = 9, width = 13)
