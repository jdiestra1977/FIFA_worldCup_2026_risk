# ============================================================
# FIFA World Cup 2026 — Infectious Disease Importation Risk
# Author : Jose Herrera-Diestra
# Updated: May 2026
#
# OVERVIEW
# --------
# This script estimates the probability of at least one imported case
# of dengue, malaria, measles, pertussis, and influenza reaching each
# US host city during June 2026. Three nested models of increasing
# spatial resolution are built and compared:
#
#   Model 1 — Baseline
#     Travel:    COR June 2024 × BTS T-100 routing fractions (no WC adjustment)
#     Incidence: Country-level (same precision as Models 2–3)
#     Cities:    All 11 US WC venue cities covered by T-100
#
#   Model 2 — WC-adjusted
#     Travel:    Country-level June 2026 projections (NTTO + COR × phi_c)
#     Incidence: Country-level (same as Model 1)
#     Cities:    All 11 US WC venue cities covered by T-100
#
#   Model 3 — Schedule-driven
#     Travel:    2026 projections decomposed into WC-fan and background
#                streams; WC fans routed by match schedule
#     Incidence: Country-level (same as Models 1–2)
#     Cities:    11 US WC venue cities (results restricted to US)
#
# The importation framework is a Poisson model (Eq. 1 in manuscript):
#   P(X >= 1) = 1 - exp(-Lambda)
#   Lambda     = sum_s [ arrivals(s,h) * incidence(s,d) * p_d ]
#
# where s = origin unit (region or country), h = host city, d = disease,
# and p_d = probability of travelling while infectious.
#
# Sections:
#   0.  Packages
#   1.  Working directory
#   2.  Reference data (population, COR arrivals, region mapping)
#   3.  Travel volume — T-100 routing fractions + COR June 2024 baseline
#   4.  Disease data (dengue, malaria, measles, pertussis, influenza)
#   5.  Model functions (Poisson core + plot helper)
#   6.  Baseline importation estimates (Model 1)
#   7.  Combined baseline panel figure
#   8.  WC-adjusted importation model (Model 2)
#   9.  Schedule-driven venue routing model (Model 3)
#  10.  Three-model comparison plots
#  11.  Country-level importation contributions
#  12.  Sensitivity analysis (rho and p_travel_inf ± 50 %)
# ============================================================


# ============================================================
# 0. PACKAGES
# ============================================================
library(tidyverse)   # data wrangling, ggplot2, purrr
library(readxl)      # read .xlsx disease/schedule data
library(janitor)     # clean_names() — standardise column names
library(lubridate)   # month(), year() on Date objects
library(cowplot)     # plot_grid() for multi-panel figures
library(maps)        # map_data() — base country/state polygons
library(ggrepel)     # geom_label_repel() — overlap-free map labels


# ============================================================
# 1. WORKING DIRECTORY
# ============================================================
setwd("~/Documents/GitHub/FIFA_worldCup_2026_risk/")


# ============================================================
# 2. REFERENCE DATA
# ============================================================

# --- 2a. Country populations (2020 census baseline) ---------
# Used as the denominator when converting raw case counts to
# per-capita incidence (Eq. 3 and Eq. 6 in manuscript).
population_of_world <- read_csv("Data/population2020.csv") %>%
  rename(Country = COUNTRY, population_country = POPULATION) %>%
  # Harmonise the DRC name to match the disease and arrivals datasets.
  # The COR dataset uses the older "Zaire" convention; we propagate
  # that throughout to avoid broken joins downstream.
  mutate(Country = if_else(Country == "DR Congo", "Zaire (formerly DRC)", Country))

# --- 2b. Monthly arrivals by Country of Residence (COR/I-94) --
# Source: CBP I-94 Monthly Arrivals by Country of Residence
# (https://travel.trade.gov). Used to (i) build the country-to-
# region correspondence table and (ii) extract June 2024 volumes
# for the WC-adjusted and schedule-driven models (Section 8).
arrivals_COR <- read_csv("Data/Monthly_Arrivals_Country_of_Residence_COR_1.csv")

# --- 2c. Country → broad world region mapping ---------------
# Derived from the COR dataset (which carries a World_region field).
# Two structural choices:
#   (1) Western Europe + Eastern Europe → "Europe": keeps region counts
#       manageable and aligns with I-92 regional aggregation.
#   (2) Mexico and Canada kept as own regions: both are co-host nations
#       with volumes and disease profiles distinct from their neighbours.
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
  drop_na() %>%          # drop rows with no population match (territories, etc.)
  mutate(new_region = case_when(
    Country == "Mexico" ~ "Mexico",
    Country == "Canada" ~ "Canada",
    TRUE ~ new_region
  ))

# --- 2d. Regional population totals -------------------------
# Aggregate national populations to the broad region level.
# No longer used in any active model tier (all three models use
# country-level data via T-100 + COR). Retained because it is
# referenced in the preserved I-92 regional baseline blocks.
population_by_region <- correspondence_country_region %>%
  select(new_region, population_country) %>%
  group_by(new_region) %>%
  summarise(popu_region = sum(population_country), .groups = "drop")

# --- 2e. WC 2026 venue map -----------------------------------
# Reads stadium coordinates and produces a North America map
# with colour-coded circles (USA / Canada / Mexico) and
# ggrepel labels to avoid overlap on the East Coast and
# California clusters. Suburb names are mapped to their
# metropolitan area so labels match the rest of the analysis.

stadiums <- read_csv("Data/world_cup_2026_stadiums_coordinates.csv") %>%
  mutate(
    city_label = case_when(
      city == "East Rutherford" ~ "New York",
      city == "Foxborough"      ~ "Boston",
      city == "Arlington"       ~ "Dallas",
      city == "Inglewood"       ~ "Los Angeles",
      city == "Santa Clara"     ~ "San Francisco",
      city == "Miami Gardens"   ~ "Miami",
      TRUE                      ~ city
    ),
    label = paste0(city_label, "\n", stadium),
    # Per-point nudges: push the New York label eastward (over the
    # Atlantic) so it clears the Philadelphia/Boston cluster.
    nudge_x = if_else(city == "East Rutherford",  6.0, 0),
    nudge_y = if_else(city == "East Rutherford", -1.5, 0)
  )

north_america <- map_data("world") %>%
  filter(region %in% c("USA", "Canada", "Mexico"))

venue_map <- ggplot() +
  geom_polygon(
    data  = north_america,
    aes(x = long, y = lat, group = group),
    fill  = "gray92", color = "white", linewidth = 0.25
  ) +
  geom_point(
    data  = stadiums,
    aes(x = longitude, y = latitude, color = country),
    size  = 4, alpha = 0.9
  ) +
  geom_label_repel(
    data          = stadiums,
    aes(x = longitude, y = latitude, label = label, color = country),
    nudge_x       = stadiums$nudge_x,
    nudge_y       = stadiums$nudge_y,
    size          = 2.6,
    box.padding   = 0.55,
    point.padding = 0.4,
    max.overlaps  = Inf,
    segment.size  = 0.3,
    segment.color = "gray50",
    show.legend   = FALSE
  ) +
  coord_fixed(xlim = c(-130, -60), ylim = c(14, 57), ratio = 1.3) +
  scale_color_manual(
    name   = "Host nation",
    values = c("USA" = "#1a6faf", "Canada" = "#c0392b", "Mexico" = "#27ae60")
  ) +
  labs(
    title    = "FIFA World Cup 2026 — Venue Stadiums",
    subtitle = "16 venues across USA (11), Canada (2), and Mexico (3)",
    x = NULL, y = NULL
  ) +
  theme_bw() +
  theme(
    panel.grid      = element_blank(),
    axis.text       = element_blank(),
    axis.ticks      = element_blank(),
    panel.border    = element_rect(color = "gray70"),
    legend.position = "bottom",
    plot.title      = element_text(size = 14, face = "bold"),
    plot.subtitle   = element_text(size = 11)
  )

ggsave(venue_map,
       file   = "Figures/wc2026_venue_map.png",
       height = 8, width = 12, dpi = 300)

# ============================================================
# 3. TRAVEL VOLUME — T-100 ROUTING FRACTIONS + COR JUNE 2024
# ============================================================
#
# WHY BTS T-100 INSTEAD OF I-92
# ------------------------------
# The US International Air Travel Statistics (I-92 programme) was
# previously used to supply routing fractions — the share of arrivals
# from each world region that land at a specific US gateway city. I-92
# covers only 5 gateway cities (Boston, Dallas, Houston, Newark/New York,
# Philadelphia), leaving 6 of the 11 US WC venue cities (Atlanta, Kansas
# City, Los Angeles, Miami, San Francisco, Seattle) with no data.
#
# The BTS T-100 International Segment (Form 41 Traffic — All Carriers)
# provides nonstop international flight segment counts for EVERY US
# airport with scheduled international service. From T-100 we derive
# country-level routing fractions:
#
#   f_{c,v}^{T100} = N_{c,v,y}^{June} / N_{c,US,y}^{June}
#
# averaged over June 2023–2025 for stability. These fractions cover all
# 11 US venue cities, giving a consistent spatial basis across all three
# model tiers.
#
# For Model 1 (baseline), COR June 2024 arrivals are used WITHOUT any
# World Cup growth adjustment. This is the pre-tournament counterfactual:
#   N_{c,v}^{baseline} = COR_{c,June2024} × f_{c,v}^{T100}
#
# NOTE: The original I-92 data loading code is preserved below for
# reference (commented out). It is not used in the current analysis.
# ============================================================


# ============================================================
# ===== BEGIN: ORIGINAL I-92 APPROACH — PRESERVED FOR REFERENCE =====
# ============================================================
# The I-92 programme provides monthly air arrivals by region of origin
# to five specific US gateway cities. This block reads those files,
# computes mean June arrivals, and builds routing fractions at the
# region level. It was replaced by BTS T-100 data (§3b below) because
# T-100 covers all 11 US WC venue cities at country (not region) level.
#
# Source: US International Air Travel Statistics (I-92 programme),
# Bureau of Transportation Statistics / US Dept of Commerce.
# https://www.trade.gov/us-international-air-travel-statistics-i-92-data

# files <- Sys.glob("Data/Selected_cities_and_origins/*.xlsx")
#
# # Helper: parse the region and destination city from the file name,
# # then read the sheet, standardising column names via clean_names().
# # The date_year filter drops any BTS footnote rows (non-numeric years).
# read_arrivals_file <- function(f) {
#   name <- basename(f)
#   region <- name %>%
#     str_extract("data_(.*)_to_") %>%
#     str_remove("^data_") %>%
#     str_remove("_to_$") %>%
#     str_replace_all("_", " ") %>%
#     str_to_title()
#   destination <- name %>%
#     str_extract("to_.*\\.xlsx") %>%
#     str_remove("^to_") %>%
#     str_remove("\\.xlsx$") %>%
#     str_replace_all("_", " ") %>%
#     str_to_title()
#   read_excel(f) %>%
#     clean_names() %>%
#     mutate(region_origin = region, destination_city = destination) %>%
#     filter(str_detect(as.character(date_year), "^(19|20)\\d{2}$"))
# }
#
# data_all <- map_dfr(files, read_arrivals_file)
#
# # Shading rectangles for May–July (the WC window) used in time-series
# # plots. Built from the full date range in the I-92 data.
# shade_df <- data_all %>%
#   mutate(year_month = as.Date(paste(date_year, month_number, 1, sep = "-"))) %>%
#   distinct(year_month) %>%
#   filter(month(year_month) %in% c(5, 6, 7)) %>%
#   mutate(xmin = year_month, xmax = year_month + months(1))
#
# definite_data_arrivals <- data_all %>%
#   mutate(
#     all_arrivals = foreign_originating + foreign_returning + u_s_citizen_returning,
#     year_month   = as.Date(paste(date_year, month_number, 1, sep = "-"))
#   ) %>%
#   select(date_year, month_number, date_month, region_origin,
#          destination_city, all_arrivals, year_month) %>%
#   drop_na() %>%
#   filter(!destination_city %in% c("Austin")) %>%
#   filter(!region_origin   %in% c("Oceania", "World"))
#
# usa_arrivals <- definite_data_arrivals %>%
#   filter(destination_city == "Usa") %>%
#   select(region_origin, year_month, all_arrivals) %>%
#   rename(all_arrivals_usa = all_arrivals)
#
# definite_data_arrivals <- definite_data_arrivals %>%
#   left_join(usa_arrivals, by = c("region_origin", "year_month")) %>%
#   filter(destination_city != "Usa")
#
# # Diagnostic time-series plot (May–July shaded)
# arrivals_time_plot <- definite_data_arrivals %>%
#   ggplot(aes(x = year_month, y = all_arrivals,
#              color = destination_city, group = destination_city)) +
#   theme_bw() +
#   geom_rect(data = shade_df, inherit.aes = FALSE,
#     aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
#     fill = "gray80", alpha = 0.5) +
#   geom_line() +
#   facet_wrap(~region_origin, scales = "free_y")
# print(arrivals_time_plot)
# ggsave(filename = "Figures/temporal_arrivals_from_regions.png",
#        plot = arrivals_time_plot, height = 6, width = 10)
#
# # Mean June arrivals (2023–2025) per region × city.
# arrivals_only_june <- definite_data_arrivals %>%
#   filter(month_number == 6, as.numeric(date_year) >= 2023) %>%
#   group_by(region_origin, destination_city) %>%
#   summarise(arrivals_June = mean(all_arrivals, na.rm = TRUE), .groups = "drop")
#
# # Mean June US-total arrivals per region (2023–2025).
# mean_arrivals_all_usa_june <- definite_data_arrivals %>%
#   filter(month_number == 6, as.numeric(date_year) >= 2023) %>%
#   group_by(region_origin) %>%
#   summarise(mean_all_usa_June = mean(all_arrivals_usa, na.rm = TRUE), .groups = "drop")
# ============================================================
# ===== END: ORIGINAL I-92 APPROACH =====
# ============================================================


# ---- 3a. Shading helper for diagnostic plots ----------------
# May–July rectangles used in dengue time-series (Section 4).
# No longer derived from I-92 data; computed directly from a fixed range.
shade_df <- tibble(start_year = 2019:2025) %>%
  mutate(
    xmin = as.Date(paste(start_year, "05", "01", sep = "-")),
    xmax = as.Date(paste(start_year, "07", "31", sep = "-"))
  )

# ---- 3b. COR June 2024 country-level arrivals ---------------
# Extract June 2024 from the COR/I-94 dataset. This is the baseline
# travel volume (no WC adjustment) used in Model 1, and the base year
# for the growth factors applied in Models 2 and 3. Extracting it here
# makes it available to all downstream sections.
cor_june_2024 <- arrivals_COR %>%
  select(Country, World_region, `2024-06`) %>%
  mutate(june_2024 = readr::parse_number(as.character(`2024-06`))) %>%
  select(Country, World_region, june_2024) %>%
  drop_na()

# ---- 3c. T-100 country-level routing fractions --------------
# Pre-computed by Code/t100_routing_prep.R. Run that script once to
# regenerate Data/t100_routing_fractions.csv from the BTS downloads.
#
# WHY T-100 IS THE RIGHT CHOICE FOR ALL THREE MODELS
# ---------------------------------------------------
# T-100 records actual nonstop international passenger segments landing
# at each US airport. From these counts we compute:
#
#   f_{c,v}^{T100} = N_{c,v,y}^{June} / N_{c,US,y}^{June}
#
# averaged over June 2023–2025. Key advantages over I-92:
#
#   (1) Country-level resolution: every source country gets its own
#       routing fraction based on its actual direct flight patterns,
#       rather than inheriting its broad region's average.
#
#   (2) Complete US WC city coverage: T-100 covers all 11 US host
#       cities — including Los Angeles, Atlanta, Kansas City, Miami,
#       San Francisco, and Seattle, which have zero coverage in I-92.
#
#   (3) Connecting-itinerary interpretation: T-100 records the
#       international segment entry point. A traveller flying
#       São Paulo → Miami → Kansas City appears under Miami, not
#       Kansas City. Kansas City's near-zero routing fractions for
#       most countries thus correctly reflect its limited direct
#       international service; WC fans travelling there are captured
#       by the schedule-driven fan stream (Model 3).
#
# All three model tiers use these fractions, giving a consistent
# spatial basis for the three-way comparison.
t100_routing <- read_csv("Data/t100_routing_fractions.csv",
                         show_col_types = FALSE)

message("T-100 routing fractions loaded: ",
        n_distinct(t100_routing$Country), " countries × ",
        n_distinct(t100_routing$venue_city), " venue cities")

# ============================================================
# 4. DISEASE DATA
# ============================================================

# ---- 4a. Dengue (monthly WHO data) -------------------------
# Source: WHO Global Dengue Surveillance dataset (accessed Dec 2025).
# Data are monthly country-level reported case counts. We use
# June 2024–2025 (two most recent complete Junes) for both the
# regional baseline and the country-level WC-adjusted estimates.
dengue_data_world <- read_xlsx("Data/dengue-global-data-2025-12-10.xlsx")

dengue_data_world_selected <- dengue_data_world %>%
  select(date, date_lab, who_region_long, country, cases) %>%
  # Recode long-form WHO country names to match COR naming conventions.
  # These four countries have the longest name discrepancies.
  mutate(country = recode(country,
    "Venezuela (Bolivarian Republic of)" = "Venezuela",
    "Bolivia (Plurinational State of)"   = "Bolivia",
    "Iran (Islamic Republic of)"         = "Iran",
    "United Republic of Tanzania"        = "Tanzania")) %>%
  left_join(correspondence_country_region %>%
              select(country = Country, new_region), by = "country")

# Diagnostic: dengue seasonality by region (2019 onwards).
# Canada and Europe are excluded — dengue is not endemic there and
# any reported cases are themselves importations.
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

# Mean regional dengue burden in June (2024–2025).
# Two June months are available for averaging. Using 2024+ avoids
# the anomalous 2023 dengue season (unusually high in several regions)
# and gives the most recent signal before the tournament.
mean_dengue_cases_regions_june <- dengue_data_world_selected %>%
  filter(month(date) == 6, year(date) > 2023) %>%
  drop_na() %>%
  select(date, new_region, cases) %>%
  group_by(date, new_region) %>%
  summarise(cases_region = sum(cases), .groups = "drop") %>%
  group_by(new_region) %>%
  summarise(mean_cases_june = mean(cases_region, na.rm = TRUE), .groups = "drop") %>%
  # "Middle East" → "Mideast": match the shorter label used in the
  # I-92 data files so the join in Section 6 succeeds.
  mutate(new_region = if_else(new_region == "Middle East", "Mideast", new_region))

# ---- 4b. Malaria (annual incidence per 1,000, 2024) --------
# Source: WHO Global Malaria Programme — National Unit Data.
# Metric used: "Incidence Rate" (cases per 1,000 population, 2024).
# Divided by 12 in Section 6 to approximate a monthly rate.
malaria_data_raw <- read_csv("Data/Malaria_National_Unit_data.csv")

malaria_cases <- malaria_data_raw %>%
  filter(Year == 2024, Metric == "Incidence Rate") %>%
  select(Country = Name, cases_per1K = Value)

# ---- 4c. Measles (annual incidence per 1,000,000) ----------
# Source: WHO Immunization Data portal (accessed Sep 2025).
# Column 4 is the incidence rate; coercion to numeric drops any
# header/footnote rows that slipped through.
measles_data <- read_xlsx("Data/Measles reported cases and incidence 2025-09-12 14-18 UTC.xlsx")

measles_incidence <- measles_data %>%
  select(Country = 1, incidence_per1M = 4) %>%
  drop_na() %>%
  mutate(incidence_per1M = as.numeric(incidence_per1M))

# ---- 4d. Pertussis (annual incidence per 1,000,000) --------
# Source: WHO Immunization Data portal (accessed Dec 2025).
# Same structure as measles data above.
pertussis_data <- read_xlsx("Data/Pertussis reported cases and incidence 2025-22-12 14-46 UTC.xlsx")

pertussis_incidence <- pertussis_data %>%
  select(Country = 1, incidence_per1M = 4) %>%
  drop_na() %>%
  mutate(incidence_per1M = as.numeric(incidence_per1M))

# ---- 4e. Influenza (weekly positive specimens, FluNet) ------
# Source: WHO FluNet / GISRS global surveillance (accessed May 2026).
# API: https://xmart-api-public.who.int/FLUMART/VIW_FNT?$format=csv
#
# FluNet reports weekly specimen counts (not true case counts), so
# INF_ALL (all influenza A+B positive specimens) is used as a proxy
# for reported cases, consistent with how measles/pertussis data are
# used. The under-reporting correction rho accounts for the gap
# between laboratory-confirmed specimens and true incidence.
#
# June corresponds to ISO weeks 22-26. We average over 2023-2025
# to match the temporal window used for other diseases.
flunet_cache <- "Data/flunet_viwfnt.csv"

if (!file.exists(flunet_cache)) {
  flunet_url <- "https://xmart-api-public.who.int/FLUMART/VIW_FNT?$format=csv"
  download.file(flunet_url, destfile = flunet_cache, mode = "wb")
  message("FluNet data downloaded and cached at ", flunet_cache)
} else {
  message("Loading FluNet data from local cache: ", flunet_cache)
}

flunet_raw <- read_csv(flunet_cache, show_col_types = FALSE)

# Keep only the columns we need and filter to June ISO weeks 2023-2025
flunet_june <- flunet_raw %>%
  select(Country = COUNTRY_AREA_TERRITORY, YEAR = ISO_YEAR, WEEK = ISO_WEEK,
         inf_all = INF_ALL, spec_processed = SPEC_PROCESSED_NB) %>%
  filter(YEAR %in% 2023:2025, WEEK %in% 22:26) %>%
  mutate(inf_all = as.numeric(inf_all)) %>%
  drop_na(inf_all) %>%
  # Harmonise FluNet country names → COR naming conventions
  mutate(Country = recode(Country,
    "United Kingdom of Great Britain and Northern Ireland" = "United Kingdom",
    "Republic of Korea"               = "South Korea",
    "Bolivia (Plurinational State of)"= "Bolivia",
    "Venezuela (Bolivarian Republic of)" = "Venezuela",
    "Iran (Islamic Republic of)"      = "Iran",
    "United Republic of Tanzania"     = "Tanzania",
    "Democratic Republic of the Congo"= "Zaire (formerly DRC)",
    "Viet Nam"                        = "Vietnam",
    "The former Yugoslav Republic of Macedonia" = "North Macedonia",
    "Republic of Moldova"             = "Moldova",
    "Czechia"                         = "Czech Republic"
  ))

# Mean June positive specimens per country across 2023-2025
influenza_june_specimens <- flunet_june %>%
  group_by(Country, YEAR) %>%
  summarise(year_june_inf = sum(inf_all, na.rm = TRUE), .groups = "drop") %>%
  group_by(Country) %>%
  summarise(mean_june_inf = mean(year_june_inf, na.rm = TRUE), .groups = "drop")

# ============================================================
# 5. MODEL FUNCTIONS
# ============================================================

# ---- 5a. Bar-chart helper ----------------------------------
# Produces a standard importation intensity bar chart ordered from
# highest to lowest risk. The label above each bar shows the rounded
# P(>=1) value so the probability metric is visible without a
# separate plot.
plot_importation <- function(df, title_text) {
  ggplot(df, aes(x = reorder(destination_city, -imp_intensity), y = imp_intensity)) +
    geom_col(fill = "steelblue") +
    geom_text(aes(label = round(prob_at_least_one, 2)), vjust = -0.5, size = 8) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
    labs(x = "", y = "Importation intensity", title = title_text) +
    theme_bw() + theme(text = element_text(size = 26),axis.text.x = element_text(angle=45,hjust = 1))
}

# ---- 5b. Core Poisson importation model (region-level) -----
# Preserved for reference; not used in current analysis (all three
# models now use country-level routing and incidence via §5c below).
#
# Implements Equations 4–5 from the manuscript at region level:
#
#   lambda[r, h] = N_{r,h} * I_{r,d} * p_d
#   Lambda[h]    = sum_r lambda[r, h]
#   P(>=1)       = 1 - exp(-Lambda[h])
#
# Arguments:
#   arrivals_df         — region_origin, destination_city, arrivals_June
#   region_incidence_df — region_origin, total_inc
#                         (total_inc = rho_d * cases / population)
#   p_travel_inf        — scalar p_d: probability of travelling while
#                         infectious (disease-specific, see Table 1)
#   title_text          — plot title
#
# Returns a list:
#   $expected_imports — full region × city contribution table
#   $importation      — city-level Lambda and P(>=1)
#   $plot             — bar chart
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

# ---- 5c. Core Poisson importation model (country-level) ----
#
# Used by all three model tiers (§6 Baseline, §8 WC-adjusted, §9
# Schedule-driven). Implements the same Poisson logic as §5b but
# at country granularity and using T-100-routed arrivals:
#
#   lambda[c, v] = N_{c,v} * I_{c,d} * p_d
#   Lambda[v]    = sum_c lambda[c, v]
#   P(>=1)       = 1 - exp(-Lambda[v])
#
# Arguments:
#   arrivals_df    — Country, destination_city, arrivals_june_2026
#                    (the column name arrivals_june_2026 is used for
#                     all tiers, even the baseline; it just holds
#                     COR 2024 values with no growth factor there)
#   country_inc_df — Country, total_inc
#                    (total_inc = rho_d * incidence_metric)
#   p_travel_inf   — scalar p_d
#   title_text     — plot title
#
# Returns a list:
#   $importation — city-level Lambda and P(>=1)
#   $plot        — bar chart
compute_importation_country_level <- function(arrivals_df,
                                              country_inc_df,
                                              p_travel_inf = 1,
                                              title_text   = "Estimated importation intensity (country-level)") {
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
# 6. BASELINE IMPORTATION ESTIMATES (MODEL 1 — T-100 BASELINE)
# ============================================================
#
# WHY T-100 + COR 2024 FOR THE BASELINE
# --------------------------------------
# Model 1 establishes the pre-tournament importation risk — what we
# would expect in a typical June without the World Cup effect. Using
# T-100 routing fractions with COR June 2024 arrivals (no growth
# factor) achieves three things:
#
#   (1) All 11 US venue cities are represented. The old I-92 baseline
#       produced zero risk for Atlanta, Kansas City, Los Angeles, Miami,
#       San Francisco, and Seattle — not because those cities are safe,
#       but because I-92 simply didn't cover them. T-100 fixes this.
#
#   (2) Country-level precision. T-100 routing fractions are country-
#       specific, giving each source nation its own city-allocation
#       share rather than its entire world region's average.
#
#   (3) Consistent spatial basis across all three models. Models 2 and
#       3 both use T-100 fractions; Model 1 now does too. The three-way
#       comparison therefore isolates exactly one change at each step:
#         Model 1 → Model 2: WC travel surge (phi_c growth factors)
#         Model 2 → Model 3: schedule-based fan routing
#
# ---- 6a. Disease parameters (shared across §6, §8, §9) ------
# Parameters are defined once here and reused in all downstream
# sections. See Table 1 in the manuscript for derivation rationale.

# Dengue:
# rho = 0.10: literature puts global detection at 6-26% of symptomatic
#             cases (expansion factor ~4-20; mean ~8 in SE Asia).
#             Undurraga et al. 2013 (PLOS NTD) and Bhatt et al. 2013
#             (Nature) support values of 0.06-0.15 for mixed-income
#             source countries. We use 0.10 (conservative upper end
#             for global endemic regions).
# p   = 0.50: early viraemic phase is often mild; ~40% of dengue
#             travellers are viremic on arrival in Europe (empirical).
#             Liebman & Wilder-Smith 2018; Tatem et al. 2012.
under_rho_dengue    <- 0.10
p_travel_inf_dengue <- 0.5

# Malaria:
# rho = 0.20: WHO World Malaria Report methodology implies ~11% detection
#             in Africa (EF ~9); Americas/SE Asia ~28-55%. Global mean
#             of 0.20 is well-calibrated (WHO WMR 2022-2024 Annex).
# p   = 0.30: significant illness; long incubation (7-14 d) means most
#             travellers develop illness post-return. VFR travellers who
#             return home febrile push the estimate to ~0.3.
under_rho_malaria    <- 0.2
p_travel_inf_malaria <- 0.3

# Measles:
# rho = 0.60: notifiable disease; detection ~40-80% in middle/high-income
#             source countries (Simons et al. 2012, Lancet). Appropriate
#             for the mix of WC source nations.
# p   = 0.05: prostrating illness (high fever, rash); ambulatory only
#             during short pre-rash prodrome. Well-supported.
under_rho_measles    <- 0.6
p_travel_inf_measles <- 0.05

# Pertussis:
# rho = 0.10: massively under-reported. McLaughlin et al. 2016 found
#             adult detection ~1-3% (EF 42-93x). Crowcroft et al. 2018
#             gives 2-68% by age group. We use 0.10 as a conservative
#             upper bound consistent with the upper tail of the literature.
# p   = 0.70: catarrhal stage mimics a common cold; fully ambulatory;
#             diagnosis typically delayed weeks. GeoSentinel data confirm
#             routine travel during the most infectious phase.
under_rho_pertussis    <- 0.10
p_travel_inf_pertussis <- 0.7

# Influenza:
# rho = 0.10: FluNet reports laboratory-confirmed specimens; large
#             fraction of community influenza goes untested. ILI-based
#             studies (WHO GISRS, Iuliano et al. 2018 Lancet) estimate
#             true incidence ~10-30x confirmed counts; we use 0.10 as a
#             conservative upper bound for the positive-specimen proxy.
# p   = 0.50: influenza illness is moderate; many travellers continue
#             journeys during early illness (2-3 day incubation + 1-2 day
#             prodrome). GeoSentinel and sentinel surveillance data support
#             ~0.4-0.6 for seasonal influenza. June = Southern Hemisphere
#             peak season (Brazil, Argentina, Australia) amplifying risk.
under_rho_influenza    <- 0.10
p_travel_inf_influenza <- 0.50

# ---- 6b. Country-level disease incidence tables ---------------
# These tables are computed once here and used in §6, §8, and §9.
# total_inc[c] = rho_d * (disease metric for country c)
# The metric varies by data source:
#   dengue   — mean June cases (2024–2025) / national population
#   malaria  — annual incidence per 1,000 / (12 × 1,000)
#   measles  — annual incidence per 1,000,000 / (12 × 1e6)
#   pertussis— annual incidence per 1,000,000 / (12 × 1e6)

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
  mutate(total_inc = under_rho_dengue * mean_june_cases / population_country) %>%
  select(Country, total_inc)

malaria_country_inc <- malaria_cases %>%
  mutate(Country = recode(Country,
    "Democratic Republic of the Congo" = "Zaire (formerly DRC)")) %>%
  mutate(total_inc = under_rho_malaria * cases_per1K / (12 * 1000)) %>%
  select(Country, total_inc)

measles_country_inc <- measles_incidence %>%
  mutate(Country = recode(Country,
    "Democratic Republic of the Congo" = "Zaire (formerly DRC)")) %>%
  mutate(total_inc = under_rho_measles * incidence_per1M / (12 * 1e6)) %>%
  select(Country, total_inc)

pertussis_country_inc <- pertussis_incidence %>%
  mutate(Country = recode(Country,
    "Democratic Republic of the Congo" = "Zaire (formerly DRC)")) %>%
  mutate(total_inc = under_rho_pertussis * incidence_per1M / (12 * 1e6)) %>%
  select(Country, total_inc)

# Influenza: mean June positive specimens (2023–2025) / national population
# Specimens serve as a proxy for reported cases; rho corrects for under-detection.
influenza_june_country <- influenza_june_specimens %>%
  left_join(population_of_world, by = "Country") %>%
  drop_na(population_country) %>%
  filter(mean_june_inf > 0) %>%
  mutate(total_inc = under_rho_influenza * mean_june_inf / population_country) %>%
  select(Country, total_inc)

# ---- 6c. Baseline arrivals: COR June 2024 × T-100 routing ----
# N_{c,v}^{baseline} = COR_{c,June2024} × f_{c,v}^{T100}
# No growth factor — this is the no-WC counterfactual.
# The column is named arrivals_june_2026 for compatibility with
# compute_importation_country_level() which is shared across tiers.
arrivals_baseline <- cor_june_2024 %>%
  left_join(t100_routing, by = "Country") %>%
  drop_na(venue_city) %>%
  mutate(
    arrivals_june_2026 = june_2024 * routing_fraction,
    destination_city   = venue_city
  ) %>%
  select(Country, destination_city, arrivals_june_2026)

# ---- 6d. Baseline estimates for all five diseases ------------
dengue_results <- compute_importation_country_level(
  arrivals_df    = arrivals_baseline,
  country_inc_df = dengue_june_country,
  p_travel_inf   = p_travel_inf_dengue,
  title_text     = "Dengue importation intensity — Baseline"
)
print(dengue_results$importation)
print(dengue_results$plot)

malaria_results <- compute_importation_country_level(
  arrivals_df    = arrivals_baseline,
  country_inc_df = malaria_country_inc,
  p_travel_inf   = p_travel_inf_malaria,
  title_text     = "Malaria importation intensity — Baseline"
)
print(malaria_results$importation)
print(malaria_results$plot)

measles_results <- compute_importation_country_level(
  arrivals_df    = arrivals_baseline,
  country_inc_df = measles_country_inc,
  p_travel_inf   = p_travel_inf_measles,
  title_text     = "Measles importation intensity — Baseline"
)
print(measles_results$importation)
print(measles_results$plot)

pertussis_results <- compute_importation_country_level(
  arrivals_df    = arrivals_baseline,
  country_inc_df = pertussis_country_inc,
  p_travel_inf   = p_travel_inf_pertussis,
  title_text     = "Pertussis importation intensity — Baseline"
)
print(pertussis_results$importation)
print(pertussis_results$plot)

influenza_results <- compute_importation_country_level(
  arrivals_df    = arrivals_baseline,
  country_inc_df = influenza_june_country,
  p_travel_inf   = p_travel_inf_influenza,
  title_text     = "Influenza importation intensity — Baseline"
)
print(influenza_results$importation)
print(influenza_results$plot)

# ============================================================
# ===== BEGIN: ORIGINAL I-92 BASELINE (MODEL 1) — PRESERVED FOR REFERENCE =====
# ============================================================
# The original baseline used I-92 regional arrivals (arrivals_only_june)
# with broad-region incidence aggregates. It only covered 5 cities and
# used region-level disease aggregates. Replaced by §6 above, which uses
# T-100 routing + COR June 2024 + country-level incidence for all 11 US
# WC venue cities.

# # ---- Original 6a. Dengue (I-92 regional) --------------------
# dengue_region_incidence <- mean_dengue_cases_regions_june %>%
#   rename(region_origin = new_region) %>%
#   left_join(
#     population_by_region %>%
#       mutate(new_region = if_else(new_region == "Middle East", "Mideast", new_region)) %>%
#       rename(region_origin = new_region),
#     by = "region_origin"
#   ) %>%
#   mutate(total_inc = under_rho_dengue * mean_cases_june / popu_region) %>%
#   select(region_origin, total_inc)
#
# dengue_results <- compute_importation_from_region_incidence(
#   arrivals_df         = arrivals_only_june,
#   region_incidence_df = dengue_region_incidence,
#   p_travel_inf        = p_travel_inf_dengue,
#   title_text          = "Estimated dengue importation intensity by destination city"
# )
# print(dengue_results$importation)
# print(dengue_results$plot)
#
# # ---- Original 6b. Malaria (I-92 regional) -------------------
# malaria_region_incidence <- malaria_cases %>%
#   mutate(Country = recode(Country,
#     "Democratic Republic of the Congo" = "Zaire (formerly DRC)")) %>%
#   left_join(correspondence_country_region %>%
#               select(Country, region_origin = new_region), by = "Country") %>%
#   drop_na() %>%
#   mutate(Incidence = under_rho_malaria * cases_per1K / (12 * 1000)) %>%
#   group_by(region_origin) %>%
#   summarise(total_inc = sum(Incidence, na.rm = TRUE), .groups = "drop")
#
# malaria_results <- compute_importation_from_region_incidence(
#   arrivals_df         = arrivals_only_june,
#   region_incidence_df = malaria_region_incidence,
#   p_travel_inf        = p_travel_inf_malaria,
#   title_text          = "Estimated malaria importation intensity by destination city"
# )
# print(malaria_results$importation)
# print(malaria_results$plot)
#
# # ---- Original 6c. Measles (I-92 regional) -------------------
# measles_region_incidence <- measles_incidence %>%
#   mutate(Country = recode(Country,
#     "Democratic Republic of the Congo" = "Zaire (formerly DRC)")) %>%
#   left_join(correspondence_country_region %>%
#               select(Country, region_origin = new_region), by = "Country") %>%
#   drop_na() %>%
#   mutate(Incidence = under_rho_measles * incidence_per1M / (12 * 1000000)) %>%
#   group_by(region_origin) %>%
#   summarise(total_inc = sum(Incidence, na.rm = TRUE), .groups = "drop")
#
# measles_results <- compute_importation_from_region_incidence(
#   arrivals_df         = arrivals_only_june,
#   region_incidence_df = measles_region_incidence,
#   p_travel_inf        = p_travel_inf_measles,
#   title_text          = "Estimated measles importation intensity by destination city"
# )
# print(measles_results$importation)
# print(measles_results$plot)
#
# # ---- Original 6d. Pertussis (I-92 regional) -----------------
# pertussis_region_incidence <- pertussis_incidence %>%
#   mutate(Country = recode(Country,
#     "Democratic Republic of the Congo" = "Zaire (formerly DRC)")) %>%
#   left_join(correspondence_country_region %>%
#               select(Country, region_origin = new_region), by = "Country") %>%
#   drop_na() %>%
#   mutate(Incidence = under_rho_pertussis * incidence_per1M / (12 * 1000000)) %>%
#   group_by(region_origin) %>%
#   summarise(total_inc = sum(Incidence, na.rm = TRUE), .groups = "drop")
#
# pertussis_results <- compute_importation_from_region_incidence(
#   arrivals_df         = arrivals_only_june,
#   region_incidence_df = pertussis_region_incidence,
#   p_travel_inf        = p_travel_inf_pertussis,
#   title_text          = "Estimated pertussis importation intensity by destination city"
# )
# print(pertussis_results$importation)
# print(pertussis_results$plot)
# ============================================================
# ===== END: ORIGINAL I-92 BASELINE =====
# ============================================================

# ============================================================
# 7. COMBINED BASELINE PANEL FIGURE
# ============================================================
panel_results_plots <- plot_grid(
  dengue_results$plot,
  malaria_results$plot,
  measles_results$plot,
  pertussis_results$plot,
  influenza_results$plot,
  ncol = 2
)

ggsave(panel_results_plots,
       file   = "Figures/estimated_importations.png",
       height = 21, width = 20)

# ============================================================
# 8. WC-ADJUSTED IMPORTATION MODEL (MODEL 2)
# ============================================================
#
# MOTIVATION
# ----------
# The baseline model (§6) uses COR June 2024 volumes without any World
# Cup adjustment, representing normal tourist patterns. Model 2 adds
# the WC-driven travel surge by projecting June 2026 arrivals using
# growth factors derived from official forecasts.
#
# TWO-SOURCE TRAVEL VOLUME STRATEGY
# ----------------------------------
# Tier 1 — NTTO country-specific projections (12 source markets):
#   Official 2026 estimates from the National Travel and Tourism Office
#   (NTTO 2025 Forecast Report, trade.gov). These already incorporate
#   the WC tourism effect — do NOT add WC visitors on top.
#   Growth factor: phi_c = V_{c,2026} / V_{c,2024}
#
# Tier 2 — COR June 2024 scaled by global growth factors:
#   WC-qualified countries not in Tier 1:
#     phi_WC = 85,017 / 72,390 = 1.174 (NTTO total; includes WC uplift)
#   Non-qualified countries:
#     phi_base = 1.134 (~6.5 % annual growth × 2 years, no WC effect)
#
# CITY ROUTING
# ------------
# All models use BTS T-100 country-level routing fractions (§3c):
#   f_{c,v}^{T100} = mean June direct passengers from c to v / total to US
# This gives each source country its own city-allocation share and
# covers all 11 US venue cities.
#
# DISEASE BURDEN
# --------------
# Country-level incidence tables (dengue_june_country, etc.) were
# computed in §6b and are reused here unchanged. The same rho and p
# parameters ensure that Model 1 → Model 2 differences are driven
# entirely by the WC travel surge, not by incidence assumptions.
# ============================================================

# ---- 8a. Load supporting data (NTTO projections + WC teams) --

# NTTO 2026 projections for the top 12 source markets.
# growth_factor_2024_2026 = visitors_2026 / visitors_2024
# Column country_ntto uses COR naming conventions (e.g., "South Korea",
# "United Kingdom") so the join in Section 8b works without recoding.
ntto_2026 <- read_csv("Data/ntto_forecast_2026.csv", show_col_types = FALSE) %>%
  rename(country_cor = country_ntto)

# WC 2026 qualified teams: 48 countries with a 'host' flag.
# Used to distinguish WC-qualified countries (phi_WC) from
# non-qualified ones (phi_base) in the growth factor assignment.
wc_teams <- read_csv("Data/wc2026_qualified_teams.csv", show_col_types = FALSE)

# ---- 8b. Data-driven baseline growth factor (phi_base) --------
#
# phi_base is the 2024→2026 growth factor for non-WC-qualified
# countries, representing background growth in US inbound travel
# independent of the tournament.
#
# DERIVATION
# ----------
# Estimated from two independent sources covering June 2023–2025:
#   (1) COR monthly arrivals: total June arrivals to the US
#   (2) BTS T-100: total June international passengers to US
#
# For each source we compute the geometric mean annual growth rate:
#   g_bar = (N_June_2025 / N_June_2023)^(1/2)
# and the 2-year forward factor:
#   phi_base = g_bar^2 = N_June_2025 / N_June_2023
#
# The two estimates are averaged to give the final phi_base.

# COR-based estimate
cor_june_totals <- arrivals_COR %>%
  mutate(across(c(`2023-06`, `2024-06`, `2025-06`),
                ~ readr::parse_number(as.character(.)))) %>%
  summarise(
    june_2023 = sum(`2023-06`, na.rm = TRUE),
    june_2024 = sum(`2024-06`, na.rm = TRUE),
    june_2025 = sum(`2025-06`, na.rm = TRUE)
  )

g1_cor       <- cor_june_totals$june_2024 / cor_june_totals$june_2023
g2_cor       <- cor_june_totals$june_2025 / cor_june_totals$june_2024
phi_base_cor <- sqrt(g1_cor * g2_cor)^2   # = june_2025 / june_2023

# T-100-based estimate
t100_june_totals <- map_dfr(2023:2025, function(yr) {
  read_csv(
    paste0("Data/Data_BTS/T_T100I_SEGMENT_ALL_CARRIER_", yr, ".csv"),
    show_col_types = FALSE
  ) %>%
    filter(MONTH == 6, DEST_COUNTRY == "US") %>%
    summarise(total_pax = sum(PASSENGERS, na.rm = TRUE), year = yr)
})

pax          <- t100_june_totals$total_pax
g1_t100      <- pax[2] / pax[1]
g2_t100      <- pax[3] / pax[2]
phi_base_t100 <- sqrt(g1_t100 * g2_t100)^2   # = pax[3] / pax[1]

# Final phi_base: average of both sources
growth_baseline <- (phi_base_cor + phi_base_t100) / 2

message(sprintf(
  "phi_base — COR: %.4f | T-100: %.4f | average (used): %.4f",
  phi_base_cor, phi_base_t100, growth_baseline))

# ---- 8c. Build country-level June 2026 travel volume ---------

# WC aggregate growth factor from NTTO totals (includes WC uplift)
growth_wc_total  <- 85017 / 72390  # 1.174

# cor_june_2024 was already extracted in §3b; it is used here as the
# base year to which growth factors are applied.

# Assign a growth factor to every country using the three-tier hierarchy:
#   Priority 1: NTTO country-specific factor (most accurate; 12 countries)
#   Priority 2: WC global factor for all other WC-qualified countries
#   Priority 3: Baseline factor for non-qualified countries
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
      !is.na(growth_factor_2024_2026) ~ growth_factor_2024_2026,  # Tier 1: NTTO
      !is.na(host)                    ~ growth_wc_total,           # Tier 2: WC team
      TRUE                            ~ growth_baseline             # Tier 3: baseline
    ),
    june_2026 = june_2024 * growth_factor
  ) %>%
  select(Country, World_region, june_2024, growth_factor, june_2026)

# ============================================================
# ===== BEGIN: ORIGINAL I-92 ROUTING FRACTIONS — PRESERVED FOR REFERENCE =====
# ============================================================
# Previously used by the baseline model (Model 1) to allocate regional
# arrivals to the 5 I-92 gateway cities. Replaced by T-100 routing
# fractions (§3c), which cover all 11 US venue cities at country level.
# f_{r, h} = mean_June_arrivals(region r, city h) / mean_June_arrivals(region r, USA)

# routing_fractions <- arrivals_only_june %>%
#   left_join(mean_arrivals_all_usa_june, by = "region_origin") %>%
#   mutate(routing_fraction = arrivals_June / mean_all_usa_June) %>%
#   select(region_origin, destination_city, routing_fraction)
# ============================================================
# ===== END: ORIGINAL I-92 ROUTING FRACTIONS =====
# ============================================================

# ---- 8d. Country × city arrivals matrix (T-100 routing) ------
# Replaces the I-92 region-level routing used in the original version.
#
# N_{c,h}^{2026} = june_2026[c] × f_{c, h}^{T100}   (Eq. 5 in manuscript)
#
# Two improvements over the prior I-92-based version:
#   (1) Country-level routing: each country gets its own fraction
#       instead of inheriting its broad region's average.
#   (2) All 11 US WC venue cities are covered (not just 5 I-92 gateways).
#
# Countries with no T-100 routing data (e.g., North Korea, some small
# island nations with no direct US service) are silently dropped via
# drop_na(venue_city). Their june_2026 volumes are negligible.
arrivals_country_city_2026 <- travel_volume_june_2026 %>%
  left_join(t100_routing, by = "Country") %>%
  drop_na(venue_city) %>%
  mutate(
    arrivals_june_2026 = june_2026 * routing_fraction,
    destination_city   = venue_city   # rename for compatibility with plot/model functions
  ) %>%
  select(Country, destination_city, arrivals_june_2026)

# ---- 8e. Country-level disease incidence tables --------------
# All four incidence tables (dengue_june_country, malaria_country_inc,
# measles_country_inc, pertussis_country_inc) were computed in §6b,
# along with the disease parameters (under_rho_*, p_travel_inf_*).
# They are shared across §6 (Model 1), §8 (Model 2), and §9 (Model 3)
# to ensure the three-way comparison isolates travel volume differences,
# not incidence assumptions.

# ---- 8f. WC-adjusted model function -------------------------
# compute_importation_country_level() was moved to §5c so it is
# available to all three model tiers (§6, §8, §9). See §5c for the
# full function definition and documentation.

# ---- 8g. WC-adjusted estimates for all five diseases --------
# Parameters are identical to Section 6 to allow direct comparison.

dengue_wc_results <- compute_importation_country_level(
  arrivals_df    = arrivals_country_city_2026,
  country_inc_df = dengue_june_country,
  p_travel_inf   = p_travel_inf_dengue,
  title_text     = "Dengue importation intensity — WC-adjusted (June 2026)"
)
print(dengue_wc_results$importation)
print(dengue_wc_results$plot)

malaria_wc_results <- compute_importation_country_level(
  arrivals_df    = arrivals_country_city_2026,
  country_inc_df = malaria_country_inc,
  p_travel_inf   = p_travel_inf_malaria,
  title_text     = "Malaria importation intensity — WC-adjusted (June 2026)"
)
print(malaria_wc_results$importation)
print(malaria_wc_results$plot)

measles_wc_results <- compute_importation_country_level(
  arrivals_df    = arrivals_country_city_2026,
  country_inc_df = measles_country_inc,
  p_travel_inf   = p_travel_inf_measles,
  title_text     = "Measles importation intensity — WC-adjusted (June 2026)"
)
print(measles_wc_results$importation)
print(measles_wc_results$plot)

pertussis_wc_results <- compute_importation_country_level(
  arrivals_df    = arrivals_country_city_2026,
  country_inc_df = pertussis_country_inc,
  p_travel_inf   = p_travel_inf_pertussis,
  title_text     = "Pertussis importation intensity — WC-adjusted (June 2026)"
)
print(pertussis_wc_results$importation)
print(pertussis_wc_results$plot)

influenza_wc_results <- compute_importation_country_level(
  arrivals_df    = arrivals_country_city_2026,
  country_inc_df = influenza_june_country,
  p_travel_inf   = p_travel_inf_influenza,
  title_text     = "Influenza importation intensity — WC-adjusted (June 2026)"
)
print(influenza_wc_results$importation)
print(influenza_wc_results$plot)

# ---- 8h. Combined WC-adjusted panel -------------------------
panel_wc_adjusted <- plot_grid(
  dengue_wc_results$plot,
  malaria_wc_results$plot,
  measles_wc_results$plot,
  pertussis_wc_results$plot,
  influenza_wc_results$plot,
  ncol = 2,
  labels = "AUTO", label_size = 20
)

ggsave(panel_wc_adjusted,
       file   = "Figures/estimated_importations_wc_adjusted.png",
       height = 21, width = 20)

# ============================================================
# 9. SCHEDULE-DRIVEN VENUE ROUTING MODEL (MODEL 3)
# ============================================================
#
# MOTIVATION
# ----------
# Models 1 and 2 distribute all international arrivals across venue
# cities using T-100 routing fractions — which reflect habitual tourist
# flows. WC fans, however, travel specifically to the cities where their
# team plays. A Brazil supporter flying in for a Houston group-stage
# match will not distribute to Boston or Philadelphia at the same rate
# as a regular tourist. Model 3 captures this by decomposing travel:
#
#   (1) WC-FAN STREAM
#       The marginal increment above the no-WC counterfactual (phi_base):
#         N_c^WC = N_{c,2024}^COR * max(0, phi_c - phi_base)
#       Fans are routed to venue cities in proportion to their team's
#       matches there:  omega_{c,v} = g_{c,v} / G_c
#
#   (2) BACKGROUND STREAM
#       All arrivals that would have occurred without the WC:
#         N_c^bg = N_{c,2026} - N_c^WC
#       These use T-100 country-level routing fractions, consistent
#       with Models 1 and 2.
#
# For NTTO countries where phi_c < phi_base (e.g., UK at 1.107), the
# WC fan increment is zero; all UK travel is treated as background.
#
# CITY COVERAGE
# -------------
# The schedule-driven model covers all 16 host venues (11 US + 5
# Canada/Mexico). All 11 US cities receive contributions from both
# streams (WC fans + T-100 background). The 5 non-US venues receive
# only the WC-fan stream — T-100 covers US airports only.
# ============================================================

# ---- 9a. Parse match schedule --------------------------------
# One row per match → pivot to one row per team per match so both
# participating nations generate fan travel to the same venue.
# TBD entries (unresolved knockout opponents) are dropped because we
# cannot predict which fans will travel for those matches.
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
  # All names must match the exact strings used in the COR arrivals data
  # and the disease incidence tables so joins succeed downstream.
  mutate(team = recode(team,
    "Korea Republic"  = "South Korea",
    "IR Iran"         = "Iran",
    "Cabo Verde"      = "Cape Verde",
    "Cote d'Ivore"    = "Côte d'Ivoire",   # schedule typo: Ivore → Ivoire
    "Cote d'Ivoire"   = "Côte d'Ivoire",
    "Ivory Coast"     = "Côte d'Ivoire",
    "Belguim"         = "Belgium",          # schedule typo: u ↔ i
    # England and Scotland both use "United Kingdom" — the single COR
    # entry for UK residents. Their combined matches are pooled to allocate
    # UK WC fans across all venues where either team plays. Because
    # omega_{UK,v} = g_{UK,v} / G_UK and fractions sum to 1, there is
    # no double-counting of UK arrivals.
    "England"         = "United Kingdom",
    "Scotland"        = "United Kingdom",
    # USA is the host; US citizens are domestic travellers and do not
    # appear in foreign-arrival records — the join intentionally produces
    # no match (no importation risk from the home team).
    "USA"             = "United States"
  )) %>%
  # --- Standardise venue city names ---
  # Map stadium host municipalities to canonical city labels used in
  # Sections 9–11. Suburb/municipality names are mapped to their metro
  # anchor so both streams share the same city-name convention.
  mutate(venue_city = recode(city,
    "East Rutherford"  = "New York",       # MetLife Stadium, NYC metro
    "Foxborough"       = "Boston",          # Gillette Stadium
    "Arlington"        = "Dallas",          # AT&T Stadium
    "Inglewood"        = "Los Angeles",     # SoFi Stadium
    "Santa Clara"      = "San Francisco",   # Levi's Stadium, SF Bay Area
    "Miami Gardens"    = "Miami",           # Hard Rock Stadium
    "Zapopan"          = "Guadalajara",     # Estadio Akron
    "Guadalupe"        = "Monterrey",       # Estadio BBVA
    "Cuidad de Mexico" = "Mexico City"      # Estadio Banorte
    # Atlanta, Houston, Kansas City, Philadelphia, Seattle,
    # Toronto, Vancouver: kept as-is (city name already canonical)
  ))

# Count matches per team × venue city (numerator for omega_{c,v})
team_venue_games <- games_schedule %>%
  group_by(team, venue_city) %>%
  summarise(n_games = n(), .groups = "drop")

# Total games per team (denominator for omega_{c,v})
total_games_per_team <- team_venue_games %>%
  group_by(team) %>%
  summarise(total_games = sum(n_games), .groups = "drop")

# Schedule-based WC fan routing fraction (Eq. 7 in manuscript):
#   omega_{c,v} = g_{c,v} / G_c
schedule_routing <- team_venue_games %>%
  left_join(total_games_per_team, by = "team") %>%
  mutate(wc_routing = n_games / total_games) %>%
  select(team, venue_city, wc_routing)

# ---- 9b. Decompose June 2026 travel into WC-fan vs. background ---
# Uses travel_volume_june_2026 from Section 8b (columns: Country,
# june_2024, growth_factor, june_2026).
#
#   N_c^WC  = june_2024 * max(0, phi_c - phi_base)    [WC increment]
#   N_c^bg  = june_2026 - N_c^WC                      [background; always >=0]
travel_decomposed <- travel_volume_june_2026 %>%
  mutate(
    june_wc = june_2024 * pmax(0, growth_factor - growth_baseline),
    june_bg = june_2026 - june_wc
  ) %>%
  select(Country, World_region, june_wc, june_bg, june_2026)

# ---- 9c. WC-fan stream: country × venue-city arrival matrix ----
# N_{c,v}^WC = june_wc[c] * omega_{c,v}
# Non-qualified countries have no row in schedule_routing, so the
# left_join produces NA → filtered out (correct: no WC fans).
# Results are restricted to the 11 US venue cities; non-US venues
# (Toronto, Vancouver, Mexico City, Monterrey, Guadalajara) are excluded.
us_venue_cities <- c("New York", "Dallas", "Houston", "Philadelphia",
                     "Boston", "Los Angeles", "Atlanta", "Kansas City",
                     "Miami", "San Francisco", "Seattle")

arrivals_wc_venue <- travel_decomposed %>%
  left_join(
    schedule_routing %>% rename(Country = team),
    by = "Country"
  ) %>%
  filter(!is.na(venue_city), venue_city %in% us_venue_cities) %>%
  mutate(arrivals_wc_city = june_wc * wc_routing) %>%
  select(Country, venue_city, arrivals_wc_city)

# ---- 9d. Background stream: country × city arrival matrix (T-100) ---
# Background travellers use T-100 country-level routing fractions (§3c),
# consistent with Models 1 and 2. All 11 US WC venue cities are covered.
# Kansas City shows near-zero background routing for most countries,
# correctly reflecting its limited direct international service; its
# WC traffic is dominated by the fan stream.
arrivals_bg_city <- travel_decomposed %>%
  left_join(t100_routing, by = "Country") %>%
  drop_na(venue_city) %>%
  mutate(arrivals_bg = june_bg * routing_fraction) %>%
  select(Country, venue_city, arrivals_bg)

# ---- 9e. Schedule-driven importation model function ----------
#
# Implements Eq. 9 in the manuscript. Processes both travel streams
# through the Poisson framework and combines them:
#
#   Lambda[v, d] = sum_{c in Q} (arrivals_wc[c,v] * I_{c,d} * p_d)   [WC fans]
#               + sum_c         (arrivals_bg[c,h]  * I_{c,d} * p_d)   [background]
#
# Also returns per-country contributions (needed by Section 11).
#
# Arguments:
#   arrivals_wc_df  — Country, venue_city, arrivals_wc_city
#   arrivals_bg_df  — Country, venue_city, arrivals_bg
#   country_inc_df  — Country, total_inc
#   p_travel_inf    — scalar p_d
#
# Returns:
#   $importation           — city-level Lambda and P(>=1)
#   $country_contributions — per-country, per-city, per-stream lambda
#   $plot                  — bar chart
compute_importation_schedule <- function(arrivals_wc_df,
                                         arrivals_bg_df,
                                         country_inc_df,
                                         p_travel_inf = 1,
                                         title_text   = "Importation intensity (schedule-driven)") {

  wc_stream <- arrivals_wc_df %>%
    left_join(country_inc_df, by = "Country") %>%
    drop_na(total_inc) %>%
    mutate(
      expected_imports = arrivals_wc_city * total_inc * p_travel_inf,
      stream           = "WC fans"
    ) %>%
    rename(city = venue_city) %>%
    select(Country, city, expected_imports, stream)

  bg_stream <- arrivals_bg_df %>%
    left_join(country_inc_df, by = "Country") %>%
    drop_na(total_inc) %>%
    mutate(
      expected_imports = arrivals_bg * total_inc * p_travel_inf,
      stream           = "Background"
    ) %>%
    rename(city = venue_city) %>%
    select(Country, city, expected_imports, stream)

  combined <- bind_rows(wc_stream, bg_stream)

  importation_df <- combined %>%
    group_by(city) %>%
    summarise(imp_intensity = sum(expected_imports, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      prob_at_least_one = 1 - exp(-imp_intensity),
      destination_city  = city   # plot_importation() expects this column name
    )

  country_contributions <- combined %>%
    group_by(Country, city, stream) %>%
    summarise(expected_imports = sum(expected_imports, na.rm = TRUE), .groups = "drop")

  list(
    importation           = importation_df,
    country_contributions = country_contributions,
    plot                  = plot_importation(importation_df, title_text)
  )
}

# ---- 9f. Schedule-driven estimates for all five diseases -----
# Parameters identical to Sections 6 and 8 for direct comparability.

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

influenza_sched_results <- compute_importation_schedule(
  arrivals_wc_df  = arrivals_wc_venue,
  arrivals_bg_df  = arrivals_bg_city,
  country_inc_df  = influenza_june_country,
  p_travel_inf    = p_travel_inf_influenza,
  title_text      = "Influenza importation intensity — schedule-driven (June 2026)"
)

print(dengue_sched_results$importation)
print(dengue_sched_results$plot)

# ---- 9g. Combined schedule-driven panel ----------------------
panel_schedule_driven <- plot_grid(
  dengue_sched_results$plot,
  malaria_sched_results$plot,
  measles_sched_results$plot,
  pertussis_sched_results$plot,
  influenza_sched_results$plot,
  ncol = 2,
  labels = "AUTO", label_size = 20
)

ggsave(panel_schedule_driven,
       file   = "Figures/estimated_importations_schedule_driven.png",
       height = 21, width = 20)

# ============================================================
# 10. THREE-MODEL COMPARISON
# ============================================================
#
# The three models form a nested hierarchy where each step adds one
# layer of resolution, while holding disease parameters fixed:
#
#   Model 1 → Model 2: effect of WC travel surge (phi_c growth factors
#                       applied to COR June 2024 base volumes)
#   Model 2 → Model 3: effect of schedule-based fan routing
#                       (WC fans directed to specific match venues)
#
# All three models use T-100 routing fractions and country-level
# incidence, so the comparisons isolate travel volume differences.
#
# Two metrics are compared (see Sections 10c and 10d):
#   P(>=1): useful for communication but saturates to 1 at high risk
#   Lambda: stays on a linear scale; quantitatively more informative
# ============================================================

# ============================================================
# ===== BEGIN: recode_i92_city() — PRESERVED FOR REFERENCE =====
# ============================================================
# This helper was used when Model 1 (baseline) relied on I-92 city
# names ("Ny", "Newark"). Now that all three models use T-100 canonical
# venue city names (str_to_title(destination_city) is sufficient),
# this function is no longer needed but is kept for reference.

# recode_i92_city <- function(df) {
#   df %>%
#     mutate(city = case_when(
#       destination_city == "Newark"       ~ "New York",
#       destination_city == "Ny"           ~ "New York",
#       destination_city == "Boston"       ~ "Boston",
#       destination_city == "Dallas"       ~ "Dallas",
#       destination_city == "Houston"      ~ "Houston",
#       destination_city == "Philadelphia" ~ "Philadelphia",
#       TRUE                               ~ str_to_title(destination_city)
#     )) %>%
#     group_by(city) %>%
#     summarise(imp_intensity = sum(imp_intensity, na.rm = TRUE), .groups = "drop") %>%
#     mutate(prob_at_least_one = 1 - exp(-imp_intensity))
# }
# ============================================================
# ===== END: recode_i92_city() =====
# ============================================================

# ---- 10b. Assemble the three-model comparison table ----------
# All three models now use T-100 canonical venue city names, so no
# special recoding is needed for the baseline. The same str_to_title()
# + select() pattern is applied uniformly across all three tiers.
build_comparison <- function(baseline_res, wc_res, sched_res, disease_name) {
  bind_rows(
    # Model 1: T-100 baseline — canonical venue city names
    baseline_res$importation %>%
      mutate(city = str_to_title(destination_city)) %>%
      select(city, imp_intensity, prob_at_least_one) %>%
      mutate(model = "Baseline"),
    # Model 2: WC-adjusted — same T-100 canonical names
    wc_res$importation %>%
      mutate(city = str_to_title(destination_city)) %>%
      select(city, imp_intensity, prob_at_least_one) %>%
      mutate(model = "WC-adjusted"),
    # Model 3: schedule-driven — city column already canonical
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
  build_comparison(pertussis_results, pertussis_wc_results, pertussis_sched_results, "Pertussis"),
  build_comparison(influenza_results, influenza_wc_results, influenza_sched_results, "Influenza")
)

# Consistent colour palette across all comparison plots
model_colors <- c(
  "Baseline"        = "#4393c3",
  "WC-adjusted"     = "#d6604d",
  "Schedule-driven" = "#74c476"
)

# Order x-axis by descending schedule-driven total importation intensity
# across all diseases — most-at-risk cities appear first.
city_order <- comparison_all %>%
  filter(model == "Schedule-driven") %>%
  group_by(city) %>%
  summarise(total_imp = sum(imp_intensity, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_imp)) %>%
  pull(city)

# ---- 10c. Comparison plot: P(>=1) by city and model ----------
# All three models cover the same 11 US venue cities via T-100 routing.
# Model 3 adds 5 non-US venues (Toronto, Vancouver, Guadalajara,
# Mexico City, Monterrey) that show non-zero values for Model 3 only.
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
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 18),
    text         = element_text(size = 20),
    legend.position = "bottom"
  )

ggsave(prob_comparison_plot,
       file   = "Figures/model_comparison_probability.png",
       height = 12, width = 18)

# ---- 10d. Comparison plot: expected importation intensity (lambda) --
# Lambda remains informative when P(>=1) → 1 (it does not saturate),
# making it the preferred metric for quantifying relative risk between
# high-burden cities and across models.
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
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 18),
    text         = element_text(size = 20),
    legend.position = "bottom"
  )

ggsave(intensity_comparison_plot,
       file   = "Figures/model_comparison_intensity.png",
       height = 12, width = 18)

# ============================================================
# 11. COUNTRY-LEVEL IMPORTATION CONTRIBUTIONS
# ============================================================
#
# The Poisson model decomposes additively:
#   Lambda[v, d] = sum_c lambda[c, v, d]
#
# compute_importation_schedule() stores per-country, per-city,
# per-stream contributions in $country_contributions. This section
# uses those to produce three summaries (Figs 5–8 in manuscript):
#
#   (a) Country importation ranking — top 15 by total lambda
#   (b) WC-fan vs. background stream breakdown — all five diseases
#   (c) Country × city heatmaps — all five diseases
# ============================================================

# ---- 11a. Aggregate contributions across all five diseases ---
all_contributions <- bind_rows(
  dengue_sched_results$country_contributions    %>% mutate(disease = "Dengue"),
  malaria_sched_results$country_contributions   %>% mutate(disease = "Malaria"),
  measles_sched_results$country_contributions   %>% mutate(disease = "Measles"),
  pertussis_sched_results$country_contributions %>% mutate(disease = "Pertussis"),
  influenza_sched_results$country_contributions %>% mutate(disease = "Influenza")
)

# ---- 11b. Country importation ranking ------------------------
# Rank by total expected importations summed across all destination
# cities, separately for each disease. Top 15 per disease are shown.
top_countries <- all_contributions %>%
  group_by(disease, Country) %>%
  summarise(total_imports = sum(expected_imports, na.rm = TRUE), .groups = "drop") %>%
  group_by(disease) %>%
  slice_max(total_imports, n = 15, with_ties = FALSE) %>%
  ungroup()

country_ranking_plot <- top_countries %>%
  mutate(disease = factor(disease, levels = c("Dengue","Malaria","Measles","Pertussis","Influenza"))) %>%
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
  theme(text = element_text(size = 20))

ggsave(country_ranking_plot,
       file   = "Figures/country_importation_ranking.png",
       height = 12, width = 16)

# ---- 11c. WC-fan vs. background stream breakdown (all diseases) --
# For each disease, shows the fraction of importation risk attributable
# to WC-specific fan travel vs. routine background tourism for the
# top 20 contributing countries.
# WC-qualified countries show a non-zero WC-fan component; non-qualified
# countries show background travel only.
#
# NOTE: Previously this section covered dengue only. Extended here to
# all five diseases to provide a complete picture of stream attribution.

make_stream_plot <- function(disease_name, n_top = 20) {

  # Identify the top-n countries for this disease
  top_c <- all_contributions %>%
    filter(disease == disease_name) %>%
    group_by(Country) %>%
    summarise(s = sum(expected_imports), .groups = "drop") %>%
    slice_max(s, n = n_top, with_ties = FALSE) %>%
    pull(Country)

  stream_data <- all_contributions %>%
    filter(disease == disease_name, Country %in% top_c) %>%
    group_by(Country, stream) %>%
    summarise(total_imports = sum(expected_imports, na.rm = TRUE), .groups = "drop") %>%
    group_by(Country) %>%
    mutate(country_total = sum(total_imports)) %>%
    ungroup()

  stream_data %>%
    ggplot(aes(x = reorder(Country, country_total),
               y = total_imports,
               fill = stream)) +
    geom_col() +
    coord_flip() +
    scale_fill_manual(values = c("WC fans" = "#e6550d", "Background" = "#3182bd")) +
    labs(
      x     = "",
      y     = expression(lambda ~ "(expected importations)"),
      fill  = "Travel stream",
      title = paste0(disease_name,
                     ": importation by travel stream — top ", n_top, " source countries")
    ) +
    theme_bw() +
    theme(text = element_text(size = 15),legend.position = c(0.7,0.3))
}

stream_plot_dengue    <- make_stream_plot("Dengue")
stream_plot_malaria   <- make_stream_plot("Malaria")
stream_plot_measles   <- make_stream_plot("Measles")
stream_plot_pertussis <- make_stream_plot("Pertussis")
stream_plot_influenza <- make_stream_plot("Influenza")

ggsave(stream_plot_dengue,
       file = "Figures/stream_breakdown_dengue.png",    height = 9, width = 13)
ggsave(stream_plot_malaria,
       file = "Figures/stream_breakdown_malaria.png",   height = 9, width = 13)
ggsave(stream_plot_measles,
       file = "Figures/stream_breakdown_measles.png",   height = 9, width = 13)
ggsave(stream_plot_pertussis,
       file = "Figures/stream_breakdown_pertussis.png", height = 9, width = 13)
ggsave(stream_plot_influenza,
       file = "Figures/stream_breakdown_influenza.png", height = 9, width = 13)

# Combined stream panel for the supplement
panel_stream <- plot_grid(
  stream_plot_dengue, stream_plot_malaria,
  stream_plot_measles, stream_plot_pertussis,
  stream_plot_influenza,
  ncol = 2,
  labels = "AUTO", label_size = 16
)
ggsave(panel_stream,
       file   = "Figures/stream_breakdown_panel.png",
       height = 20, width = 22)

# ---- 11d. Country × city importation heatmaps ---------------
# A matrix of expected importations by source country (rows, ordered
# by total burden) and destination city (columns). WC-qualified
# countries show risk concentrated at specific match venues; non-WC
# countries show diffuse contributions spread across I-92 gateways.
make_heatmap <- function(disease_name, n_countries = 20) {

  dat <- all_contributions %>%
    filter(disease == disease_name) %>%
    group_by(Country, city) %>%
    summarise(imports = sum(expected_imports, na.rm = TRUE), .groups = "drop")

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
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      text        = element_text(size = 14)
    )
}

heatmap_dengue    <- make_heatmap("Dengue")
heatmap_malaria   <- make_heatmap("Malaria")
heatmap_measles   <- make_heatmap("Measles")
heatmap_pertussis <- make_heatmap("Pertussis")
heatmap_influenza <- make_heatmap("Influenza")

ggsave(heatmap_dengue,    file = "Figures/heatmap_dengue.png",    height = 9, width = 13)
ggsave(heatmap_malaria,   file = "Figures/heatmap_malaria.png",   height = 9, width = 13)
ggsave(heatmap_measles,   file = "Figures/heatmap_measles.png",   height = 9, width = 13)
ggsave(heatmap_pertussis, file = "Figures/heatmap_pertussis.png", height = 9, width = 13)
ggsave(heatmap_influenza, file = "Figures/heatmap_influenza.png", height = 9, width = 13)

# ============================================================
# 12. SENSITIVITY ANALYSIS
# ============================================================
#
# rho_d and p_d are the two most uncertain parameters in the model.
# Neither has strong empirical constraints for a mass sporting-event
# context. This section varies each parameter independently over a
# ±50 % range around its central estimate while holding the other
# fixed, using the schedule-driven model (Section 9) as the reference.
#
# For each combination (disease × parameter × multiplier):
#   - Recompute total_inc with the perturbed parameter
#   - Call compute_importation_schedule()
#   - Extract city-level Lambda values
#
# Output: a tidy data frame (sensitivity_results) and a faceted plot
# showing how Lambda at each city responds to parameter uncertainty.
# Relative sensitivity (ratio of perturbed to central Lambda) is also
# computed to identify which cities and diseases are most affected.
# ============================================================

# Multipliers representing ±50 % of the central value (6 levels)
sensitivity_multipliers <- c(0.50, 0.75, 1.00, 1.25, 1.50)

# Central parameters (must match Sections 6 and 9)
central_params <- tibble(
  disease       = c("Dengue",            "Malaria",            "Measles",            "Pertussis",            "Influenza"),
  under_rho     = c(under_rho_dengue,    under_rho_malaria,    under_rho_measles,    under_rho_pertussis,    under_rho_influenza),
  p_travel      = c(p_travel_inf_dengue, p_travel_inf_malaria, p_travel_inf_measles, p_travel_inf_pertussis, p_travel_inf_influenza),
  country_inc   = list(dengue_june_country, malaria_country_inc, measles_country_inc, pertussis_country_inc, influenza_june_country)
)

# Helper: run schedule-driven model with a scaled rho or p
run_sensitivity <- function(disease_name, base_inc_df, base_rho, base_p,
                             param, multiplier) {

  if (param == "rho") {
    # Scale total_inc proportionally to rho (linear relationship)
    scaled_inc <- base_inc_df %>%
      mutate(total_inc = total_inc * multiplier)
    p_use <- base_p
  } else {
    scaled_inc <- base_inc_df
    p_use      <- base_p * multiplier
  }

  res <- compute_importation_schedule(
    arrivals_wc_df  = arrivals_wc_venue,
    arrivals_bg_df  = arrivals_bg_city,
    country_inc_df  = scaled_inc,
    p_travel_inf    = p_use,
    title_text      = ""
  )

  res$importation %>%
    select(city, imp_intensity) %>%
    mutate(
      disease    = disease_name,
      param      = param,
      multiplier = multiplier
    )
}

# Run all combinations: 5 diseases × 2 parameters × 5 multipliers = 50 runs
sensitivity_results <- pmap_dfr(
  crossing(
    central_params %>% select(disease, under_rho, p_travel, country_inc),
    param      = c("rho", "p"),
    multiplier = sensitivity_multipliers
  ),
  function(disease, under_rho, p_travel, country_inc, param, multiplier) {
    run_sensitivity(
      disease_name = disease,
      base_inc_df  = country_inc,
      base_rho     = under_rho,
      base_p       = p_travel,
      param        = param,
      multiplier   = multiplier
    )
  }
)

# Add P(>=1) alongside Lambda for every run
sensitivity_results <- sensitivity_results %>%
  mutate(prob = 1 - exp(-imp_intensity))

# Compute relative Lambda and relative P(>=1): perturbed / central
central_vals <- sensitivity_results %>%
  filter(multiplier == 1.00) %>%
  select(disease, param, city,
         lambda_central = imp_intensity,
         prob_central   = prob)

sensitivity_relative <- sensitivity_results %>%
  left_join(central_vals, by = c("disease", "param", "city")) %>%
  mutate(
    relative_lambda = imp_intensity / lambda_central,
    # Use absolute difference for prob so near-saturated cities don't
    # collapse to ratio ≈ 1 artificially; keep ratio too for reference
    relative_prob   = prob / prob_central,
    delta_prob      = prob - prob_central
  )

# ---- Sensitivity plot: absolute Lambda ----------------------
sensitivity_plot_abs <- sensitivity_results %>%
  mutate(
    disease    = factor(disease, levels = c("Dengue","Malaria","Measles","Pertussis","Influenza")),
    param_lab  = if_else(param == "rho",
                          "Varying ρ (under-reporting factor)",
                          "Varying p (travel-while-infectious probability)"),
    multiplier = factor(multiplier)
  ) %>%
  ggplot(aes(x = reorder(city, -imp_intensity), y = imp_intensity,
             color = multiplier, group = multiplier)) +
  geom_line() + geom_point(size = 2) +
  facet_grid(disease ~ param_lab, scales = "free_y") +
  scale_color_brewer(palette = "RdYlBu", name = "Multiplier\n(× central)") +
  labs(
    x     = "Destination city",
    y     = expression(lambda ~ "(expected importations)"),
    title = "Sensitivity of importation intensity to parameter uncertainty (±50%)"
  ) +
  theme_bw() +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 12),
    text            = element_text(size = 15),
    legend.position = "right"
  )

ggsave(sensitivity_plot_abs,
       file   = "Figures/sensitivity_analysis_lambda.png",
       height = 17, width = 18)

# ---- Sensitivity plot: relative P(>=1) ----------------------
# Plotting relative Lambda is uninformative here because both rho
# and p enter Lambda linearly, so Lambda_perturbed / Lambda_central
# = multiplier for every city (flat lines). The Poisson probability
# P(>=1) = 1 - exp(-Lambda) is nonlinear, so its relative change
# *does* vary across cities: high-Lambda (near-saturated) cities are
# insensitive to perturbations; low-Lambda cities show large shifts.
# We show BOTH the ratio P_perturbed/P_central (relative) and the
# absolute shift delta_P = P_perturbed - P_central in two panels.

# --- Panel A: ratio P(>=1)_perturbed / P(>=1)_central -----------
sensitivity_plot_rel <- sensitivity_relative %>%
  filter(multiplier != 1.00) %>%
  mutate(
    disease    = factor(disease, levels = c("Dengue","Malaria","Measles","Pertussis","Influenza")),
    param_lab  = if_else(param == "rho", "Varying ρ", "Varying p"),
    multiplier = factor(multiplier)
  ) %>%
  ggplot(aes(x = reorder(city, -relative_prob), y = relative_prob,
             color = multiplier, group = multiplier)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  geom_line() + geom_point(size = 2) +
  facet_grid(disease ~ param_lab, scales = "free_y") +
  scale_color_brewer(palette = "RdYlBu", name = "Multiplier\n(× central)") +
  labs(
    x     = "Destination city",
    y     = expression(P("">=1)[perturbed] / P("">=1)[central]),
    title = "Relative sensitivity of P(≥1 importation) to parameter uncertainty"
  ) +
  theme_bw() +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 12),
    text            = element_text(size = 15),
    legend.position = "right"
  )

ggsave(sensitivity_plot_rel,
       file   = "Figures/sensitivity_analysis_relative.png",
       height = 17, width = 18)

# --- Panel B: absolute shift delta_P = P_perturbed - P_central ---
sensitivity_plot_delta <- sensitivity_relative %>%
  filter(multiplier != 1.00) %>%
  mutate(
    disease    = factor(disease, levels = c("Dengue","Malaria","Measles","Pertussis","Influenza")),
    param_lab  = if_else(param == "rho", "Varying ρ", "Varying p"),
    multiplier = factor(multiplier)
  ) %>%
  ggplot(aes(x = reorder(city, -abs(delta_prob)), y = delta_prob,
             color = multiplier, group = multiplier)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line() + geom_point(size = 2) +
  facet_grid(disease ~ param_lab, scales = "free_y") +
  scale_color_brewer(palette = "RdYlBu", name = "Multiplier\n(× central)") +
  labs(
    x     = "Destination city",
    y     = expression(Delta * P("">=1)),
    title = "Absolute change in P(≥1 importation) under parameter uncertainty"
  ) +
  theme_bw() +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 12),
    text            = element_text(size = 15),
    legend.position = "right"
  )

ggsave(sensitivity_plot_delta,
       file   = "Figures/sensitivity_analysis_delta_prob.png",
       height = 17, width = 18)

# Summary table: range of P(>=1) and delta_P across multipliers
sensitivity_summary <- sensitivity_relative %>%
  filter(multiplier != 1.00) %>%
  group_by(disease, city, param) %>%
  summarise(
    prob_central   = first(prob_central),
    prob_min       = min(prob,       na.rm = TRUE),
    prob_max       = max(prob,       na.rm = TRUE),
    delta_prob_min = min(delta_prob, na.rm = TRUE),
    delta_prob_max = max(delta_prob, na.rm = TRUE),
    .groups        = "drop"
  ) %>%
  arrange(disease, param, desc(prob_central))

print(sensitivity_summary)
