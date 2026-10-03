# ============================================================
# FIFA World Cup 2026 — Infectious Disease Importation Risk
# Main model with Monte Carlo uncertainty intervals
# Author : Jose Herrera-Diestra
#
# OVERVIEW
# --------
# Estimates expected imported infections (Lambda) and the probability
# of at least one imported infection, P(>=1), for dengue, malaria,
# measles, pertussis, and influenza at the 11 US host cities during
# June 2026. Model tiers:
#
#   M2 — WC-adjusted: June 2026 projected arrivals (COR June 2024 x
#        growth factors), routed to cities by BTS T-100 fractions.
#   M3 — Schedule-driven: the same arrivals split into a background
#        stream (routed by T-100) and a World Cup fan stream (routed by
#        the group-stage match schedule).
#   M1 — Baseline, 2026 travel without the World Cup: the M3
#        background stream routed by T-100, computed in
#        model1_baseline_2026.R from objects created here.
#
# Importation model (Eq. 1 in the manuscript):
#   lambda_{c,v,d} = N_{c,v} * C_{c,d} / (P_c * rho_d) * p_d
#   Lambda_{v,d}   = sum_c lambda_{c,v,d},   P(>=1) = 1 - exp(-Lambda)
# N = arrivals from country c to city v, C = reported cases,
# P = population, rho_d = reporting fraction, and p_d = D_d / T_d, the
# probability that an infected person travels while infected (5g).
#
# Uncertainty: 5,000 Monte Carlo draws from Uniform ranges of rho_d and
# p_d (Table 1); results are the median and 95% uncertainty interval.
#
# Sections:
#   0.  Packages
#   1.  Working directory
#   2.  Reference data (population, COR arrivals, venue map = Figure S4)
#   3.  Travel data (COR June 2024, T-100 routing fractions)
#   4.  Disease data
#   5.  Model functions, Monte Carlo settings, p_d (5g)
#   6.  rho_d and country-level incidence tables
#   7.  Shared plot aesthetics
#   8.  M2 — WC-adjusted model
#   9.  M3 — Schedule-driven model
#  11.  Country-level contributions; Figure S3 (CI asymmetry)
#  12.  Figures 1, 2, 4 and S2 (Figures 3 and S1: figure3_figureS1.R)
#  13.  Save outputs for downstream scripts
#
# Run order for the full pipeline: see README.md.
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
# Run from the repository root: open FIFA_worldCup_2026_risk.Rproj in
# RStudio (or setwd() to the folder containing Code/, Data/, Figures/).
if (!dir.exists("Data") || !dir.exists("Code")) {
  stop("Working directory must be the repository root ",
       "(the folder containing Code/, Data/ and Figures/). ",
       "Open FIFA_worldCup_2026_risk.Rproj in RStudio, or use setwd().")
}


# ============================================================
# 2. REFERENCE DATA
# ============================================================

# --- 2a. Country populations (2026 Worldometers projections) ---
# Denominator P_c in Eq. 1.
# Source: Worldometers, population by country (2026), accessed June 2026.
population_of_world <- read_csv("Data/population2026.csv") %>%
  rename(Country = COUNTRY, population_country = POPULATION) %>%
  # Harmonise the DRC name to match the disease and arrivals datasets.
  # The COR dataset uses the older "Zaire" convention; we propagate
  # that throughout to avoid broken joins downstream.
  mutate(Country = if_else(Country == "DR Congo", "Zaire (formerly DRC)", Country))

# --- 2b. Monthly arrivals by Country of Residence (COR/I-94) --
# Source: CBP I-94 Monthly Arrivals by Country of Residence
# (https://travel.trade.gov). Used to (i) build the country-to-
# region correspondence table and (ii) extract June 2024 volumes
# for the M2 and M3 models (Section 8).
arrivals_COR <- read_csv("Data/Monthly_Arrivals_Country_of_Residence_COR_1.csv")

# --- 2c. Country → broad world region mapping ---------------
# Derived from the COR dataset (which carries a World_region field).
# Two structural choices:
#   (1) Western Europe + Eastern Europe → "Europe": keeps region counts
#       manageable.
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

# --- 2e. WC 2026 venue map (Figure S4) ----------------------
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
    seed          = 2026,   # fixed label layout across reruns
    show.legend   = FALSE
  ) +
  coord_fixed(xlim = c(-130, -60), ylim = c(14, 57), ratio = 1.3) +
  scale_color_manual(
    name   = "Host nation",
    values = c("USA" = "#1a6faf", "Canada" = "#c0392b", "Mexico" = "#27ae60")
  ) +
  labs(x = NULL, y = NULL) +
  # No title/subtitle baked into the plot: explanatory text belongs in
  # the LaTeX caption, not duplicated in the image itself.
  theme_bw() +
  theme(
    panel.grid      = element_blank(),
    axis.text       = element_blank(),
    axis.ticks      = element_blank(),
    panel.border    = element_rect(color = "gray70"),
    legend.position = "bottom"
  )

ggsave(venue_map,
       file   = "Figures/FigureS4.png",
       height = 8, width = 12, dpi = 300)
# Vector PDF for journal submission (Elsevier line-art requirement)
ggsave(venue_map,
       file   = "Figures/FigureS4.pdf",
       height = 8, width = 12, device = grDevices::quartz, type = "pdf")  # macOS native vector PDF (cairo_pdf needs XQuartz)
print(venue_map)

# ============================================================
# 3. TRAVEL DATA — COR JUNE 2024 + T-100 ROUTING FRACTIONS
# ============================================================
# Country-level arrivals come from the CBP I-94 Country of Residence
# (COR) series; June 2024 is the base year for all growth factors.
# Arrivals are allocated to US cities with BTS T-100 International
# Segment routing fractions: nonstop passengers from country c landing
# at city v, pooled over June 2023–2025, divided by passengers from c
# landing at any US airport (Appendix A.1–A.2):
#   f_{c,v} = sum_y N_{c,v,y} / sum_y N_{c,US,y}
# ============================================================


# ---- 3b. COR June 2024 country-level arrivals ---------------
# Extract June 2024 from the COR/I-94 dataset: the base year for the
# growth factors applied in Section 8.
cor_june_2024 <- arrivals_COR %>%
  select(Country, World_region, `2024-06`) %>%
  mutate(june_2024 = readr::parse_number(as.character(`2024-06`))) %>%
  select(Country, World_region, june_2024) %>%
  drop_na()

# ---- 3c. T-100 country-level routing fractions --------------
# Built by Code/t100_routing_prep.R. T-100 records the US entry airport
# of each international segment: a traveller flying São Paulo → Miami →
# Kansas City is counted at Miami. Kansas City therefore has near-zero
# background routing for most countries; its World Cup traffic comes
# from the fan stream in M3.
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
# Data are monthly country-level reported case counts; June 2024 and
# June 2025 are averaged (Section 6b).
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

# ---- 4b. Malaria (annual incidence per 1,000, 2024) --------
# Source: WHO Global Malaria Programme — National Unit Data.
# Metric used: "Incidence Rate" (cases per 1,000 population, 2024).
# Divided by 12 in Section 6b to approximate a monthly rate.
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

# ---- 5c. Poisson importation model (country level) ----------
# Used for M2 (Section 8), for M1 in model1_baseline_2026.R,
# and by the evaluation scripts:
#   lambda[c, v] = N_{c,v} * I_{c,d} * p_d,   I_{c,d} = C_{c,d} / (P_c * rho_d)
#   Lambda[v]    = sum_c lambda[c, v]
#   P(>=1)       = 1 - exp(-Lambda[v])
#
# Arguments:
#   arrivals_df    — Country, destination_city, arrivals_june_2026
#                    (column name shared by every caller, whatever the period)
#   country_inc_df — Country, total_inc (= reported incidence / rho_d)
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

# ---- 5d. Monte Carlo uncertainty helper ----------------------
#
# Given a vector of central Lambda values (one per city) and the
# literature ranges for rho and p, draws n_mc (rho, p) pairs,
# rescales Lambda for each draw, and returns median + 95% uncertainty
# intervals.
#
# Because Lambda = (p / rho) × (constant city factor), the MC reduces
# to a scalar rescaling: Lambda_i = (p_i/rho_i)/(p_c/rho_c) × Lambda_c.
# This is implemented as a vectorised outer product — no loop needed.
#
# Arguments:
#   lambda_central_vec — numeric vector of central Lambda per city
#   city_names         — character vector matching lambda_central_vec
#   rho_c, p_c         — central values (used as scale denominators)
#   rho_min, rho_max   — literature range for rho
#   p_min, p_max       — literature range for p
#   n_mc               — number of MC draws (default: 5,000)
#
# Returns a tibble: destination_city, lambda_median/lo/hi, prob_median/lo/hi

compute_mc_summary <- function(lambda_central_vec, city_names,
                                rho_c, p_c,
                                rho_min, rho_max, p_min, p_max,
                                n_mc = 5000) {

  # Common random numbers: every call for the same disease (identified by
  # its rho/p ranges) reuses the same fixed seed, so M1, M2, M3 and the
  # validation runs share identical draws. This makes results independent
  # of script run order, and makes tier comparisons (e.g. M3 vs M1) exact,
  # since Lambda scales by the same p/rho factor in every tier.
  # Seeds follow from the ranges in Section 5g (e.g. dengue 133056);
  # the annual evaluation ranges get their own fixed seeds.
  mc_seed <- 2026 + 7 * round(1e4 * rho_min) + 11 * round(1e4 * rho_max) +
                   13 * round(1e4 * p_min)   + 17 * round(1e4 * p_max)
  set.seed(mc_seed)

  rho_draws <- runif(n_mc, rho_min, rho_max)
  p_draws   <- runif(n_mc, p_min,   p_max)

  scales   <- (p_draws / rho_draws) / (p_c / rho_c)
  imp_mat  <- outer(scales, lambda_central_vec)   # n_mc × n_cities
  prob_mat <- 1 - exp(-imp_mat)

  tibble(
    destination_city = city_names,
    lambda_median    = apply(imp_mat,  2, median),
    lambda_lo        = apply(imp_mat,  2, quantile, probs = 0.025),
    lambda_hi        = apply(imp_mat,  2, quantile, probs = 0.975),
    prob_median      = apply(prob_mat, 2, median),
    prob_lo          = apply(prob_mat, 2, quantile, probs = 0.025),
    prob_hi          = apply(prob_mat, 2, quantile, probs = 0.975)
  )
}

# ---- 5f. MC global settings ----------------------------------
set.seed(2026)
n_mc <- 5000

# ---- 5g. p_d as a travel-window share ------------------------
# p_d = D_d / T_d is the probability that a person infected during the
# incidence reporting period is still infected AND able to travel at
# the moment they travel. D_d is the travel-eligible window (days):
# lower bound = incubation period (every infected person can travel
# while incubating); upper bound = incubation + duration of infection
# (as if every infection stayed mild enough to travel through).
# T_d is the length of the period over which incidence C is counted.
#   dengue    6-13 d : intrinsic incubation mean 5.9 d, 95% 3-10 d
#                      (Chan & Johansson 2012, PLoS ONE 7:e50972) +
#                      viremia ~7 d (CDC Yellow Book, Dengue)
#   malaria   6-30 d : P. falciparum incubation 6-30 d (CDC Yellow Book,
#                      Post-Travel Evaluation). Restricted to the incubation window,
#                      i.e. infections that can become clinical after
#                      arrival; chronic asymptomatic carriage lasting
#                      hundreds of days (Ashley & White 2014, Malar J
#                      13:500) is deliberately excluded.
#   measles  11-14 d : 11-12 d from exposure to prodrome, rash ~14 d
#                      after exposure (CDC Yellow Book, Measles)
#   pertussis 7-24 d : incubation 7-10 d (range 4-21) + catarrhal stage
#                      1-2 weeks (CDC Pink Book, Pertussis)
#   influenza 1.4-6.2 d : incubation median 1.4 d, influenza A (Lessler
#                      et al. 2009, Lancet Infect Dis) + viral shedding
#                      4.80 d (Carrat et al. 2008, Am J Epidemiol)
# Periods: June = 365.25/12 d; influenza June = 35 d (FluNet ISO weeks
# 22-26 summed); annual evaluation runs (measles, malaria, pertussis
# 2022) = 365.25 d. Central p_d = midpoint of its range.
days_june     <- 365.25 / 12
days_flu_june <- 35
days_year     <- 365.25

D_min_dengue    <- 6;   D_max_dengue    <- 13
D_min_malaria   <- 6;   D_max_malaria   <- 30
D_min_measles   <- 11;  D_max_measles   <- 14
D_min_pertussis <- 7;   D_max_pertussis <- 24
D_min_influenza <- 1.4; D_max_influenza <- 6.2

# June model (all tiers) and the monthly dengue evaluation
p_min_dengue    <- D_min_dengue    / days_june;     p_max_dengue    <- D_max_dengue    / days_june
p_min_malaria   <- D_min_malaria   / days_june;     p_max_malaria   <- D_max_malaria   / days_june
p_min_measles   <- D_min_measles   / days_june;     p_max_measles   <- D_max_measles   / days_june
p_min_pertussis <- D_min_pertussis / days_june;     p_max_pertussis <- D_max_pertussis / days_june
p_min_influenza <- D_min_influenza / days_flu_june; p_max_influenza <- D_max_influenza / days_flu_june

p_travel_inf_dengue    <- (p_min_dengue    + p_max_dengue)    / 2
p_travel_inf_malaria   <- (p_min_malaria   + p_max_malaria)   / 2
p_travel_inf_measles   <- (p_min_measles   + p_max_measles)   / 2
p_travel_inf_pertussis <- (p_min_pertussis + p_max_pertussis) / 2
p_travel_inf_influenza <- (p_min_influenza + p_max_influenza) / 2

# Annual evaluation runs (annual arrivals x annual incidence)
p_min_malaria_annual   <- D_min_malaria   / days_year; p_max_malaria_annual   <- D_max_malaria   / days_year
p_min_measles_annual   <- D_min_measles   / days_year; p_max_measles_annual   <- D_max_measles   / days_year
p_min_pertussis_annual <- D_min_pertussis / days_year; p_max_pertussis_annual <- D_max_pertussis / days_year
p_travel_inf_malaria_annual   <- (p_min_malaria_annual   + p_max_malaria_annual)   / 2
p_travel_inf_measles_annual   <- (p_min_measles_annual   + p_max_measles_annual)   / 2
p_travel_inf_pertussis_annual <- (p_min_pertussis_annual + p_max_pertussis_annual) / 2

# Parameter ranges (Uniform bounds; see Table 1)
mc_ranges <- tribble(
  ~disease,    ~rho_min, ~rho_max, ~p_min,          ~p_max,
  "Dengue",    0.06,     0.26,     p_min_dengue,    p_max_dengue,
  "Malaria",   0.10,     0.35,     p_min_malaria,   p_max_malaria,
  "Measles",   0.40,     0.80,     p_min_measles,   p_max_measles,
  "Pertussis", 0.01,     0.10,     p_min_pertussis, p_max_pertussis,
  "Influenza", 0.01,     0.10,     p_min_influenza, p_max_influenza
)

# ============================================================
# 6. rho_d AND COUNTRY-LEVEL INCIDENCE TABLES
# ============================================================
# ---- 6a. Central rho_d values --------------------------------
# The central values set the deterministic Lambda that the Monte Carlo
# rescales. Each draw is scaled by (p/rho)/(p_c/rho_c), so reported
# medians and intervals depend only on the Uniform ranges in mc_ranges
# (Table 1), not on these central values. p_d is defined in Section 5g.

# Dengue: range 0.06-0.26 (Bhatt et al. 2013; Undurraga et al. 2013)
under_rho_dengue    <- 0.10

# Malaria: range 0.10-0.35 (WHO World Malaria Report 2024)
under_rho_malaria    <- 0.2

# Measles: range 0.40-0.80 (Simons et al. 2012)
under_rho_measles    <- 0.6

# Pertussis: range 0.01-0.10 (Chen et al. 2016)
under_rho_pertussis    <- 0.10

# Influenza: range 0.01-0.10 (McCarthy et al. 2020; Hayward et al. 2014)
under_rho_influenza    <- 0.055

# ---- 6b. Country-level disease incidence tables ---------------
# Used by every model tier and the evaluation scripts.
# total_inc[c] = (disease metric for country c) / rho_d, where the metric is:
#   dengue   — mean June cases (2024–2025) / national population
#   malaria  — annual incidence per 1,000 / (12 × 1,000)
#   measles  — annual incidence per 1,000,000 / (12 × 1e6)
#   pertussis— annual incidence per 1,000,000 / (12 × 1e6)
#   influenza— mean June positive specimens (2023–2025) / national population

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
  mutate(total_inc = mean_june_cases / (population_country * under_rho_dengue)) %>%
  select(Country, total_inc)

malaria_country_inc <- malaria_cases %>%
  mutate(Country = recode(Country,
    "Democratic Republic of the Congo" = "Zaire (formerly DRC)")) %>%
  mutate(total_inc = cases_per1K / (12 * 1000 * under_rho_malaria)) %>%
  select(Country, total_inc)

measles_country_inc <- measles_incidence %>%
  mutate(Country = recode(Country,
    "Democratic Republic of the Congo" = "Zaire (formerly DRC)")) %>%
  mutate(total_inc = incidence_per1M / (12 * 1e6 * under_rho_measles)) %>%
  select(Country, total_inc)

pertussis_country_inc <- pertussis_incidence %>%
  mutate(Country = recode(Country,
    "Democratic Republic of the Congo" = "Zaire (formerly DRC)")) %>%
  mutate(total_inc = incidence_per1M / (12 * 1e6 * under_rho_pertussis)) %>%
  select(Country, total_inc)

# Influenza: mean June positive specimens (2023–2025) / national population
# Specimens serve as a proxy for reported cases; rho corrects for under-detection.
influenza_june_country <- influenza_june_specimens %>%
  left_join(population_of_world, by = "Country") %>%
  drop_na(population_country) %>%
  filter(mean_june_inf > 0) %>%
  mutate(total_inc = mean_june_inf / (population_country * under_rho_influenza)) %>%
  select(Country, total_inc)


# ============================================================
# 7. SHARED AESTHETICS — used by all narrative figures (§12)
# ============================================================

disease_colors <- c(
  "Dengue"    = "#C0392B",
  "Influenza" = "#2980B9",
  "Pertussis" = "#8E44AD",
  "Malaria"   = "#E67E22",
  "Measles"   = "#27AE60"
)

region_colors <- c(
  "Africa"        = "#D4691E",
  "Latin America" = "#2D9E4C",
  "Caribbean"     = "#1A8F61",
  "Europe"        = "#3A5FA5",
  "Asia"          = "#9C5BC4",
  "Mexico"        = "#C0392B",
  "Canada"        = "#96281B",
  "Mideast"       = "#D4AC0D",
  "Middle East"   = "#D4AC0D",
  "Oceania"       = "#17A589",
  "Other"         = "#95A5A6"
)

# Within-facet reorder helper (tidytext::reorder_within equivalent)
reorder_within <- function(x, by, within, fun = mean, sep = "___") {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}
scale_y_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_y_discrete(labels = function(x) gsub(reg, "", x), ...)
}

# ============================================================
# 8. M2 — WC-ADJUSTED IMPORTATION MODEL
# ============================================================
# June 2026 arrivals are projected from COR June 2024 with a growth
# factor phi_c assigned in three tiers (Appendix A.3):
#   Tier 1 — the 12 NTTO markets: phi_c = NTTO 2026 forecast / 2024
#            arrivals (NTTO 2025 forecast tables; includes any World
#            Cup effect).
#   Tier 2 — other World Cup-qualified countries: phi_WC = 85,017 /
#            72,390 = 1.174, NTTO's forecast of total 2026 arrivals over
#            2024 arrivals.
#   Tier 3 — all other countries: phi_base, the background growth
#            estimated from COR and T-100 data (Section 8b; 1.077).
# Projected arrivals are routed to cities with T-100 fractions.
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
# phi_base is the 2024→2026 background growth of US inbound travel:
# the growth factor for Tier 3 countries, and the background trend
# subtracted from phi_c to define the World Cup increment (Section 9b).
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
#   Priority 3: Baseline factor for all other countries
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

# ---- 8d. Country × city arrivals matrix (T-100 routing) ------
# N_{c,v}^{2026} = june_2026[c] × f_{c,v}^{T100}
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

# Incidence tables and parameters (rho_d, p_d) come from Sections 5g
# and 6, unchanged, so the model tiers differ only in travel volume and
# routing.

# ---- 8g. WC-adjusted estimates for all five diseases --------

dengue_wc_results <- compute_importation_country_level(
  arrivals_df    = arrivals_country_city_2026,
  country_inc_df = dengue_june_country,
  p_travel_inf   = p_travel_inf_dengue,
  title_text     = "Dengue importation intensity — WC-adjusted (June 2026)"
)

malaria_wc_results <- compute_importation_country_level(
  arrivals_df    = arrivals_country_city_2026,
  country_inc_df = malaria_country_inc,
  p_travel_inf   = p_travel_inf_malaria,
  title_text     = "Malaria importation intensity — WC-adjusted (June 2026)"
)

measles_wc_results <- compute_importation_country_level(
  arrivals_df    = arrivals_country_city_2026,
  country_inc_df = measles_country_inc,
  p_travel_inf   = p_travel_inf_measles,
  title_text     = "Measles importation intensity — WC-adjusted (June 2026)"
)

pertussis_wc_results <- compute_importation_country_level(
  arrivals_df    = arrivals_country_city_2026,
  country_inc_df = pertussis_country_inc,
  p_travel_inf   = p_travel_inf_pertussis,
  title_text     = "Pertussis importation intensity — WC-adjusted (June 2026)"
)

influenza_wc_results <- compute_importation_country_level(
  arrivals_df    = arrivals_country_city_2026,
  country_inc_df = influenza_june_country,
  p_travel_inf   = p_travel_inf_influenza,
  title_text     = "Influenza importation intensity — WC-adjusted (June 2026)"
)

# ---- 8h. MC summaries — WC-adjusted (Model 2) ---------------
dengue_mc_wc    <- compute_mc_summary(dengue_wc_results$importation$imp_intensity,    dengue_wc_results$importation$destination_city,    under_rho_dengue,    p_travel_inf_dengue,    0.06,   0.26,   p_min_dengue,  p_max_dengue, n_mc)
malaria_mc_wc   <- compute_mc_summary(malaria_wc_results$importation$imp_intensity,   malaria_wc_results$importation$destination_city,   under_rho_malaria,   p_travel_inf_malaria,   0.10,   0.35,   p_min_malaria,  p_max_malaria, n_mc)
measles_mc_wc   <- compute_mc_summary(measles_wc_results$importation$imp_intensity,   measles_wc_results$importation$destination_city,   under_rho_measles,   p_travel_inf_measles,   0.40,   0.80,   p_min_measles,  p_max_measles, n_mc)
pertussis_mc_wc <- compute_mc_summary(pertussis_wc_results$importation$imp_intensity, pertussis_wc_results$importation$destination_city, under_rho_pertussis, p_travel_inf_pertussis, 0.01,   0.10,   p_min_pertussis,  p_max_pertussis, n_mc)
influenza_mc_wc <- compute_mc_summary(influenza_wc_results$importation$imp_intensity, influenza_wc_results$importation$destination_city, under_rho_influenza, p_travel_inf_influenza, 0.01,   0.10,   p_min_influenza,  p_max_influenza, n_mc)

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
#       The increment above background growth (phi_base):
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
# The fan increment is computed for every country with phi_c >
# phi_base. Countries with no matches in the schedule (non-qualified
# countries, including the NTTO markets China, India, and Italy, and the
# playoff qualifiers listed as TBD in the schedule template) have no fan
# routing, so their increment is counted in M2 but not in M3 (Appendix
# A.5; main text, Limitations).
#
# Fans of matches played in Canada or Mexico drop out of the US totals;
# results are kept for the 11 US host cities only.
# ============================================================

# ---- 9a. Parse match schedule --------------------------------
# One row per match → pivot to one row per team per match so both
# participating nations generate fan travel to the same venue.
# The schedule file is the group-stage template released before the
# playoffs: TBD entries (the six playoff qualifiers) are dropped, and
# Mexico's three matches are not listed.
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

# Schedule-based WC fan routing fraction:
#   omega_{c,v} = g_{c,v} / G_c
schedule_routing <- team_venue_games %>%
  left_join(total_games_per_team, by = "team") %>%
  mutate(wc_routing = n_games / total_games) %>%
  select(team, venue_city, wc_routing)

# ---- 9b. Decompose June 2026 travel into WC-fan vs. background ---
# Uses travel_volume_june_2026 from Section 8c (columns: Country,
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
# Countries with no matches in the schedule have no row in
# schedule_routing and are dropped here (see Section 9 header).
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
# Processes both travel streams
# through the Poisson framework and combines them:
#
#   Lambda[v, d] = sum_{c in Q} (arrivals_wc[c,v] * I_{c,d} * p_d)   [WC fans]
#               + sum_c         (arrivals_bg[c,v]  * I_{c,d} * p_d)   [background]
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


# ---- 9g. MC summaries — Schedule-driven (Model 3) -----------
# This is the primary uncertainty result used in the main paper.
dengue_mc_sched    <- compute_mc_summary(dengue_sched_results$importation$imp_intensity,    dengue_sched_results$importation$destination_city,    under_rho_dengue,    p_travel_inf_dengue,    0.06,   0.26,   p_min_dengue,  p_max_dengue, n_mc)
malaria_mc_sched   <- compute_mc_summary(malaria_sched_results$importation$imp_intensity,   malaria_sched_results$importation$destination_city,   under_rho_malaria,   p_travel_inf_malaria,   0.10,   0.35,   p_min_malaria,  p_max_malaria, n_mc)
measles_mc_sched   <- compute_mc_summary(measles_sched_results$importation$imp_intensity,   measles_sched_results$importation$destination_city,   under_rho_measles,   p_travel_inf_measles,   0.40,   0.80,   p_min_measles,  p_max_measles, n_mc)
pertussis_mc_sched <- compute_mc_summary(pertussis_sched_results$importation$imp_intensity, pertussis_sched_results$importation$destination_city, under_rho_pertussis, p_travel_inf_pertussis, 0.01,   0.10,   p_min_pertussis,  p_max_pertussis, n_mc)
influenza_mc_sched <- compute_mc_summary(influenza_sched_results$importation$imp_intensity, influenza_sched_results$importation$destination_city, under_rho_influenza, p_travel_inf_influenza, 0.01,   0.10,   p_min_influenza,  p_max_influenza, n_mc)

# ---- COMPARISON TABLE: 11-city totals (schedule-driven, MC) ----
for (dis in c("dengue", "malaria", "measles")) {
  obj <- get(paste0(dis, "_mc_sched"))
  tot <- obj %>%
    summarise(
      med = sum(lambda_median),
      lo  = sum(lambda_lo),
      hi  = sum(lambda_hi)
    )
  cat(sprintf("%-10s  median = %5.1f   95%% CI: %5.1f – %5.1f\n",
              dis, tot$med, tot$lo, tot$hi))
}

# ============================================================
# 11. COUNTRY-LEVEL IMPORTATION CONTRIBUTIONS
# ============================================================
#
# The Poisson model decomposes additively:
#   Lambda[v, d] = sum_c lambda[c, v, d]
#
# compute_importation_schedule() stores per-country, per-city,
# per-stream contributions in $country_contributions. These feed
# Figure 4 (Section 12e) and the diaspora extension (saved in
# Section 13).
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

# Monte Carlo uncertainty for country-level contributions. Country
# lambda is proportional to p_d / rho_d, so every country of a disease
# shares the same multiplicative factor (same seeded draws as the main
# model). Bars use the Monte Carlo median and error bars the 2.5th and
# 97.5th percentiles, consistent with Figures 1-3.
mc_country_scales <- tibble(
  disease = c("Dengue", "Malaria", "Measles", "Pertussis", "Influenza"),
  sc = list(
    compute_mc_summary(1, "s", under_rho_dengue,    p_travel_inf_dengue,    0.06, 0.26, p_min_dengue,    p_max_dengue),
    compute_mc_summary(1, "s", under_rho_malaria,   p_travel_inf_malaria,   0.10, 0.35, p_min_malaria,   p_max_malaria),
    compute_mc_summary(1, "s", under_rho_measles,   p_travel_inf_measles,   0.40, 0.80, p_min_measles,   p_max_measles),
    compute_mc_summary(1, "s", under_rho_pertussis, p_travel_inf_pertussis, 0.01, 0.10, p_min_pertussis, p_max_pertussis),
    compute_mc_summary(1, "s", under_rho_influenza, p_travel_inf_influenza, 0.01, 0.10, p_min_influenza, p_max_influenza)
  )
) %>%
  mutate(scale_mid = map_dbl(sc, "lambda_median"),
         scale_lo  = map_dbl(sc, "lambda_lo"),
         scale_hi  = map_dbl(sc, "lambda_hi")) %>%
  select(disease, scale_mid, scale_lo, scale_hi)

top_countries_ci <- top_countries %>%
  left_join(mc_country_scales, by = "disease") %>%
  mutate(
    imports_lo    = total_imports * scale_lo,
    imports_hi    = total_imports * scale_hi,
    total_imports = total_imports * scale_mid   # bar = Monte Carlo median
  )

# ============================================================
# 11e. FIGURE S3 — CI ASYMMETRY
# ============================================================
# For each disease × city, compares the upside ratio
# Lambda_hi / Lambda_median with the downside ratio
# Lambda_median / Lambda_lo. Because Lambda scales with p_d / rho_d,
# both ratios are the same for every city of a disease and are set by
# the Uniform ranges of rho_d and p_d.

mc_asym_data <- bind_rows(
  dengue_mc_sched    %>% mutate(disease = "Dengue"),
  malaria_mc_sched   %>% mutate(disease = "Malaria"),
  measles_mc_sched   %>% mutate(disease = "Measles"),
  pertussis_mc_sched %>% mutate(disease = "Pertussis"),
  influenza_mc_sched %>% mutate(disease = "Influenza")
) %>%
  mutate(
    disease    = factor(disease,
                        levels = c("Dengue","Influenza","Pertussis","Malaria","Measles")),
    city_clean = str_to_title(destination_city),
    upside     = lambda_hi     / lambda_median,
    downside   = lambda_median / lambda_lo,
    skewness   = (lambda_hi - lambda_median) / (lambda_median - lambda_lo)
  )

ci_asymmetry_plot <- mc_asym_data %>%
  ggplot(aes(x = downside, y = upside, color = disease, label = city_clean)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "gray60", linewidth = 0.7) +
  geom_point(size = 3, alpha = 0.85) +
  annotate("text", x = 1.4, y = 1.1, label = "Interval extends\nfurther below median",
           size = 3.2, color = "gray40", hjust = 0) +
  annotate("text", x = 1.1, y = 3.2, label = "Interval extends\nfurther above median",
           size = 3.2, color = "gray40", hjust = 0) +
  scale_color_manual(values = disease_colors, name = "") +
  scale_x_continuous(name = expression("Downside ratio  " *
                                       (Lambda[median] / Lambda[lo])),
                     limits = c(1, NA)) +
  scale_y_continuous(name = expression("Upside ratio  " *
                                       (Lambda[hi] / Lambda[median])),
                     limits = c(1, NA)) +
  # No title/subtitle baked into the plot: explanatory text belongs in
  # the LaTeX caption, not duplicated in the image itself. The
  # annotate() calls above already label the upper/lower bound
  # regions directly on the plot.
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

ggsave(ci_asymmetry_plot,
       file   = "Figures/FigureS3.png",
       height = 6, width = 9, dpi = 300)
# Vector PDF for journal submission (Elsevier line-art requirement)
ggsave(ci_asymmetry_plot,
       file   = "Figures/FigureS3.pdf",
       height = 6, width = 9, device = grDevices::quartz, type = "pdf")  # macOS native vector PDF (cairo_pdf needs XQuartz)
print(ci_asymmetry_plot)


# ============================================================
# 12. MAIN AND SUPPLEMENTARY FIGURES
# ============================================================
#  Figure 1  — fig1_risk_heatmap:   P(>=1) by disease and city (M3)
#  Figure 2  — fig2_lambda_ci:      Lambda, median and 95% UI (M3)
#  Figure 4  — fig4_country_drivers: top 10 source countries per disease
#  Figure S2 — fig1_heatmap_wc:     P(>=1) heatmap under M2
# Figure 3 and Figure S1 need the M1 baseline and are built in
# figure3_figureS1.R.
# ============================================================

# ---- 12a. Shared data: all schedule-driven MC results -------

mc_all_sched <- bind_rows(
  dengue_mc_sched    %>% mutate(disease = "Dengue"),
  malaria_mc_sched   %>% mutate(disease = "Malaria"),
  measles_mc_sched   %>% mutate(disease = "Measles"),
  pertussis_mc_sched %>% mutate(disease = "Pertussis"),
  influenza_mc_sched %>% mutate(disease = "Influenza")
) %>%
  mutate(
    disease    = factor(disease,
                        levels = c("Dengue","Influenza","Pertussis","Malaria","Measles")),
    city_clean = str_to_title(destination_city)
  )

# Canonical city order: highest aggregate Lambda first
city_order_main <- mc_all_sched %>%
  group_by(city_clean) %>%
  summarise(total_lambda = sum(lambda_median), .groups = "drop") %>%
  arrange(desc(total_lambda)) %>%
  pull(city_clean)

# ---- 12b. FIGURE 1 — Risk overview heatmap ------------------
# Disease rows × city columns. Fill = P(≥1) under schedule-driven
# model (MC median). Annotated with the exact probability value.
#
# Text contrast rule:
#   plasma palette is dark (blue/purple) at LOW values and bright
#   (orange/yellow) at HIGH values.  White text is needed on dark tiles
#   (low prob); dark text on bright tiles (high prob).
#   Crossover at ~0.55 on the plasma scale.
#
# Legend key height (1.75 cm) keeps the "1.00" label inside the plot.

fig1_data <- mc_all_sched %>%
  mutate(
    city_clean = factor(city_clean, levels = city_order_main),
    prob_label = case_when(
      prob_median >= 0.995 ~ ">0.99",
      prob_median  < 0.005 ~ "<0.01",
      TRUE                 ~ sprintf("%.2f", prob_median)
    ),
    text_col = if_else(prob_median > 0.55, "gray15", "white")
  )

fig1_risk_heatmap <- ggplot(fig1_data,
    aes(x = city_clean, y = disease, fill = prob_median)) +
  geom_tile(color = "white", linewidth = 0.9) +
  geom_text(aes(label = prob_label, color = text_col),
            size = 4, fontface = "bold") +
  scale_fill_viridis_c(
    name   = expression(P(X >= 1)),
    option = "plasma",
    limits = c(0, 1),
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    labels = c("0.00", "0.25", "0.50", "0.75", "1.00")
  ) +
  scale_color_identity() +
  labs(x = "", y = "") +
  # No title/subtitle baked into the plot: explanatory text belongs in
  # the LaTeX caption, not duplicated in the image itself.
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x       = element_text(angle = 35, hjust = 1, size = 11),
    axis.text.y       = element_text(size = 12, face = "italic"),
    panel.grid        = element_blank(),
    legend.position   = "right",
    legend.key.height = unit(1.75, "cm"),
    legend.margin     = margin(t = 0, r = 4, b = 0, l = 4),
    # Top plot margin reserves room for the legend title, which ggplot2
    # stacks above the color key; without it, the title is clipped by
    # the top edge of the device.
    plot.margin       = margin(t = 20, r = 8, b = 5.5, l = 5.5)
  )

ggsave(fig1_risk_heatmap,
       file   = "Figures/Figure1.png",
       height = 4.8, width = 12.5, dpi = 300)
# Vector PDF for journal submission (Elsevier line-art requirement)
ggsave(fig1_risk_heatmap,
       file   = "Figures/Figure1.pdf",
       height = 4.8, width = 12.5, device = grDevices::quartz, type = "pdf")  # macOS native vector PDF (cairo_pdf needs XQuartz)
print(fig1_risk_heatmap)

# ---- 12b-extra. FIGURE S2 — M2 heatmap ----------------------
# Same layout as Figure 1, with the city order fixed to
# city_order_main. make_heatmap() is reused for Figure S1 (M1) in
# figure3_figureS1.R.

mc_all_wc <- bind_rows(
  dengue_mc_wc    %>% mutate(disease = "Dengue"),
  malaria_mc_wc   %>% mutate(disease = "Malaria"),
  measles_mc_wc   %>% mutate(disease = "Measles"),
  pertussis_mc_wc %>% mutate(disease = "Pertussis"),
  influenza_mc_wc %>% mutate(disease = "Influenza")
) %>%
  mutate(
    disease    = factor(disease,
                        levels = c("Dengue","Influenza","Pertussis","Malaria","Measles")),
    city_clean = str_to_title(destination_city)
  )

make_heatmap <- function(mc_data) {
  dat <- mc_data %>%
    filter(city_clean %in% city_order_main) %>%
    mutate(
      city_clean = factor(city_clean, levels = city_order_main),
      prob_label = case_when(
        prob_median >= 0.995 ~ ">0.99",
        prob_median  < 0.005 ~ "<0.01",
        TRUE                 ~ sprintf("%.2f", prob_median)
      ),
      text_col = if_else(prob_median > 0.55, "gray15", "white")
    )

  ggplot(dat, aes(x = city_clean, y = disease, fill = prob_median)) +
    geom_tile(color = "white", linewidth = 0.9) +
    geom_text(aes(label = prob_label, color = text_col),
              size = 4, fontface = "bold") +
    scale_fill_viridis_c(
      name   = expression(P(X >= 1)),
      option = "plasma",
      limits = c(0, 1),
      breaks = c(0, 0.25, 0.5, 0.75, 1),
      labels = c("0.00", "0.25", "0.50", "0.75", "1.00")
    ) +
    scale_color_identity() +
    labs(x = "", y = "") +
    # No title/subtitle baked into the plot: explanatory text belongs
    # in the LaTeX caption, not duplicated in the image itself.
    theme_minimal(base_size = 13) +
    theme(
      axis.text.x       = element_text(angle = 35, hjust = 1, size = 11),
      axis.text.y       = element_text(size = 12, face = "italic"),
      panel.grid        = element_blank(),
      legend.position   = "right",
      legend.key.height = unit(1.75, "cm"),
      legend.margin     = margin(t = 0, r = 4, b = 0, l = 4),
      # Top plot margin reserves room for the legend title, which ggplot2
      # stacks above the color key; without it, the title is clipped by
      # the top edge of the device.
      plot.margin       = margin(t = 20, r = 8, b = 5.5, l = 5.5)
    )
}

fig1_heatmap_wc <- make_heatmap(mc_all_wc)

ggsave(fig1_heatmap_wc,
       file   = "Figures/FigureS2.png",
       height = 4.8, width = 12.5, dpi = 300)
# Vector PDF for journal submission (Elsevier line-art requirement)
ggsave(fig1_heatmap_wc,
       file   = "Figures/FigureS2.pdf",
       height = 4.8, width = 12.5, device = grDevices::quartz, type = "pdf")  # macOS native vector PDF (cairo_pdf needs XQuartz)
print(fig1_heatmap_wc)

# ---- 12c. FIGURE 2 — Expected importations with 95% CI ------
# Horizontal dot-range chart: point = median Lambda,
# whiskers = 95% MC uncertainty interval.
# Each disease panel has a free x-axis so within-disease
# city differences are clear regardless of scale differences.

fig2_data <- mc_all_sched %>%
  mutate(city_clean = factor(city_clean, levels = rev(city_order_main)))

fig2_lambda_ci <- ggplot(fig2_data,
    aes(x = lambda_median, xmin = lambda_lo, xmax = lambda_hi,
        y = city_clean, color = disease)) +
  geom_linerange(linewidth = 0.85, alpha = 0.65) +
  geom_point(size = 3) +
  facet_wrap(~ disease, scales = "free_x", ncol = 3) +
  scale_color_manual(values = disease_colors, guide = "none") +
  scale_x_continuous(
    expand = expansion(mult = c(0.04, 0.20)),
    labels = scales::number_format(accuracy = 0.001, drop0trailing = TRUE)
  ) +
  labs(
    x        = expression(Lambda ~ "(expected importations, median and 95% UI)"),
    y        = ""
  ) +
  # No title/subtitle baked into the plot: explanatory text belongs in
  # the LaTeX caption, not duplicated in the image itself.
  theme_minimal(base_size = 16) +
  theme(
    strip.text         = element_text(face = "bold", size = 15.5, color = "gray20"),
    strip.background   = element_rect(fill = "gray96", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.y        = element_text(size = 12.5),
    axis.text.x        = element_text(size = 12)
  )

ggsave(fig2_lambda_ci,
       file   = "Figures/Figure2.png",
       height = 9, width = 14, dpi = 300)
# Vector PDF for journal submission (Elsevier line-art requirement)
ggsave(fig2_lambda_ci,
       file   = "Figures/Figure2.pdf",
       height = 9, width = 14, device = grDevices::quartz, type = "pdf")  # macOS native vector PDF (cairo_pdf needs XQuartz)
print(fig2_lambda_ci)

# ---- 12d. 11-city totals helper ------------------------------
# Sums city-level MC medians and interval bounds over the 11 cities.
# Used by model1_baseline_2026.R.
total_lambda <- function(mc_obj) {
  mc_obj %>%
    summarise(
      median = sum(lambda_median),
      lo     = sum(lambda_lo),
      hi     = sum(lambda_hi)
    )
}

# ---- 12e. FIGURE 4 — Source country drivers -----------------
# Top 10 source countries per disease, colored by world region.
# Built as 5 individual plots (so each has its own within-panel
# country ordering) then combined with a shared legend.
#
# Region lookup is hard-coded to avoid unreliable joins between
# disease data naming and arrivals data naming conventions.

assign_region <- function(country) {
  dplyr::case_when(
    country %in% c(
      "Brazil","Colombia","Venezuela","Peru","Ecuador","Bolivia",
      "Argentina","Chile","Paraguay","Uruguay","Costa Rica","Honduras",
      "Guatemala","El Salvador","Nicaragua","Panama","Dominican Republic",
      "Cuba","Haiti","Jamaica","Trinidad and Tobago","Guyana","Suriname"
    ) ~ "Latin America",
    country %in% c("Mexico")              ~ "Mexico",
    country %in% c("Canada")              ~ "Canada",
    country %in% c(
      "Nigeria","Ghana","Kenya","Ethiopia","Tanzania","Uganda",
      "Cameroon","Zaire (formerly DRC)","Mozambique","Angola","Zambia",
      "Zimbabwe","Rwanda","Burundi","Malawi","Madagascar","Senegal",
      "Côte d'Ivoire","Guinea","Burkina Faso","Mali","Niger","Chad",
      "Sudan","Somalia","South Africa","Egypt","Morocco","Algeria",
      "Tunisia","Libya","Cape Verde"
    ) ~ "Africa",
    country %in% c(
      "India","China","Japan","South Korea","Philippines","Vietnam",
      "Thailand","Indonesia","Malaysia","Bangladesh","Pakistan",
      "Nepal","Myanmar","Sri Lanka","Cambodia","Laos","Taiwan",
      "Singapore","Hong Kong"
    ) ~ "Asia",
    country %in% c(
      "Germany","France","United Kingdom","Italy","Spain","Netherlands",
      "Belgium","Poland","Ukraine","Russia","Sweden","Norway","Denmark",
      "Finland","Portugal","Austria","Switzerland","Czech Republic",
      "Hungary","Romania","Greece","Turkey","Slovakia","Croatia",
      "Bulgaria","Serbia","North Macedonia","Albania","Ireland",
      "Scotland","England"
    ) ~ "Europe",
    country %in% c(
      "Israel","Jordan","Saudi Arabia","United Arab Emirates",
      "Iraq","Syria","Lebanon","Iran","Kuwait","Qatar","Bahrain",
      "Oman","Yemen"
    ) ~ "Middle East",
    country %in% c("Australia","New Zealand","Fiji","Papua New Guinea")
                   ~ "Oceania",
    TRUE           ~ "Other"
  )
}

top_countries_region <- top_countries_ci %>%
  mutate(
    new_region = assign_region(Country),
    disease    = factor(disease,
                        levels = c("Dengue","Influenza","Pertussis","Malaria","Measles"))
  ) %>%
  group_by(disease) %>%
  slice_max(total_imports, n = 10, with_ties = FALSE) %>%
  ungroup()

make_country_panel <- function(dis) {
  dat <- top_countries_region %>% filter(disease == dis)
  ggplot(dat, aes(x = reorder(Country, total_imports),
                  y = total_imports,
                  fill = new_region)) +
    geom_col(alpha = 0.85, width = 0.75) +
    geom_errorbar(aes(ymin = imports_lo, ymax = imports_hi),
                  width = 0.38, linewidth = 0.65, color = "gray30") +
    coord_flip() +
    scale_fill_manual(values = region_colors, name = "World region") +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.22)),
      labels = scales::number_format(accuracy = 0.0001, drop0trailing = TRUE)
    ) +
    labs(x = "", y = expression(Lambda), title = dis) +
    theme_minimal(base_size = 17.5) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      plot.title         = element_text(face = "bold", size = 18.5, color = "gray15"),
      legend.position    = "none",
      plot.margin        = margin(5.5, 22, 5.5, 5.5)  # room for the last axis label
    )
}

# Extract a shared legend from a plot that contains all regions.
# Font sizes here and in make_country_panel() are chosen so that,
# once both this figure and Figure 2 are scaled to the same page
# width via \includegraphics[width=\textwidth], their text renders
# at matching apparent size (font size / source canvas width held
# constant across figures).
shared_legend <- cowplot::get_legend(
  ggplot(top_countries_region,
         aes(x = Country, y = total_imports, fill = new_region)) +
    geom_col() +
    scale_fill_manual(values = region_colors, name = "World region") +
    theme_minimal(base_size = 17.5) +
    theme(
      legend.position = "right",
      legend.title    = element_text(size = 15.5, face = "bold"),
      legend.text     = element_text(size = 14.5),
      legend.key.size = unit(0.6, "cm")
    )
)

# 5 disease panels + shared legend in the 6th (empty) slot.
country_panels <- list(
  make_country_panel("Dengue"),
  make_country_panel("Influenza"),
  make_country_panel("Pertussis"),
  make_country_panel("Malaria"),
  make_country_panel("Measles"),
  shared_legend
)

fig4_country_drivers <- cowplot::plot_grid(
  plotlist   = country_panels,
  ncol       = 2,
  labels     = c("A", "B", "C", "D", "E", ""),
  label_size = 14.5
)

ggsave(fig4_country_drivers,
       file   = "Figures/Figure4.png",
       height = 13, width = 15, dpi = 300)
# Vector PDF for journal submission (Elsevier line-art requirement)
ggsave(fig4_country_drivers,
       file   = "Figures/Figure4.pdf",
       height = 13, width = 15, device = grDevices::quartz, type = "pdf")  # macOS native vector PDF (cairo_pdf needs XQuartz)
print(fig4_country_drivers)

# ============================================================
# 13. SAVE OUTPUTS FOR DOWNSTREAM SCRIPTS
# ============================================================
# Objects saved here are loaded by importationRisk_diaspora_extension.R.
# Re-run this script to refresh the file.

save(
  all_contributions,   # country × city × stream lambda (Model 3)
  mc_all_sched,        # MC summaries for schedule-driven model
  top_countries_ci,    # country rankings with 95% CI
  city_order_main,     # canonical city order (by descending total lambda)
  disease_colors,      # shared color palette
  region_colors,       # shared region color palette
  mc_ranges,           # MC parameter bounds per disease
  mc_country_scales,   # per-disease MC scale factors (median, 2.5%, 97.5%)
  assign_region,       # function: country → world region label
  file = "Data/model_outputs.RData"
)
message("Model outputs saved to Data/model_outputs.RData")

