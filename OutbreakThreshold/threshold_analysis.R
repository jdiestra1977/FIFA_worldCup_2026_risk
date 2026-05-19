# ============================================================
# Critical Outbreak Size for Disease Importation into the US
# Threshold Analysis — Starter Script
# Author : Jose Herrera-Diestra
# Created: May 2026
#
# PURPOSE
# -------
# This script implements the inverted Poisson importation model
# described in threshold_pipeline.tex. It answers:
#   "How large must an outbreak in country c be before we expect
#    at least one imported case to arrive in the US?"
#
# The mathematical framework is inherited from the WC importation
# risk analysis (Code/importationRisk_main.R). All shared data
# files are read from the parent project's Data/ folder.
#
# SECTIONS
# --------
#   0.  Packages
#   1.  Working directory and shared data
#   2.  Disease parameters
#   3.  Core functions
#   4.  Critical thresholds — all country × disease pairs
#   5.  Temporal model — time to threshold
#   6.  Case studies: DRC/Ebola vs Brazil/Dengue
#   7.  Figures
#   8.  Export results table
# ============================================================


# ============================================================
# 0. PACKAGES
# ============================================================
library(tidyverse)
library(readxl)
library(janitor)
library(cowplot)


# ============================================================
# 1. WORKING DIRECTORY AND SHARED DATA
# ============================================================
# All data files live in the parent project's Data/ folder.
# This script is run from OutbreakThreshold/ — paths go up one
# level with ../
setwd("~/Documents/GitHub/FIFA_worldCup_2026_risk/")

# Create output directory for figures if it does not exist yet
dir.create("OutbreakThreshold/Figures/", recursive = TRUE, showWarnings = FALSE)

# --- 1a. Country populations ---------------------------------
population_of_world <- read_csv("Data/population2020.csv") %>%
  rename(Country = COUNTRY, population_country = POPULATION) %>%
  mutate(Country = if_else(
    Country == "DR Congo", "Zaire (formerly DRC)", Country))

# --- 1b. COR monthly arrivals --------------------------------
# Use mean ANNUAL arrivals (all months) for N_c — this analysis
# is not WC-specific and should not be limited to June only.
arrivals_COR <- read_csv(
  "Data/Monthly_Arrivals_Country_of_Residence_COR_1.csv")

# Compute mean monthly arrivals per country across all available
# monthly columns (skip Country and World_region columns).
monthly_cols <- arrivals_COR %>%
  select(-Country, -World_region) %>%
  names()

mean_monthly_arrivals <- arrivals_COR %>%
  mutate(across(all_of(monthly_cols),
                ~ readr::parse_number(as.character(.)))) %>%
  rowwise() %>%
  mutate(mean_monthly = mean(c_across(all_of(monthly_cols)),
                             na.rm = TRUE)) %>%
  ungroup() %>%
  select(Country, mean_monthly) %>%
  drop_na()

message("Countries with COR travel data: ",
        nrow(mean_monthly_arrivals))

# --- 1c. Disease incidence data (for current-incidence overlay) -
# Dengue: mean June cases (2024-2025), country level
dengue_data <- read_xlsx(
  "Data/dengue-global-data-2025-12-10.xlsx") %>%
  select(date, country, cases) %>%
  mutate(
    country = recode(country,
      "Venezuela (Bolivarian Republic of)" = "Venezuela",
      "Bolivia (Plurinational State of)"   = "Bolivia",
      "Iran (Islamic Republic of)"         = "Iran",
      "United Republic of Tanzania"        = "Tanzania")
  ) %>%
  filter(month(date) == 6, year(date) >= 2024) %>%
  group_by(country) %>%
  summarise(current_monthly_cases = mean(cases, na.rm = TRUE),
            .groups = "drop") %>%
  rename(Country = country)

# Malaria: 2024 annual incidence per 1,000 → monthly cases
malaria_data <- read_csv("Data/Malaria_National_Unit_data.csv") %>%
  filter(Year == 2024, Metric == "Incidence Rate") %>%
  select(Country = Name, incidence_per1K = Value) %>%
  left_join(population_of_world, by = "Country") %>%
  mutate(current_monthly_cases =
           incidence_per1K / 1000 * population_country / 12) %>%
  select(Country, current_monthly_cases) %>%
  drop_na()


# ============================================================
# 2. DISEASE PARAMETERS
# ============================================================
# rho   : reporting adjustment factor
# p     : travel-while-infectious probability
# R0    : basic reproduction number (central estimate)
# T_s   : mean serial interval in days
# See Table 1 of threshold_pipeline.tex for sources.

disease_params <- tribble(
  ~disease,    ~rho,  ~p,    ~R0,   ~T_s,
  "Dengue",    0.3,   0.50,  3.0,   20,
  "Malaria",   0.2,   0.30,  1.5,   30,
  "Measles",   0.6,   0.05,  15.0,  12,
  "Pertussis", 0.3,   0.70,  9.0,   14,
  "Ebola",     0.8,   0.01,  1.8,   15
)

print(disease_params)


# ============================================================
# 3. CORE FUNCTIONS
# ============================================================

# ---- 3a. Critical threshold ---------------------------------
# Computes the monthly case count C* in the source country at which
# P(>=1 importation to US) = theta.
#
#   C* = -ln(1 - theta) * P_c / (N_c * rho_d * p_d)
#
# Arguments:
#   N_c   — mean monthly travelers from source country to US
#   P_c   — population of source country
#   rho   — reporting adjustment factor
#   p     — travel-while-infectious probability
#   theta — importation probability threshold (vector OK)
#
# Returns a named numeric vector, one value per theta level.
compute_threshold <- function(N_c, P_c, rho, p,
                               theta = c(0.05, 0.10, 0.50, 0.95)) {
  -log(1 - theta) * P_c / (N_c * rho * p)
}


# ---- 3b. Time to threshold ----------------------------------
# Given an outbreak of current size C0 growing at rate r,
# computes how many days until C* is reached.
#
#   t* = (1/r) * ln(C* / C0)
#
# r is derived from R0 and serial interval T_s:
#   r = ln(R0) / T_s   (early exponential approximation)
#
# Returns NA if C0 >= C_star (threshold already crossed) and
# Inf if r <= 0 (no growth or declining outbreak).
compute_time_to_threshold <- function(C0, C_star, R0, T_s) {
  r <- log(R0) / T_s
  if (r <= 0)  return(Inf)
  if (C0 <= 0) return(NA_real_)
  t_star <- (1 / r) * log(C_star / C0)
  t_star  # negative means threshold already crossed
}


# ============================================================
# 4. CRITICAL THRESHOLDS — ALL COUNTRY × DISEASE PAIRS
# ============================================================
# Join travel volumes and populations, then apply compute_threshold()
# for each disease. Thresholds are computed at four probability
# levels: 5%, 10%, 50%, 95%.

base_data <- mean_monthly_arrivals %>%
  inner_join(population_of_world, by = "Country") %>%
  filter(mean_monthly > 0)

message("Countries with both travel and population data: ",
        nrow(base_data))

# Expand: one row per country × disease
thresholds_all <- base_data %>%
  crossing(disease_params) %>%
  mutate(
    C_star_05 = compute_threshold(mean_monthly, population_country,
                                   rho, p, theta = 0.05),
    C_star_10 = compute_threshold(mean_monthly, population_country,
                                   rho, p, theta = 0.10),
    C_star_50 = compute_threshold(mean_monthly, population_country,
                                   rho, p, theta = 0.50),
    C_star_95 = compute_threshold(mean_monthly, population_country,
                                   rho, p, theta = 0.95)
  )

# --- Quick diagnostic ----------------------------------------
message("\nTop 10 lowest C* (50% threshold) for Dengue:")
thresholds_all %>%
  filter(disease == "Dengue") %>%
  arrange(C_star_50) %>%
  select(Country, mean_monthly, C_star_05, C_star_50, C_star_95) %>%
  slice_head(n = 10) %>%
  mutate(across(where(is.numeric), ~ round(., 1))) %>%
  print()

message("\nTop 10 lowest C* (50% threshold) for Ebola:")
thresholds_all %>%
  filter(disease == "Ebola") %>%
  arrange(C_star_50) %>%
  select(Country, mean_monthly, C_star_05, C_star_50, C_star_95) %>%
  slice_head(n = 10) %>%
  mutate(across(where(is.numeric), ~ round(., 0))) %>%
  print()


# ============================================================
# 5. TEMPORAL MODEL — TIME TO THRESHOLD
# ============================================================
# For each country × disease, compute how many days the outbreak
# would need to grow (from a seed of 1 case) to cross C* (theta=0.10).
# This gives a "days to concern" metric under uncontrolled growth.

thresholds_temporal <- thresholds_all %>%
  mutate(
    days_to_threshold_10pct = pmap_dbl(
      list(C_star_10, R0, T_s),
      ~ compute_time_to_threshold(C0 = 1, C_star = ..1,
                                   R0 = ..2, T_s = ..3)
    )
  )

message("\nMedian days to 10% importation threshold (from seed of 1),",
        " by disease:")
thresholds_temporal %>%
  group_by(disease) %>%
  summarise(
    median_days = median(days_to_threshold_10pct,  na.rm = TRUE),
    min_days    = min(days_to_threshold_10pct,     na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(median_days) %>%
  print()


# ============================================================
# 6. CASE STUDIES: DRC/EBOLA vs BRAZIL/DENGUE
# ============================================================
# These two cases illustrate the extremes of the threshold spectrum:
#   - DRC/Ebola: tiny travel volume + very low p → enormous C*
#   - Brazil/Dengue: huge travel volume + moderate p → low C*

case_study_params <- tibble(
  label     = c("DRC / Ebola", "Brazil / Dengue"),
  Country   = c("Zaire (formerly DRC)", "Brazil"),
  disease   = c("Ebola", "Dengue"),
  # Current monthly cases (approximate as of May 2026)
  # DRC: ~350 total suspected/confirmed / ~1 month
  # Brazil: mean June dengue cases from WHO data
  C0        = c(350, NA)   # Brazil C0 filled from data below
)

# Fill Brazil C0 from dengue data
brazil_dengue_C0 <- dengue_data %>%
  filter(Country == "Brazil") %>%
  pull(current_monthly_cases)

if (length(brazil_dengue_C0) > 0) {
  case_study_params$C0[2] <- brazil_dengue_C0
} else {
  case_study_params$C0[2] <- 50000  # fallback estimate
  message("Brazil dengue C0 not found in data; using 50,000 as fallback.")
}

# Join with thresholds and compute time to threshold
case_studies <- case_study_params %>%
  left_join(
    thresholds_all %>% select(Country, disease, R0, T_s,
                               C_star_05, C_star_10, C_star_50),
    by = c("Country", "disease")
  ) %>%
  mutate(
    days_to_5pct  = pmap_dbl(list(C0, C_star_05, R0, T_s),
      ~ compute_time_to_threshold(..1, ..2, ..3, ..4)),
    days_to_10pct = pmap_dbl(list(C0, C_star_10, R0, T_s),
      ~ compute_time_to_threshold(..1, ..2, ..3, ..4)),
    days_to_50pct = pmap_dbl(list(C0, C_star_50, R0, T_s),
      ~ compute_time_to_threshold(..1, ..2, ..3, ..4)),
    already_above_50pct = C0 >= C_star_50
  )

message("\n--- Case study results ---")
case_studies %>%
  select(label, C0, C_star_05, C_star_50,
         days_to_5pct, days_to_50pct, already_above_50pct) %>%
  mutate(across(where(is.numeric), ~ round(., 1))) %>%
  print()


# ============================================================
# 7. FIGURES
# ============================================================

# ---- 7a. Top 20 countries by lowest C* (50%) per disease ----
top20_plot_data <- thresholds_all %>%
  group_by(disease) %>%
  slice_min(C_star_50, n = 20, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(disease = factor(disease,
    levels = c("Dengue","Malaria","Measles","Pertussis","Ebola")))

fig_ranking <- top20_plot_data %>%
  ggplot(aes(x = reorder(Country, -C_star_50),
             y = C_star_50)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = 1, linetype = "dashed",
             color = "red", linewidth = 0.5) +
  coord_flip() +
  scale_y_log10(
    labels = scales::label_comma(),
    breaks = c(1, 10, 100, 1e3, 1e4, 1e5, 1e6, 1e7)
  ) +
  facet_wrap(~disease, scales = "free_x", ncol = 3) +
  labs(
    x     = "",
    y     = "Critical monthly cases C* (log scale)",
    title = "Minimum outbreak size for 50% probability of US importation",
    subtitle = "Top 20 lowest-threshold source countries per disease"
  ) +
  theme_bw() +
  theme(
    axis.text.y  = element_text(size = 8),
    text         = element_text(size = 11),
    strip.text   = element_text(face = "bold")
  )

ggsave(fig_ranking,
       file   = "OutbreakThreshold/Figures/fig_threshold_ranking.png",
       height = 14, width = 18)

# ---- 7b. Risk ramp curves: P(>=1) vs outbreak size ----------
# Show how importation probability ramps up with outbreak size
# for the two case-study countries.
ramp_data <- case_studies %>%
  select(label, Country, disease, R0, T_s, C_star_50) %>%
  rowwise() %>%
  mutate(
    C_seq = list(
      seq(0, max(C_star_50 * 3, C0 * 2, na.rm = TRUE), length.out = 300)
    )
  ) %>%
  unnest(C_seq) %>%
  left_join(
    base_data %>% select(Country, mean_monthly),
    by = "Country"
  ) %>%
  left_join(
    disease_params %>% select(disease, rho, p),
    by = "disease"
  ) %>%
  mutate(
    Lambda   = mean_monthly * rho * C_seq / population_of_world$population_country[
      match(Country, population_of_world$Country)],
    prob     = 1 - exp(-Lambda * p)
  )

# Recalculate properly with population joined
ramp_data2 <- case_studies %>%
  select(label, Country, disease, C_star_50, C0) %>%
  left_join(base_data,        by = "Country") %>%
  left_join(disease_params %>% select(disease, rho, p), by = "disease") %>%
  rowwise() %>%
  mutate(
    C_max = max(C_star_50 * 3, C0 * 2, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
    C_seq = list(seq(0, C_max, length.out = 300))
  ) %>%
  unnest(C_seq) %>%
  mutate(
    Lambda = mean_monthly * rho * C_seq / population_country,
    prob   = 1 - exp(-Lambda * p)
  )

fig_ramp <- ramp_data2 %>%
  ggplot(aes(x = C_seq, y = prob, color = label)) +
  geom_line(linewidth = 1.2) +
  geom_vline(
    data = case_studies %>%
      left_join(case_study_params %>% select(label, C0),
                by = "label"),
    aes(xintercept = C0, color = label),
    linetype = "dashed", linewidth = 0.8
  ) +
  geom_hline(yintercept = c(0.05, 0.50),
             linetype = "dotted", color = "gray50") +
  annotate("text", x = 0, y = 0.07,
           label = "5% threshold", hjust = 0, size = 3.5, color = "gray40") +
  annotate("text", x = 0, y = 0.52,
           label = "50% threshold", hjust = 0, size = 3.5, color = "gray40") +
  scale_x_continuous(labels = scales::label_comma()) +
  scale_y_continuous(labels = scales::label_percent(),
                     limits = c(0, 1)) +
  scale_color_manual(
    values = c("DRC / Ebola" = "#d6604d", "Brazil / Dengue" = "#4393c3")
  ) +
  labs(
    x     = "Active monthly cases in source country",
    y     = expression(P(X >= 1 ~ "importation to US")),
    color = "",
    title = "Importation risk ramp: DRC/Ebola vs Brazil/Dengue",
    subtitle = "Dashed vertical lines = current reported outbreak size"
  ) +
  theme_bw() +
  theme(
    text             = element_text(size = 13),
    legend.position  = "bottom"
  )

ggsave(fig_ramp,
       file   = "OutbreakThreshold/Figures/fig_risk_ramp.png",
       height = 7, width = 10)

message("\nFigures saved to OutbreakThreshold/Figures/")


# ============================================================
# 8. EXPORT RESULTS TABLE
# ============================================================
# Full table: all country × disease combinations with thresholds
# and whether current incidence is above/below threshold.

# Add current incidence for dengue and malaria (others: extend later)
current_incidence <- bind_rows(
  dengue_data  %>% mutate(disease = "Dengue"),
  malaria_data %>% mutate(disease = "Malaria")
)

results_table <- thresholds_all %>%
  left_join(current_incidence, by = c("Country", "disease")) %>%
  mutate(
    ratio_to_threshold_50 = current_monthly_cases / C_star_50,
    above_threshold_50    = ratio_to_threshold_50 >= 1
  ) %>%
  select(Country, disease, mean_monthly, population_country,
         C_star_05, C_star_10, C_star_50, C_star_95,
         current_monthly_cases, ratio_to_threshold_50,
         above_threshold_50) %>%
  arrange(disease, C_star_50)

write_csv(results_table,
          "OutbreakThreshold/threshold_results.csv")

message("\n--- Summary: countries above 50% importation threshold ---")
results_table %>%
  filter(above_threshold_50 == TRUE, !is.na(current_monthly_cases)) %>%
  select(Country, disease, current_monthly_cases,
         C_star_50, ratio_to_threshold_50) %>%
  mutate(across(where(is.numeric), ~ round(., 2))) %>%
  arrange(disease, desc(ratio_to_threshold_50)) %>%
  print(n = 30)

message("\n--- Done ---")
message("Results table: OutbreakThreshold/threshold_results.csv")
message("Figures: OutbreakThreshold/Figures/")
