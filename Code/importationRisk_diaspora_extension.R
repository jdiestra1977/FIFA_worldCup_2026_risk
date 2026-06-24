# ============================================================
# FIFA World Cup 2026 — Diaspora-Adjusted Importation Risk
#
# Author : Jose Herrera-Diestra
# Created: June 2026
#
# PURPOSE
# -------
# Extends the three-model importation framework from
# importationRisk_main_with_uncertainty.R by incorporating
# US diaspora population data (Census ACS 2020-2024) to estimate
# community-weighted importation risk at each WC venue city.
#
# Two mechanisms are modelled:
#
#   Mechanism A — Local social mixing at the venue city
#     Omega_A[c, v, d] = lambda[c, v, d] * D[c, v]
#     An imported case lands inside a socially connected local
#     diaspora community from the same source country.
#
#   Mechanism B — Diaspora convergence and return seeding
#     Diaspora members from across the US travel to venue cities
#     to watch their home country's games, then return home.
#     Omega_B[c, v_home, d] = lambda[c, v_match, d] * D[c, v_home]
#     Risk lands in diaspora hub cities, not the game venue.
#
# PREREQUISITES
# -------------
# 1. Run importationRisk_main_with_uncertainty.R (through Section 13)
#    to generate Data/model_outputs.RData.
# 2. Get a free Census API key at https://api.census.gov/data/key_signup.html
#    and run: census_api_key("YOUR_KEY_HERE", install = TRUE)
#    Section 2 downloads ACS table B05006 automatically and caches it.
# ============================================================


# ============================================================
# 0. PACKAGES
# ============================================================
library(tidyverse)
library(cowplot)
library(tidycensus)


# ============================================================
# 1. WORKING DIRECTORY + LOAD MODEL OUTPUTS
# ============================================================
setwd("~/Documents/GitHub/FIFA_worldCup_2026_risk/")

load("Data/model_outputs.RData")
# Loads: all_contributions, mc_all_sched, comparison_all,
#        top_countries_ci, city_order_main, disease_colors,
#        region_colors, mc_ranges, assign_region


# ============================================================
# 2. CENSUS DIASPORA DATA — ACS 5-year via tidycensus
# ============================================================
# Table B05006: Place of Birth for the Foreign-Born Population
# Geography: Core Based Statistical Areas (CBSAs) for the 11
#            US WC host metro areas.
#
# SETUP (run once):
#   Get a free Census API key at https://api.census.gov/data/key_signup.html
#   Then run: census_api_key("YOUR_KEY_HERE", install = TRUE)
#
# The downloaded data is cached to Data/census_diaspora_wc_cities.csv.
# Delete that file to force a fresh download.

diaspora_cache <- "Data/census_diaspora_wc_cities.csv"

# ---- 2a. WC host metro CBSA codes ---------------------------
wc_metros <- c(
  "New York"      = "35620",   # New York-Newark-Jersey City, NY-NJ-PA
  "Los Angeles"   = "31080",   # Los Angeles-Long Beach-Anaheim, CA
  "Miami"         = "33100",   # Miami-Fort Lauderdale-Pompano Beach, FL
  "San Francisco" = "41940",   # San Jose-Sunnyvale-Santa Clara, CA
  "Houston"       = "26420",   # Houston-The Woodlands-Sugar Land, TX
  "Dallas"        = "19100",   # Dallas-Fort Worth-Arlington, TX
  "Atlanta"       = "12060",   # Atlanta-Sandy Springs-Alpharetta, GA
  "Boston"        = "14460",   # Boston-Cambridge-Newton, MA-NH
  "Seattle"       = "42660",   # Seattle-Tacoma-Bellevue, WA
  "Philadelphia"  = "37980",   # Philadelphia-Camden-Wilmington, PA-NJ-DE-MD
  "Kansas City"   = "28140"    # Kansas City, MO-KS
)

# ---- 2b. Crosswalk: Census B05006 name → COR model name ----
# Census uses "Korea" not "South Korea", "Ivory Coast" not
# "Côte d'Ivoire", etc. This table bridges the two naming systems.
census_to_cor <- tribble(
  ~country_census,              ~Country,
  "Mexico",                     "Mexico",
  "Brazil",                     "Brazil",
  "Colombia",                   "Colombia",
  "Ecuador",                    "Ecuador",
  "Venezuela",                  "Venezuela",
  "Argentina",                  "Argentina",
  "Peru",                       "Peru",
  "Bolivia",                    "Bolivia",
  "Chile",                      "Chile",
  "Uruguay",                    "Uruguay",
  "Paraguay",                   "Paraguay",
  "Costa Rica",                 "Costa Rica",
  "Panama",                     "Panama",
  "Honduras",                   "Honduras",
  "Guatemala",                  "Guatemala",
  "Haiti",                      "Haiti",
  "United Kingdom",             "United Kingdom",
  "France",                     "France",
  "Germany",                    "Germany",
  "Spain",                      "Spain",
  "Netherlands",                "Netherlands",
  "Belgium",                    "Belgium",
  "Portugal",                   "Portugal",
  "Canada",                     "Canada",
  "Japan",                      "Japan",
  "Korea",                      "South Korea",
  "Australia",                  "Australia",
  "New Zealand",                "New Zealand",
  "India",                      "India",
  "Philippines",                "Philippines",
  "Iran",                       "Iran",
  "Nigeria",                    "Nigeria",
  "Ghana",                      "Ghana",
  "Cameroon",                   "Cameroon",
  "Senegal",                    "Senegal",
  "Morocco",                    "Morocco",
  "South Africa",               "South Africa",
  "Cape Verde",                 "Cape Verde",
  "Cabo Verde",                 "Cape Verde",
  "Ivory Coast",                "Côte d'Ivoire",
  "Cote d'Ivoire",              "Côte d'Ivoire",
  "Côte d'Ivoire",              "Côte d'Ivoire"
)

# ---- 2c. Download or load from cache ------------------------
if (!file.exists(diaspora_cache)) {

  message("Downloading ACS B05006 data via tidycensus...")

  # Load full B05006 variable list and identify country-level entries.
  # Leaf nodes (specific countries) have labels that do NOT end in ":"
  b05006_vars <- load_variables(2023, "acs5", cache = TRUE) %>%
    filter(str_starts(name, "B05006_")) %>%
    mutate(
      country_census = label %>%
        str_extract("[^!]+$") %>%
        str_remove(":$") %>%
        str_trim()
    ) %>%
    filter(!str_ends(label, ":"))

  # Keep only variables for countries in our crosswalk + the total (B05006_001)
  target_vars <- b05006_vars %>%
    inner_join(census_to_cor, by = "country_census") %>%
    select(variable = name, Country)

  pull_vars <- c("B05006_001", target_vars$variable)

  # Pull ACS 5-year 2019-2023 estimates for all 11 metro areas
  raw <- get_acs(
    geography = "metropolitan statistical area/micropolitan statistical area",
    variables = pull_vars,
    year      = 2023,
    survey    = "acs5",
    cache_table = TRUE
  ) %>%
    filter(GEOID %in% wc_metros)

  metro_lookup <- tibble(GEOID = wc_metros, venue_city = names(wc_metros))

  # Total foreign-born per metro (B05006_001) — used as denominator
  total_fb <- raw %>%
    filter(variable == "B05006_001") %>%
    left_join(metro_lookup, by = "GEOID") %>%
    select(venue_city, total_foreign_born = estimate)

  # Country-level counts, joined with total to compute concentration
  diaspora <- raw %>%
    filter(variable != "B05006_001") %>%
    left_join(metro_lookup,  by = "GEOID") %>%
    left_join(target_vars,   by = "variable") %>%
    select(Country, venue_city, diaspora_pop = estimate) %>%
    drop_na(Country, venue_city, diaspora_pop) %>%
    filter(diaspora_pop > 0) %>%
    left_join(total_fb, by = "venue_city") %>%
    # diaspora_conc: share of the metro's foreign-born population
    # from country c. Ranges 0-1. Interpretable as the "density"
    # of the source-country social network in that city.
    mutate(diaspora_conc = diaspora_pop / total_foreign_born)

  write_csv(diaspora, diaspora_cache)
  message("Saved to ", diaspora_cache)

} else {
  message("Loading cached ACS diaspora data from ", diaspora_cache)
  diaspora <- read_csv(diaspora_cache, show_col_types = FALSE)
}


# ============================================================
# 3. MECHANISM A — LOCAL MIXING AT VENUE CITY
# ============================================================
#
# Normalized formula:
#   Omega_A[c, v, d] = lambda[c, v, d] * (D[c, v] / FB[v])
#
# where D[c,v]/FB[v] = diaspora_conc: the share of city v's
# foreign-born population from country c (a fraction 0–1).
#
# Interpretation: Omega_A is lambda weighted by how concentrated
# the source-country community is among all immigrants in that city.
# A value of 0.04 means: "the expected number of infectious arrivals
# from c, scaled by the fact that 4% of this city's foreign-born
# residents share that national background."
#
# Omega_A stays on the same scale as lambda (expected importations)
# but discounts it when the diaspora network is small and amplifies
# it when the network is large relative to the immigrant community.

lambda_city <- all_contributions %>%
  group_by(Country, city, disease) %>%
  summarise(lambda = sum(expected_imports, na.rm = TRUE), .groups = "drop")

omega_A <- lambda_city %>%
  left_join(
    diaspora %>% select(Country, city = venue_city,
                        diaspora_pop, total_foreign_born, diaspora_conc),
    by = c("Country", "city")
  ) %>%
  drop_na(diaspora_conc) %>%
  mutate(omega_A = lambda * diaspora_conc)


# ============================================================
# 4. MECHANISM B — DIASPORA CONVERGENCE AND RETURN SEEDING
# ============================================================
#
# Normalized formula:
#   Omega_B[c, v_match, v_home, d] =
#       lambda[c, v_match, d] * (D[c, v_home] / FB[v_home])
#
# Interpretation: the importation intensity at the match venue,
# weighted by the concentration of the source-country diaspora
# in the hub city they return to. A value of 0.02 means: "given
# the expected infectious arrivals at the match venue, 2% of the
# hub city's foreign-born community shares that national background
# and could sustain onward transmission after fans return home."

lambda_match <- all_contributions %>%
  group_by(Country, city, disease) %>%
  summarise(lambda_match = sum(expected_imports, na.rm = TRUE), .groups = "drop")

diaspora_hub <- diaspora %>%
  select(Country, hub_city = venue_city, diaspora_conc_hub = diaspora_conc)

omega_B <- lambda_match %>%
  rename(match_venue = city) %>%
  # cross join: every match-venue lambda against every hub-city diaspora
  left_join(
    diaspora_hub %>% rename(Country_hub = Country),
    by = character()
  ) %>%
  filter(Country == Country_hub,       # same source country
         match_venue != hub_city) %>%  # hub must differ from match venue
  mutate(omega_B = lambda_match * diaspora_conc_hub) %>%
  select(Country, match_venue, hub_city, disease,
         lambda_match, diaspora_conc_hub, omega_B)


# ============================================================
# 5. SUMMARY TABLES
# ============================================================

# --- 5a. Top city × disease combinations by Omega_A ----------
omega_A_summary <- omega_A %>%
  group_by(city, disease) %>%
  summarise(
    omega_A   = sum(omega_A, na.rm = TRUE),
    lambda    = sum(lambda,  na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  # prob_A: probability that at least one imported case enters a
  # co-national diaspora network — Poisson CDF, same logic as P(>=1)
  # in the core model but applied to the diaspora sub-population.
  mutate(prob_A = 1 - exp(-omega_A)) %>%
  arrange(disease, desc(omega_A))

# --- 5b. Top hub cities at secondary risk (Mechanism B) ------
omega_B_summary <- omega_B %>%
  group_by(hub_city, disease) %>%
  summarise(
    omega_B = sum(omega_B, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(disease, desc(omega_B))

print(omega_A_summary)
print(omega_B_summary)


# ============================================================
# 7. VISUALIZATION COMPARISON — three candidate designs for
#    the diaspora extension figure
#
#  Option 1 — Rank-shift slopegraph
#    Shows how city rankings change from raw importation risk (λ)
#    to community-weighted risk (Ω^A) for each disease.
#    Blue lines = cities that rise in rank; red = fall; gray = same.
#
#  Option 2 — Diaspora concentration heatmap (κ only)
#    Shows the share of each city's foreign-born population from
#    each WC source country (top 15 by total λ). Pure demographics,
#    no λ scaling. Values are percentages.
#
#  Option 3 — Top country × city pairs by Ω^A
#    Bar charts of the top 8 source-country → venue-city pairs per
#    disease by Ω^A (expected importations into diaspora network).
#    Focuses on specific pairs rather than city aggregates.
# ============================================================

# ---- 7a. OPTION 1 — Rank-shift slopegraph -------------------

slope_data <- omega_A_summary %>%
  group_by(disease) %>%
  mutate(
    rank_lambda  = rank(-lambda,  ties.method = "first"),
    rank_omegaA  = rank(-omega_A, ties.method = "first"),
    rank_change  = rank_lambda - rank_omegaA,
    direction    = case_when(
      rank_change > 0 ~ "Rises",
      rank_change < 0 ~ "Falls",
      TRUE            ~ "No change"
    )
  ) %>%
  ungroup()

make_slope_panel <- function(dis) {
  d <- slope_data %>% filter(disease == dis)

  ggplot(d) +
    geom_segment(
      aes(x = 0, xend = 1,
          y = rank_lambda, yend = rank_omegaA,
          color = direction),
      linewidth = 1.3, alpha = 0.85
    ) +
    geom_point(aes(x = 0, y = rank_lambda,  color = direction), size = 2.8) +
    geom_point(aes(x = 1, y = rank_omegaA, color = direction), size = 2.8) +
    geom_text(aes(x = -0.04, y = rank_lambda,  label = city),
              hjust = 1, size = 2.6, color = "gray20") +
    geom_text(aes(x =  1.04, y = rank_omegaA, label = city),
              hjust = 0, size = 2.6, color = "gray20") +
    scale_y_reverse(breaks = 1:11, limits = c(11.5, 0.5)) +
    scale_x_continuous(
      limits = c(-1.1, 2.1),
      breaks = c(0, 1),
      labels = c("Raw risk\n(λ)", "Community\nrisk (Ω^A)")
    ) +
    scale_color_manual(
      values = c("Rises" = "#2166AC", "Falls" = "#D6604D", "No change" = "gray60"),
      guide  = "none"
    ) +
    labs(title = dis, x = "", y = "City rank") +
    theme_minimal(base_size = 9) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.text.x  = element_text(size = 8, face = "bold"),
      axis.text.y  = element_text(size = 7.5),
      axis.title.y = element_text(size = 8),
      plot.title   = element_text(face = "bold", hjust = 0.5, size = 10)
    )
}

figOpt1 <- cowplot::plot_grid(
  make_slope_panel("Dengue"),
  make_slope_panel("Influenza"),
  make_slope_panel("Pertussis"),
  make_slope_panel("Malaria"),
  make_slope_panel("Measles"),
  nrow = 1
)

# ---- 7b. OPTION 2 — Diaspora concentration heatmap (κ) ------

top_countries_list <- lambda_city %>%
  group_by(Country) %>%
  summarise(total_lambda = sum(lambda, na.rm = TRUE), .groups = "drop") %>%
  slice_max(total_lambda, n = 15) %>%
  pull(Country)

kappa_data <- diaspora %>%
  rename(city = venue_city) %>%
  filter(Country %in% top_countries_list) %>%
  mutate(
    city    = factor(city, levels = city_order_main),
    Country = reorder(Country, diaspora_conc, FUN = max),
    pct     = diaspora_conc * 100,
    label   = sprintf("%.1f", pct),
    text_col = if_else(pct > 4, "white", "gray15")
  )

figOpt2 <- ggplot(kappa_data,
                  aes(x = city, y = Country, fill = pct)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = label, color = text_col), size = 2.6) +
  scale_color_identity() +
  scale_fill_viridis_c(
    option = "plasma",
    name   = "% of city's\nforeign-born",
    breaks = c(0, 2, 5, 8),
    labels = c("0%", "2%", "5%", "8%")
  ) +
  labs(
    x        = "",
    y        = "",
    title    = "Option 2: Diaspora concentration (κ)",
    subtitle = "% of each city's foreign-born population from each WC source country (top 15 by λ)"
  ) +
  theme_minimal(base_size = 9.5) +
  theme(
    axis.text.x       = element_text(angle = 35, hjust = 1, size = 8.5),
    axis.text.y       = element_text(size = 8.5),
    panel.grid        = element_blank(),
    legend.position   = "right",
    legend.key.height = unit(1.2, "cm"),
    plot.title        = element_text(face = "bold", size = 11),
    plot.subtitle     = element_text(size = 8, color = "gray45")
  )

# ---- 7c. OPTION 3 — Top country × city pairs by Ω^A ---------

top_pairs <- omega_A %>%
  mutate(pair_label = paste0(Country, " → ", city)) %>%
  group_by(disease) %>%
  slice_max(omega_A, n = 8, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    disease = factor(disease,
                     levels = c("Dengue","Influenza","Pertussis","Malaria","Measles"))
  )

figOpt3 <- ggplot(top_pairs,
                  aes(x = reorder(pair_label, omega_A),
                      y = omega_A, fill = disease)) +
  geom_col(alpha = 0.85, width = 0.75) +
  coord_flip() +
  facet_wrap(~ disease, scales = "free", ncol = 3) +
  scale_fill_manual(values = disease_colors, guide = "none") +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.18)),
    labels = scales::number_format(accuracy = 0.001, drop0trailing = TRUE)
  ) +
  labs(
    x        = "",
    y        = "Expected importations into diaspora network (Ω^A)",
    title    = "Option 3: Top source-country → venue-city pairs by Ωᴀ",
    subtitle = "Top 8 country → city combinations per disease (schedule-driven model)"
  ) +
  theme_minimal(base_size = 9.5) +
  theme(
    strip.text         = element_text(face = "bold", size = 10),
    strip.background   = element_rect(fill = "gray96", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.title         = element_text(face = "bold", size = 11),
    plot.subtitle      = element_text(size = 8, color = "gray45")
  )

# ---- 7d. Combine all three options --------------------------

fig_comparison <- cowplot::plot_grid(
  cowplot::plot_grid(
    cowplot::ggdraw() +
      cowplot::draw_label("Option 1: City rank shifts (raw λ  →  community-weighted Ω^A)",
                          fontface = "bold", size = 11, x = 0.02, hjust = 0),
    figOpt1,
    ncol = 1, rel_heights = c(0.06, 1)
  ),
  figOpt2,
  figOpt3,
  ncol    = 1,
  labels  = c("A", "B", "C"),
  label_size = 13,
  rel_heights = c(1.1, 1.0, 1.2)
)

ggsave(fig_comparison,
       file   = "Figures/fig_diaspora_comparison.png",
       height = 22, width = 18, dpi = 300)
message("Saved Figures/fig_diaspora_comparison.png")

# ============================================================
# 6. FIGURES
# ============================================================

# ---- 6a. FIGURE A — Diaspora importation probability heatmap --
# Each cell shows P^A(>=1) = 1 - exp(-Omega_A_v_d), the probability
# that at least one imported case enters the co-national diaspora
# network in that city — directly comparable to Figure 1 (P>=1 for
# the whole city). A cell showing P=1 in Figure 1 and P=0.45 here
# means importation is certain but only 45% likely to land inside
# a co-national social network; the remainder circulates in the
# general population.

figA_data <- omega_A_summary %>%
  mutate(
    city    = factor(city,    levels = city_order_main),
    disease = factor(disease, levels = c("Dengue","Influenza",
                                          "Pertussis","Malaria","Measles")),
    label    = sprintf("%.2f", prob_A),
    text_col = if_else(prob_A < 0.5, "white", "gray15")
  )

figA <- ggplot(figA_data, aes(x = city, y = disease, fill = prob_A)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = label, color = text_col), size = 3.2) +
  scale_color_identity() +
  scale_fill_viridis_c(
    option  = "magma",
    name    = "P(≥1 importation\ninto diaspora\nnetwork)",
    limits  = c(0, 1),
    breaks  = c(0, 0.25, 0.5, 0.75, 1),
    labels  = c("0", "0.25", "0.50", "0.75", "1")
  ) +
  labs(
    x        = "",
    y        = "",
    title    = "Probability of at least one importation into local diaspora networks",
    subtitle = "P(≥1) = 1 − exp(−Ωᴀ): same Poisson logic as Figure 1, applied to co-national sub-population at each city"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x       = element_text(angle = 35, hjust = 1, size = 10),
    axis.text.y       = element_text(size = 11, face = "italic"),
    panel.grid        = element_blank(),
    legend.position   = "right",
    legend.key.height = unit(1.6, "cm"),
    plot.title        = element_text(size = 13, face = "bold"),
    plot.subtitle     = element_text(size = 9.5, color = "gray45")
  )

ggsave(figA,
       file   = "Figures/figA_diaspora_local_mixing.png",
       height = 4.5, width = 13, dpi = 300)
print(figA)

# ---- 6b. FIGURE B — Omega_B bar chart (hub city secondary risk) --
# Shows which hub cities face the highest secondary seeding risk
# (Mechanism B: diaspora members attend WC games then return home).
# Y-axis is Omega_B = lambda at match venue × diaspora_conc in hub.

figB_data <- omega_B_summary %>%
  mutate(
    disease = factor(disease, levels = c("Dengue","Influenza",
                                          "Pertussis","Malaria","Measles"))
  ) %>%
  group_by(disease) %>%
  slice_max(omega_B, n = 8, with_ties = FALSE) %>%
  ungroup()

figB <- ggplot(figB_data,
               aes(x = reorder(hub_city, omega_B),
                   y = omega_B,
                   fill = disease)) +
  geom_col(alpha = 0.85, width = 0.75) +
  coord_flip() +
  facet_wrap(~ disease, scales = "free_x", ncol = 3) +
  scale_fill_manual(values = disease_colors, guide = "none") +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15)),
    labels = scales::number_format(accuracy = 0.0001, drop0trailing = TRUE)
  ) +
  labs(
    x        = "",
    y        = "Expected importations at match venue\n× diaspora concentration in hub city",
    title    = "Expected secondary seeding risk in diaspora hub cities",
    subtitle = "Cities where diaspora members attend WC games then return home — risk lands here, not at the venue"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text         = element_text(face = "bold", size = 11),
    strip.background   = element_rect(fill = "gray96", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.title         = element_text(size = 13, face = "bold"),
    plot.subtitle      = element_text(size = 10, color = "gray45")
  )

ggsave(figB,
       file   = "Figures/figB_diaspora_hub_seeding.png",
       height = 8, width = 13, dpi = 300)
print(figB)

# ---- 6c. FIGURE C — Country contributions to diaspora-weighted risk --
# Top 10 source countries per disease ranked by expected importations
# into diaspora networks (summed across all venue cities).
# Colored by world region, same palette as Fig 4 in the main paper.
#
# Compare with Fig 4: countries whose rank rises here have high diaspora
# concentration relative to their arrival volume. Countries whose rank
# falls send many travelers but into cities where few co-nationals live.

top_countries_omegaA <- omega_A %>%
  group_by(Country, disease) %>%
  summarise(
    omega_A_total = sum(omega_A, na.rm = TRUE),
    lambda_total  = sum(lambda,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(disease) %>%
  slice_max(omega_A_total, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    new_region = assign_region(Country),
    disease    = factor(disease,
                        levels = c("Dengue","Influenza",
                                   "Pertussis","Malaria","Measles"))
  )

make_omegaA_panel <- function(dis) {
  dat <- top_countries_omegaA %>% filter(disease == dis)
  ggplot(dat, aes(x = reorder(Country, omega_A_total),
                  y = omega_A_total,
                  fill = new_region)) +
    geom_col(alpha = 0.85, width = 0.75) +
    coord_flip() +
    scale_fill_manual(values = region_colors, name = "World region") +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.22)),
      labels = scales::number_format(accuracy = 0.00001, drop0trailing = TRUE)
    ) +
    labs(
      x     = "",
      y     = "Expected importations\ninto diaspora network",
      title = dis
    ) +
    theme_minimal(base_size = 17) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      plot.title         = element_text(face = "bold", size = 18, color = "gray15"),
      legend.position    = "none"
    )
}

shared_legend_C <- cowplot::get_legend(
  ggplot(top_countries_omegaA,
         aes(x = Country, y = omega_A_total, fill = new_region)) +
    geom_col() +
    scale_fill_manual(values = region_colors, name = "World region") +
    theme_minimal(base_size = 17) +
    theme(
      legend.position = "right",
      legend.title    = element_text(size = 15, face = "bold"),
      legend.text     = element_text(size = 14),
      legend.key.size = unit(0.6, "cm")
    )
)

figC <- cowplot::plot_grid(
  make_omegaA_panel("Dengue"),
  make_omegaA_panel("Influenza"),
  make_omegaA_panel("Pertussis"),
  make_omegaA_panel("Malaria"),
  make_omegaA_panel("Measles"),
  shared_legend_C,
  ncol       = 2,
  labels     = c("A", "B", "C", "D", "E", ""),
  label_size = 14
)

ggsave(figC,
       file   = "Figures/figC_country_drivers_omegaA.png",
       height = 13, width = 15, dpi = 300)
print(figC)


# ============================================================
# DIAGNOSTIC: diaspora drivers per host city (for manuscript text)
# Prints top diaspora source communities (with kappa = diaspora_conc)
# and P^A(>=1) per city, to keep the Figure 6 narrative accurate.
# Safe to comment out once the text is finalized.
# ============================================================
cat("\n===== DENGUE: top 3 diaspora drivers per host city =====\n")
omega_A %>%
  filter(disease == "Dengue") %>%
  group_by(city) %>%
  slice_max(omega_A, n = 3, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(desc(omega_A)) %>%
  transmute(city, Country,
            kappa   = round(diaspora_conc, 3),
            omega_A = round(omega_A, 3)) %>%
  print(n = 40)

cat("\n===== MALARIA: top 3 diaspora drivers per host city =====\n")
omega_A %>%
  filter(disease == "Malaria") %>%
  group_by(city) %>%
  slice_max(omega_A, n = 3, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(desc(omega_A)) %>%
  transmute(city, Country,
            kappa   = round(diaspora_conc, 3),
            omega_A = round(omega_A, 3)) %>%
  print(n = 40)

cat("\n===== P^A(>=1) per city (Dengue & Malaria) =====\n")
omega_A_summary %>%
  filter(disease %in% c("Dengue", "Malaria")) %>%
  arrange(disease, desc(prob_A)) %>%
  transmute(disease, city,
            omega_A = round(omega_A, 3),
            prob_A  = round(prob_A, 3)) %>%
  print(n = 40)


# ============================================================
# 7. MECHANISM B EXTENSION — NON-VENUE DIASPORA HUB CITIES
# ============================================================
# Mechanism B was designed to capture secondary seeding when fans
# travel to a match and return home, INCLUDING to cities that do not
# host matches. The base analysis (Section 4) pulled ACS diaspora
# data only for the 11 venue metros, so hub_city was restricted to
# venues. Here we add major non-venue diaspora metros, pull the same
# B05006 country-of-birth data for them, and recompute Omega_B over
# the full set of hub cities (venue + non-venue). The original
# venue-only objects (omega_B, omega_B_summary) are left untouched.
#
# NOTE: requires a Census API key (same as the venue pull). The
# non-venue pull is cached to Data/census_diaspora_nonvenue_cities.csv.
# Verify the CBSA codes below if a city appears to be missing — an
# incorrect GEOID is silently dropped by the GEOID filter.

# ---- 7a. Non-venue hub metros (CBSA codes) ------------------
nonvenue_metros <- c(
  "Chicago"       = "16980",   # Chicago-Naperville-Elgin, IL-IN-WI
  "Washington DC" = "47900",   # Washington-Arlington-Alexandria, DC-VA-MD-WV
  "Minneapolis"   = "33460",   # Minneapolis-St. Paul-Bloomington, MN-WI
  "Phoenix"       = "38060",   # Phoenix-Mesa-Chandler, AZ
  "Orlando"       = "36740",   # Orlando-Kissimmee-Sanford, FL
  "San Diego"     = "41740",   # San Diego-Chula Vista-Carlsbad, CA
  "Detroit"       = "19820",   # Detroit-Warren-Dearborn, MI
  "Tampa"         = "45300",   # Tampa-St. Petersburg-Clearwater, FL
  "Denver"        = "19740",   # Denver-Aurora-Lakewood, CO
  "Charlotte"     = "16740",   # Charlotte-Concord-Gastonia, NC-SC
  "Las Vegas"     = "29820",   # Las Vegas-Henderson-Paradise, NV
  "Austin"        = "12420",   # Austin-Round Rock-Georgetown, TX
  "San Antonio"   = "41700",   # San Antonio-New Braunfels, TX
  "Portland"      = "38900",   # Portland-Vancouver-Hillsboro, OR-WA
  "Sacramento"    = "40900"    # Sacramento-Roseville-Folsom, CA
)

nonvenue_cache <- "Data/census_diaspora_nonvenue_cities.csv"

# ---- 7b. Download or load non-venue diaspora data -----------
if (!file.exists(nonvenue_cache)) {

  message("Downloading ACS B05006 data for non-venue hub metros...")

  b05006_vars_nv <- load_variables(2023, "acs5", cache = TRUE) %>%
    filter(str_starts(name, "B05006_")) %>%
    mutate(country_census = label %>%
             str_extract("[^!]+$") %>% str_remove(":$") %>% str_trim()) %>%
    filter(!str_ends(label, ":"))

  target_vars_nv <- b05006_vars_nv %>%
    inner_join(census_to_cor, by = "country_census") %>%
    select(variable = name, Country)

  pull_vars_nv <- c("B05006_001", target_vars_nv$variable)

  raw_nv <- get_acs(
    geography   = "metropolitan statistical area/micropolitan statistical area",
    variables   = pull_vars_nv,
    year        = 2023,
    survey      = "acs5",
    cache_table = TRUE
  ) %>%
    filter(GEOID %in% nonvenue_metros)

  metro_lookup_nv <- tibble(GEOID = nonvenue_metros,
                            venue_city = names(nonvenue_metros))

  total_fb_nv <- raw_nv %>%
    filter(variable == "B05006_001") %>%
    left_join(metro_lookup_nv, by = "GEOID") %>%
    select(venue_city, total_foreign_born = estimate)

  diaspora_nonvenue <- raw_nv %>%
    filter(variable != "B05006_001") %>%
    left_join(metro_lookup_nv, by = "GEOID") %>%
    left_join(target_vars_nv,  by = "variable") %>%
    select(Country, venue_city, diaspora_pop = estimate) %>%
    drop_na(Country, venue_city, diaspora_pop) %>%
    filter(diaspora_pop > 0) %>%
    left_join(total_fb_nv, by = "venue_city") %>%
    mutate(diaspora_conc = diaspora_pop / total_foreign_born)

  write_csv(diaspora_nonvenue, nonvenue_cache)
  message("Saved to ", nonvenue_cache)

} else {
  message("Loading cached non-venue diaspora data from ", nonvenue_cache)
  diaspora_nonvenue <- read_csv(nonvenue_cache, show_col_types = FALSE)
}

# ---- 7c. Combined hub set (venue + non-venue) ---------------
# Tag each hub so venue vs non-venue can be distinguished downstream.
diaspora_hub_ext <- bind_rows(
  diaspora          %>% mutate(hub_type = "venue"),
  diaspora_nonvenue %>% mutate(hub_type = "non-venue")
) %>%
  select(Country, hub_city = venue_city, hub_type,
         diaspora_conc_hub = diaspora_conc)

diaspora_hub_ext %>% select(hub_city,hub_type) %>% unique() %>% print(n=26)

# ---- 7d. Recompute Omega_B over all hub cities --------------
# lambda_match (importation intensity at the match venue) is defined
# in Section 4. The match_venue != hub_city guard still removes
# self-seeding; hubs may now be non-venue metros.
omega_B_ext <- lambda_match %>%
  rename(match_venue = city) %>%
  left_join(diaspora_hub_ext %>% rename(Country_hub = Country),
            by = character()) %>%
  filter(Country == Country_hub,
         match_venue != hub_city) %>%
  mutate(omega_B = lambda_match * diaspora_conc_hub) %>%
  select(Country, match_venue, hub_city, hub_type, disease,
         lambda_match, diaspora_conc_hub, omega_B)

omega_B_ext_summary <- omega_B_ext %>%
  group_by(hub_city, hub_type, disease) %>%
  summarise(omega_B = sum(omega_B, na.rm = TRUE), .groups = "drop") %>%
  arrange(disease, desc(omega_B))

cat("\n===== MECHANISM B (extended): top hubs incl. non-venue =====\n")
omega_B_ext_summary %>%
  filter(disease %in% c("Dengue", "Malaria")) %>%
  group_by(disease) %>%
  slice_max(omega_B, n = 12, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(omega_B = round(omega_B, 4)) %>%
  print(n = 40)

# ---- 7e. Top diaspora drivers per hub city (for manuscript) -
# For the top 8 hubs per disease, show the source countries that
# contribute most to Omega_B (summed across all match venues), with
# kappa (diaspora_conc_hub) = the hub's diaspora concentration.
top_hubs_B <- omega_B_ext_summary %>%
  filter(disease %in% c("Dengue", "Malaria")) %>%
  group_by(disease) %>%
  slice_max(omega_B, n = 8, with_ties = FALSE) %>%
  ungroup() %>%
  select(disease, hub_city)

cat("\n===== MECHANISM B (extended): top diaspora drivers per hub =====\n")
omega_B_ext %>%
  group_by(disease, hub_city, hub_type, Country) %>%
  summarise(kappa   = first(diaspora_conc_hub),
            omega_B = sum(omega_B, na.rm = TRUE), .groups = "drop") %>%
  inner_join(top_hubs_B, by = c("disease", "hub_city")) %>%
  group_by(disease, hub_city) %>%
  slice_max(omega_B, n = 2, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(disease, desc(omega_B)) %>%
  transmute(disease, hub_city, hub_type, Country,
            kappa   = round(kappa, 3),
            omega_B = round(omega_B, 3)) %>%
  print(n = 40)


# ---- 7f. Figure S5 (extended): all-disease Mechanism B ------
# Rebuilds the supplementary Mechanism B figure on the extended hub
# set (venue + non-venue), shaded by hub type, for all five diseases.
# Used by the IJID figure script as figS5. reorder_within orders bars
# within each facet (defined in the main pipeline; redefined here so
# this section also works if the diaspora script is run standalone).
if (!exists("reorder_within")) {
  reorder_within <- function(x, by, within, fun = mean, sep = "___") {
    stats::reorder(paste(x, within, sep = sep), by, FUN = fun)
  }
}

hub_type_colors <- c("Venue" = "#0072B2", "Non-venue" = "#D55E00")  # CB-safe

figB_ext_data <- omega_B_ext_summary %>%
  mutate(
    disease  = factor(disease, levels = c("Dengue", "Influenza",
                                           "Pertussis", "Malaria", "Measles")),
    hub_type = factor(if_else(hub_type == "venue", "Venue", "Non-venue"),
                      levels = c("Venue", "Non-venue"))
  ) %>%
  group_by(disease) %>%
  slice_max(omega_B, n = 15, with_ties = FALSE) %>%
  ungroup()

figB_ext <- ggplot(figB_ext_data,
                   aes(x = reorder_within(hub_city, omega_B, disease),
                       y = omega_B, fill = hub_type)) +
  geom_col(alpha = 0.9, width = 0.75) +
  coord_flip() +
  facet_wrap(~ disease, scales = "free", ncol = 3) +
  scale_x_discrete(labels = function(x) gsub("___.+$", "", x)) +
  scale_fill_manual(values = hub_type_colors, name = NULL) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15)),
    labels = scales::number_format(accuracy = 0.0001, drop0trailing = TRUE)
  ) +
  labs(x = "", y = expression(Seeding~index~(Omega[B]))) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text         = element_text(face = "bold", size = 11),
    strip.background   = element_rect(fill = "gray96", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = c(0.8,0.3)
  )

ggsave(figB_ext,
       file   = "Figures/figB_diaspora_hub_seeding_extended.png",
       height = 8, width = 13, dpi = 300)
