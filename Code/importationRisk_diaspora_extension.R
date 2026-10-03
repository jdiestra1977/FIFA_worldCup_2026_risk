# ============================================================
# FIFA World Cup 2026 — Diaspora-Adjusted Importation Risk
# Exploratory extension (Appendix B)
# Author : Jose Herrera-Diestra
#
# PURPOSE
# -------
# Extends the importation model with US diaspora data (Census ACS
# 5-year, Table B05006) to estimate how much importation risk lands in
# co-national communities.
#
#   Mechanism A — local mixing at the venue city
#     Omega_A[c, v, d] = lambda[c, v, d] * kappa[c, v]
#     kappa[c, v] = share of city v's foreign-born population born in
#     country c;  P^A(>=1) = 1 - exp(-Omega_A).
#
#   Mechanism B — return seeding of diaspora hub cities
#     Omega_B[c, v_home, d] =
#       sum_{v_match != v_home} omega[c, v_match] * Omega_A[c, v_match, d] * kappa[c, v_home]
#     omega[c, v_match] = share of country c's matches played at v_match.
#     Omega_B is reported as a relative index of secondary seeding, not
#     as a probability (Appendix B).
#
# PREREQUISITES
# -------------
# 1. Run importationRisk_main.R first;
#    it writes Data/model_outputs.RData, loaded below.
# 2. A free Census API key (https://api.census.gov/data/key_signup.html),
#    installed once with census_api_key("YOUR_KEY_HERE", install = TRUE).
#    ACS data are downloaded on the first run and cached in Data/.
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
# Loads: all_contributions, mc_all_sched, top_countries_ci,
#        city_order_main, disease_colors, region_colors, mc_ranges,
#        mc_country_scales, assign_region


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

# all_contributions holds central lambda (at the reference rho_c, p_c);
# rescale to the Monte Carlo median so Omega values are on the same scale
# as the main results (mc_country_scales comes from the main pipeline).
lambda_city <- all_contributions %>%
  group_by(Country, city, disease) %>%
  summarise(lambda = sum(expected_imports, na.rm = TRUE), .groups = "drop") %>%
  left_join(select(mc_country_scales, disease, scale_mid), by = "disease") %>%
  mutate(lambda = lambda * scale_mid) %>%   # central -> Monte Carlo median
  select(-scale_mid)

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
# Formula (see header):
#   Omega_B[c, v_home, d] =
#     sum_v_match omega[c,v_match] * Omega_A[c,v_match,d] * kappa[c,v_home]
#
# omega[c, v_match] — schedule-driven share of country c's matches
# played at each US venue. Recovered from the WC-fan stream already
# computed in the main pipeline (all_contributions$stream == "WC
# fans") rather than re-reading the fixture list: the ratio of WC-fan
# expected_imports across venues for a fixed country is exactly
# omega[c,v] regardless of disease, because incidence/rho_d/p_d enter
# as a common multiplier across all venues for a given (c,d). Derived
# from Dengue's column as an arbitrary but valid representative.
schedule_weight <- all_contributions %>%
  filter(disease == "Dengue", stream == "WC fans") %>%
  group_by(Country) %>%
  mutate(omega_sched = expected_imports / sum(expected_imports)) %>%
  ungroup() %>%
  filter(omega_sched > 0) %>%
  select(Country, match_venue = city, omega_sched)

# Omega_A[c, v_match, d] at the match venue (Mechanism A, computed
# above) — the local co-national exposure hazard a visiting diaspora
# member from v_home would encounter while attending the match.
omega_A_match <- omega_A %>%
  select(Country, match_venue = city, disease, omega_A_match = omega_A)

# compute_omega_B(): applied in Section 7 to the hub set of venue and
# non-venue metros. Works whether or not diaspora_hub_df carries a
# hub_type column.
compute_omega_B <- function(diaspora_hub_df) {
  group_cols <- c("Country", "hub_city", "disease", "diaspora_conc_hub",
                   intersect("hub_type", names(diaspora_hub_df)))

  omega_A_match %>%
    inner_join(schedule_weight, by = c("Country", "match_venue")) %>%
    # cross join: every match venue's Omega_A against every hub's diaspora
    left_join(
      diaspora_hub_df %>% rename(Country_hub = Country),
      by = character()
    ) %>%
    filter(Country == Country_hub,       # same source country
           match_venue != hub_city) %>%  # exclude self-seeding (Mechanism A already covers it)
    mutate(omega_B_term = omega_sched * omega_A_match * diaspora_conc_hub) %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(omega_B = sum(omega_B_term, na.rm = TRUE), .groups = "drop")
}


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

print(omega_A_summary)


# ============================================================
# 6. FIGURES
# ============================================================

# ---- 6a. FIGURE S7 — P^A(>=1) heatmap (Mechanism A) ---------
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
  labs(x = "", y = "") +
  # No title/subtitle baked into the plot: explanatory text belongs in
  # the LaTeX caption, not duplicated in the image itself.
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x       = element_text(angle = 35, hjust = 1, size = 10),
    axis.text.y       = element_text(size = 11, face = "italic"),
    panel.grid        = element_blank(),
    legend.position   = "right",
    legend.key.height = unit(1.6, "cm"),
    # Top plot margin reserves room for the 3-line legend title, which
    # ggplot2 stacks above the color key; without it, the title is
    # clipped by the top edge of the device.
    plot.margin       = margin(t = 34, r = 8, b = 5.5, l = 5.5)
  )

ggsave(figA,
       file   = "Figures/FigureS7.png",
       height = 4.9, width = 13, dpi = 300)
# Vector PDF for journal submission (Elsevier line-art requirement)
ggsave(figA,
       file   = "Figures/FigureS7.pdf",
       height = 4.9, width = 13, device = grDevices::quartz, type = "pdf")  # macOS native vector PDF (cairo_pdf needs XQuartz)
print(figA)

# ---- 6c. FIGURE S6 — Source countries ranked by Omega_A -----
# Top 10 source countries per disease ranked by expected importations
# into diaspora networks (summed across all venue cities).
# Colored by world region, same palette as Figure 4.
#
# Compared with Figure 4, countries whose rank rises here have high diaspora
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
       file   = "Figures/FigureS6.png",
       height = 13, width = 15, dpi = 300)
# Vector PDF for journal submission (Elsevier line-art requirement)
ggsave(figC,
       file   = "Figures/FigureS6.pdf",
       height = 13, width = 15, device = grDevices::quartz, type = "pdf")  # macOS native vector PDF (cairo_pdf needs XQuartz)
print(figC)


# ============================================================
# DIAGNOSTIC: diaspora drivers per host city (Appendix B.5)
# Prints top diaspora source communities (with kappa = diaspora_conc)
# and P^A(>=1) per city.
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
# B05006 country-of-birth data for them, and compute Omega_B over
# the full set of hub cities (venue + non-venue).
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

# ---- 7d. Omega_B over all hub cities ------------------------
# compute_omega_B() from Section 4 (Omega_A at the match venue x
# schedule weight x hub kappa), applied to venue and non-venue hubs. The
# match_venue != hub_city guard (inside compute_omega_B) still
# excludes self-seeding.
omega_B_ext <- compute_omega_B(diaspora_hub_ext)

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

# ---- 7e. Top diaspora drivers per hub city (Appendix B.5) --
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


# ---- 7f. FIGURE S5 — Omega_B by hub city, all diseases -------
# Hub cities (venue + non-venue), shaded by hub type, for all five
# diseases. reorder_within orders bars
# within each facet (defined in the main pipeline; redefined here so
# this section also works if the diaspora script is run standalone).
if (!exists("reorder_within")) {
  reorder_within <- function(x, by, within, fun = mean, sep = "___") {
    stats::reorder(paste(x, within, sep = sep), by, FUN = fun)
  }
}

hub_type_colors <- c("Venue" = "#0072B2", "Non-venue" = "#D55E00")  # CB-safe

# Axis labels that stay readable when a facet's values are tiny
# (measles Omega_B is ~1e-8, which a fixed 4-decimal format shows as 0)
label_small <- function(x) {
  big <- suppressWarnings(max(abs(x), na.rm = TRUE))
  out <- if (is.finite(big) && big > 0 && big < 1e-3) {
    format(x, scientific = TRUE, digits = 2)
  } else {
    format(x, scientific = FALSE, drop0trailing = TRUE, trim = TRUE)
  }
  out[!is.na(x) & x == 0] <- "0"
  out
}

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
    labels = label_small
  ) +
  labs(x = "", y = expression(Seeding~index~(Omega[B]))) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text         = element_text(face = "bold", size = 11),
    strip.background   = element_rect(fill = "gray96", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = c(0.8,0.3),
    plot.margin        = margin(5.5, 22, 5.5, 5.5)  # room for the last axis label
  )

ggsave(figB_ext,
       file   = "Figures/FigureS5.png",
       height = 8, width = 13, dpi = 300)
# Vector PDF for journal submission (Elsevier line-art requirement)
ggsave(figB_ext,
       file   = "Figures/FigureS5.pdf",
       height = 8, width = 13, device = grDevices::quartz, type = "pdf")  # macOS native vector PDF (cairo_pdf needs XQuartz)
print(figB_ext)


# ---- 7g. FIGURE S8 — Source-country drivers of Omega_B -------
# Stacked bar chart showing which source countries contribute most to
# Omega_B at the top hub cities, for dengue and malaria.
# Top 10 hub cities per disease; top 3 source countries per hub shown.

top_n_hubs_B <- 10

figS_mechB_data <- omega_B_ext %>%
  group_by(disease, hub_city, hub_type, Country) %>%
  summarise(omega_B = sum(omega_B, na.rm = TRUE),
            kappa   = first(diaspora_conc_hub),
            .groups = "drop") %>%
  filter(disease %in% c("Dengue", "Malaria")) %>%
  group_by(disease, hub_city) %>%
  mutate(omega_B_hub_total = sum(omega_B)) %>%
  ungroup() %>%
  group_by(disease) %>%
  mutate(hub_rank = dense_rank(desc(omega_B_hub_total))) %>%
  ungroup() %>%
  filter(hub_rank <= top_n_hubs_B) %>%
  group_by(disease, hub_city) %>%
  slice_max(omega_B, n = 3, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    disease  = factor(disease, levels = c("Dengue", "Malaria")),
    hub_type = factor(if_else(hub_type == "venue", "Venue", "Non-venue"),
                      levels = c("Venue", "Non-venue"))
  )

# Okabe-Ito palette — colorblind-safe for up to 8 categories
okabe_ito <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442",
  "#0072B2", "#D55E00", "#CC79A7", "#999999"
)

figS_mechB <- ggplot(figS_mechB_data,
                     aes(x    = reorder_within(hub_city, omega_B_hub_total, disease),
                         y    = omega_B,
                         fill = Country)) +
  geom_col(alpha = 0.88, width = 0.75) +
  geom_point(aes(shape = hub_type, y = -0.003),
             size = 4, color = "gray30", show.legend = TRUE) +
  coord_flip() +
  facet_wrap(~ disease, scales = "free", ncol = 2) +
  scale_x_discrete(labels = function(x) gsub("___.+$", "", x)) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  scale_fill_manual(values = okabe_ito, name = "Source country") +
  scale_shape_manual(values = c("Venue" = 16, "Non-venue" = 17),
                     name   = "Hub type") +
  guides(fill = guide_legend(override.aes = list(shape = NA))) +
  labs(
    x    = "",
    y    = expression(Seeding~index~(Omega[B])~"by source country"),
    fill = "Source country"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text         = element_text(face = "bold", size = 11),
    strip.background   = element_rect(fill = "gray96", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "right"
  )

ggsave(figS_mechB,
       file   = "Figures/FigureS8.png",
       height = 7, width = 13, dpi = 300)
# Vector PDF for journal submission (Elsevier line-art requirement)
ggsave(figS_mechB,
       file   = "Figures/FigureS8.pdf",
       height = 7, width = 13, device = grDevices::quartz, type = "pdf")  # macOS native vector PDF (cairo_pdf needs XQuartz)

print(figS_mechB)

