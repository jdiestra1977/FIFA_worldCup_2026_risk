# ============================================================
# T-100 International Segment Data — Routing Fraction Prep
# Author : Jose Herrera-Diestra
# Updated: May 2026
#
# PURPOSE
# -------
# importationRisk_main.R originally used I-92 routing fractions to
# allocate international travelers to specific US cities. I-92 covers
# only 5 gateway cities (Boston, Dallas, Houston, Newark/NY, Philadelphia).
# This script replaces those fractions with BTS T-100 International
# Segment data, which covers ALL US airports, including the 6 remaining
# US WC venue cities (Los Angeles, Atlanta, Kansas City, Miami, San
# Francisco, Seattle).
#
# T-100 records nonstop international flight segments landing at US
# airports. Passengers on connecting itineraries (e.g., São Paulo →
# Miami → Kansas City) appear only in the Miami row — not Kansas City —
# because the BTS T-100 captures the international segment entry point.
# This means Kansas City and other non-gateway venues correctly show
# near-zero routing fractions for most countries; their WC-specific
# traffic is captured separately by the schedule-driven fan stream.
#
# LIMITATION — Canadian and Mexican venue cities
# This script produces routing fractions for US airports only. Toronto,
# Vancouver, Guadalajara, Mexico City, and Monterrey would require
# Statistics Canada (Table 23-10-0079-01) and SICT/IATA data
# respectively, which are not automated here.
#
# OUTPUT
# ------
# Saves  Data/t100_routing_fractions.csv  with columns:
#   Country          — country name in COR naming convention
#   venue_city       — canonical WC venue city name
#   routing_fraction — share of that country's US-bound passengers
#                      landing at this city (mean June 2023–2025)
#   years_observed   — number of June months averaged (1–3)
#
# USAGE
# -----
# Run this script ONCE before running importationRisk_main.R.
# The output CSV is read in Section 3b of the main script.
#
# HOW TO GET THE DATA
# -------------------
# Download URL (bookmark this):
#   https://www.transtats.bts.gov/DL_SelectFields.aspx?gnoyr_VQ=FJE&QO_fu146_anzr=Nv4%20Pn44vr45
#
#   (if that redirects: transtats.bts.gov/DataIndex.asp →
#    "Air Carrier Statistics (Form 41 Traffic) — All Carriers" →
#    "T-100 International Segment (All Carriers)")
#
#   1. Select ALL of these fields:
#        YEAR  MONTH  ORIGIN_COUNTRY  ORIGIN_COUNTRY_NAME
#        Dest  DestCityName  DestCountry
#        PASSENGERS  SEATS  CARRIER_GROUP_NEW  CLASS
#        (no DEST_STATE_ABR in this table — use DestCountry instead;
#         the service class field is CLASS, not SERVICE_CLASS)
#   2. Filter Year → check 2023, 2024, 2025
#      Do NOT filter by Period — download the full year for each.
#      This script filters to June (MONTH == 6) automatically.
#   3. Click "Download" — the site delivers a .zip file
#   4. Unzip and save each CSV into Data/Data_BTS/
#      (any filename is fine; the script reads all .csv files in that folder)
#   5. Re-run this script.
# ============================================================

library(tidyverse)
library(janitor)

setwd("~/Documents/GitHub/FIFA_worldCup_2026_risk/")

# ============================================================
# 1. AIRPORT → WC VENUE CITY MAPPING
# ============================================================
# Maps destination IATA airport codes to the canonical WC venue
# city names used throughout the main model.
#
# Metro groupings:
#   New York  — JFK + EWR + LGA all serve MetLife Stadium (East Rutherford)
#   Miami     — MIA + FLL both serve Hard Rock Stadium (Miami Gardens)
#   San Fran  — SFO + OAK + SJC all serve Levi's Stadium (Santa Clara)
#   Houston   — IAH is the main international gateway; HOU is included
#               for completeness but carries almost no international traffic

airport_to_venue_city <- c(
  # New York metro (MetLife Stadium)
  "JFK" = "New York", "EWR" = "New York", "LGA" = "New York",
  # Los Angeles (SoFi Stadium)
  "LAX" = "Los Angeles",
  # Dallas (AT&T Stadium)
  "DFW" = "Dallas",  "DAL" = "Dallas",
  # Atlanta (Mercedes-Benz Stadium)
  "ATL" = "Atlanta",
  # Houston (NRG Stadium)
  "IAH" = "Houston", "HOU" = "Houston",
  # Kansas City (Arrowhead Stadium)
  "MCI" = "Kansas City",
  # Philadelphia (Lincoln Financial Field)
  "PHL" = "Philadelphia",
  # Miami metro (Hard Rock Stadium)
  "MIA" = "Miami",   "FLL" = "Miami",
  # San Francisco Bay Area (Levi's Stadium)
  "SFO" = "San Francisco", "OAK" = "San Francisco", "SJC" = "San Francisco",
  # Seattle (Lumen Field)
  "SEA" = "Seattle",
  # Boston (Gillette Stadium)
  "BOS" = "Boston"
)


# ============================================================
# 2. READ T-100 DATA (ALL FILES IN Data/Data_BTS/)
# ============================================================
# Place one or more BTS T-100 CSV files in the Data/Data_BTS/ folder.
# Any filename works — this section reads every .csv file found there,
# binds them into one table, then filters to the target years and month.
#
# DOWNLOAD URL (bookmark for future use):
#   https://www.transtats.bts.gov/DL_SelectFields.aspx?gnoyr_VQ=FJE&QO_fu146_anzr=Nv4%20Pn44vr45
#
# Steps to refresh the data:
#   1. Open the URL above in a browser
#      (if it redirects: transtats.bts.gov/DataIndex.asp
#       → "Air Carrier Statistics (Form 41 Traffic) – All Carriers"
#       → "T-100 International Segment (All Carriers)")
#   2. Select these fields:
#        YEAR  MONTH  ORIGIN_COUNTRY  ORIGIN_COUNTRY_NAME
#        Dest  DestCityName  DestCountry
#        PASSENGERS  SEATS  CARRIER_GROUP_NEW  CLASS
#      (Note: no DEST_STATE_ABR in this table; the class field is CLASS,
#       not SERVICE_CLASS)
#   3. Filter Year → 2023, 2024, 2025
#      Do NOT filter by Period — download full years.
#      This script extracts June (MONTH == 6) automatically.
#   4. Click Download, unzip, drop the CSV(s) into Data/Data_BTS/
#   5. Re-run this script.

bts_dir   <- "Data/Data_BTS/"
bts_files <- list.files(bts_dir, pattern = "\\.csv$",
                         full.names = TRUE, ignore.case = TRUE)

if (length(bts_files) == 0) {
  stop(
    "\nNo CSV files found in: ", normalizePath(bts_dir, mustWork = FALSE), "\n\n",
    "Download T-100 International Segment data from:\n",
    "  https://www.transtats.bts.gov/DL_SelectFields.aspx",
    "?gnoyr_VQ=FJE&QO_fu146_anzr=Nv4%20Pn44vr45\n\n",
    "Fields required: YEAR, MONTH, ORIGIN_COUNTRY, ORIGIN_COUNTRY_NAME,\n",
    "                 Dest, DestCityName, DestCountry, PASSENGERS, SEATS,\n",
    "                 CARRIER_GROUP_NEW, CLASS\n",
    "Years: 2023, 2024, 2025  |  Period: download full year (no period filter)\n",
    "Save all CSV file(s) to Data/Data_BTS/ and re-run.\n"
  )
}

message("Found ", length(bts_files), " file(s) in ", bts_dir)
t100_all <- map_dfr(bts_files, \(f) {
  message("  Reading: ", basename(f))
  read_csv(f, show_col_types = FALSE)
})

message("Total rows before filtering: ", scales::comma(nrow(t100_all)))

# Filter to June (month 6) for years 2023–2025.
# Three June months give stable routing fractions while staying close
# to the WC period. Adjust target_years if you have different downloads.
target_years <- c(2023L, 2024L, 2025L)

t100_raw <- t100_all %>%
  filter(YEAR %in% target_years, MONTH == 6)

message("Rows after filtering to June ", paste(target_years, collapse = "/"),
        ": ", scales::comma(nrow(t100_raw)))

# Diagnostic: confirm which year × month combos are actually present
message("\nYear × month distribution in filtered data:")
t100_raw %>%
  count(YEAR, MONTH) %>%
  arrange(YEAR) %>%
  print()


# ============================================================
# 3. PROCESS T-100 DATA
# ============================================================

# ---- 3a. Standardise and filter -----------------------------
# clean_names() converts BTS field names to snake_case.
# (e.g., DestCityName → dest_city_name, CLASS → class)
#
# Filter logic:
#   class == "F"         — scheduled passenger service only; excludes charter
#                          ("N"), all-cargo ("L"), mixed cargo ("P"), etc.
#   passengers > 0       — drop ferry / empty positioning flights
#   dest_country == "US" — keep only US-destination airports.
#                          The T-100 International table does not include a
#                          state abbreviation field; dest_country is the
#                          equivalent filter. This also excludes Puerto Rico
#                          and territories (country code != "US").

t100_clean <- t100_raw %>%
  clean_names() %>%
  filter(
    class        == "F",    # scheduled passenger service
    passengers   >  0,      # no empty / positioning flights
    dest_country == "US"    # US airports only (replaces dest_state_abr filter)
  )

# ---- 3b. Map destination airports → WC venue cities ---------
# Any airport not in airport_to_venue_city (e.g., Chicago O'Hare,
# Denver) is dropped — it is not a WC venue and not needed here.

t100_venues <- t100_clean %>%
  mutate(venue_city = airport_to_venue_city[dest]) %>%
  filter(!is.na(venue_city))

# Diagnostic: confirm airport → venue city mapping
message("\nAirport to venue city mapping found in data:")
t100_venues %>%
  distinct(dest, venue_city) %>%
  arrange(venue_city, dest) %>%
  print(n = 50)

# ---- 3c. US-total passengers per country × year × month -----
# This is the denominator for routing fractions. We count passengers
# to ALL US airports (not just WC venues) to ensure fractions are
# interpretable as true proportions of US-bound travel.

us_totals <- t100_clean %>%
  group_by(year, month, origin_country_name) %>%
  summarise(total_us_passengers = sum(passengers, na.rm = TRUE),
            .groups = "drop")

# ---- 3d. Venue-city passengers per country × year × month ---

venue_passengers <- t100_venues %>%
  group_by(year, month, origin_country_name, venue_city) %>%
  summarise(venue_passengers = sum(passengers, na.rm = TRUE),
            .groups = "drop")

# ---- 3e. Mean June routing fractions (2023–2025) ------------
# Average the routing fraction over all available June observations
# before computing the mean (not divide-then-average) so that
# fractions remain valid proportions within each year.

routing_raw <- venue_passengers %>%
  left_join(us_totals, by = c("year", "month", "origin_country_name")) %>%
  mutate(yr_fraction = venue_passengers / total_us_passengers) %>%
  group_by(origin_country_name, venue_city) %>%
  summarise(
    routing_fraction = mean(yr_fraction, na.rm = TRUE),
    years_observed   = n(),
    .groups = "drop"
  ) %>%
  filter(!is.na(routing_fraction), routing_fraction > 0)


# ============================================================
# 4. HARMONISE COUNTRY NAMES → COR NAMING CONVENTION
# ============================================================
# T-100 uses ISO/ICAO country names; CBP COR data uses informal
# short names. This recode table covers known mismatches.
# If downstream joins in the main model produce unexpected NAs,
# add the missing country pair here and re-run.

t100_to_cor <- c(
  "Korea, Republic of"                      = "South Korea",
  "Iran, Islamic Republic of"               = "Iran",
  "Bolivia, Plurinational State of"         = "Bolivia",
  "Venezuela, Bolivarian Republic of"       = "Venezuela",
  "Congo, Democratic Republic of the"      = "Zaire (formerly DRC)",
  "Tanzania, United Republic of"            = "Tanzania",
  "Viet Nam"                                = "Vietnam",
  "Russian Federation"                      = "Russia",
  "Syrian Arab Republic"                    = "Syria",
  "Lao People's Democratic Republic"        = "Laos",
  "Moldova, Republic of"                    = "Moldova",
  "Brunei Darussalam"                       = "Brunei",
  "Myanmar"                                 = "Burma (Myanmar)",
  "Czechia"                                 = "Czech Republic",
  "North Macedonia"                         = "Macedonia",
  "Palestinian Territory, Occupied"         = "Palestinian Territories",
  "Timor-Leste"                             = "East Timor",
  "Cabo Verde"                              = "Cape Verde",
  "Eswatini"                                = "Swaziland",
  "Korea, Democratic People's Republic of"  = "North Korea",
  "Cote D'Ivoire"                           = "Côte d'Ivoire",
  "Cote d'Ivoire"                           = "Côte d'Ivoire",
  "Holy See (Vatican City State)"           = "Vatican City",
  "Libyan Arab Jamahiriya"                  = "Libya",
  "Macao"                                   = "Macau",
  "Congo"                                   = "Republic of Congo"
)

t100_routing <- routing_raw %>%
  mutate(
    Country = if_else(
      origin_country_name %in% names(t100_to_cor),
      t100_to_cor[origin_country_name],
      origin_country_name   # keep as-is if no recode needed
    )
  ) %>%
  select(Country, venue_city, routing_fraction, years_observed) %>%
  arrange(Country, venue_city)


# ============================================================
# 5. VALIDATION
# ============================================================

# Routing fractions should sum to <= 1 per country
# (they sum to exactly 1 only if every passenger's destination
#  airport is a WC venue, which won't be the case for most countries)
fraction_sums <- t100_routing %>%
  group_by(Country) %>%
  summarise(total = sum(routing_fraction), .groups = "drop")

over_one <- filter(fraction_sums, total > 1.01)
if (nrow(over_one) > 0) {
  warning(
    nrow(over_one), " countries have routing fractions summing >1 — ",
    "check for duplicate airport-to-venue mappings.\n",
    "Affected: ", paste(over_one$Country, collapse = ", ")
  )
}

message("\n--- Validation summary ---")
message("Venue cities represented: ",
        paste(sort(unique(t100_routing$venue_city)), collapse = ", "))
message("Countries with T-100 routing data: ",
        n_distinct(t100_routing$Country))
message("Routing fraction range: [",
        round(min(t100_routing$routing_fraction), 5), ", ",
        round(max(t100_routing$routing_fraction), 4), "]")

# Flag countries with fewer than 3 June observations (incomplete history)
thin_data <- t100_routing %>%
  filter(years_observed < length(target_years)) %>%
  distinct(Country)

if (nrow(thin_data) > 0) {
  message("\nCountries with < 3 June observations (used with caution):")
  print(thin_data)
}

# Show top 5 countries by routing fraction for each venue city
message("\nTop source countries per venue city (highest routing fraction):")
t100_routing %>%
  group_by(venue_city) %>%
  slice_max(routing_fraction, n = 5, with_ties = FALSE) %>%
  select(venue_city, Country, routing_fraction) %>%
  print(n = Inf)


# ============================================================
# 6. SAVE OUTPUT
# ============================================================

write_csv(t100_routing, "Data/t100_routing_fractions.csv")

message("\n--- Done ---")
message("Saved: Data/t100_routing_fractions.csv")
message("Rows:  ", nrow(t100_routing),
        " (country × venue_city routing fraction pairs)")
message("Next:  Run Code/importationRisk_main.R")
