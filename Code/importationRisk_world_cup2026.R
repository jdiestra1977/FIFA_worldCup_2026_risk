library(tidyverse)
library(readxl)

setwd("~/Documents/GitHub/FIFA_worldCup_2026_risk/")

dengue_data_world<-read_xlsx("Data/dengue-global-data-2025-12-10.xlsx")

dengue_data_world_selected<-dengue_data_world %>% 
  select(date,date_lab,who_region_long,country,cases)

dengue_data_world_selected %>% select(date,who_region_long,cases) %>%
  mutate(cases = replace_na(cases, 0)) %>%
  group_by(date,who_region_long) %>%
  summarise(total_cases=sum(cases)) %>%
  filter(date>as.Date("2024-12-31")) %>%
  ggplot(aes(x=date,y=total_cases)) + geom_col() +
  facet_wrap(~who_region_long)#,scales="free_y")

Sys.glob("*")

#https://immunizationdata.who.int
measles_data <- read_xlsx("Data/Measles reported cases and incidence 2025-09-12 14-18 UTC.xlsx")

pertusis_data <- read_xlsx("Data/Pertussis reported cases and incidence 2025-22-12 14-46 UTC.xlsx")

malaria_data_otro<-read_csv("Data/Malaria_National_Unit_data.csv")
malaria_cases<-malaria_data_otro %>% filter(Year==2024) %>% filter(Metric=="Incidence Rate") %>%
  select(Country=Name,cases_per1K=Value)

#The FIFA world cup 2026 will be hosted by three countries: USA, Mexico and Canada
#Most games will happen in USA. Dates: June 11 - July 16. I want to see what is the
#the total number of travelers that get into the country in June and July and how 
#that changes over time

### Arrivals using COR

#These are arrivals to the US, regardless of airport or city of destination
arrivals_COR <- read_csv("Data/Monthly_Arrivals_Country_of_Residence_COR_1.csv")

arrivals_long<-arrivals_COR %>%
  mutate(across(-c(Country, World_region), ~ readr::parse_number(as.character(.x)))) %>%
  pivot_longer(
    cols = -c(Country, World_region),
    names_to = "year_month",
    values_to = "value"
  ) %>%
  mutate(date = lubridate::ym(year_month)) %>%
  arrange(Country, date)

arrivals_long %>% select(date,value) %>% group_by(date) %>% drop_na() %>%
  summarise(total_visits=sum(value)) %>% filter(date>as.Date("2023-01-01")) %>%
  ggplot(aes(x=date,y=total_visits)) +
  geom_col()

total_arrivals_July_August_by_country<-arrivals_COR %>% 
  select(Country,World_region,contains("-07"),contains("-08")) %>%
  pivot_longer(cols = -c(Country,World_region),names_to = "year_month",
    values_to = "value") %>%
  mutate(year = str_sub(year_month, 1, 4)) %>%   # extract YYYY
  group_by(Country,World_region, year) %>%
  summarise(total = sum(value, na.rm = TRUE),.groups = "drop")

total_arrivals_July_August_by_country <- total_arrivals_July_August_by_country %>%
  mutate(Country = if_else(
      Country == "Zaire ( formerly Congo, Democratic Republic of)",
      "Zaire (formerly DRC)",Country)) %>%
  mutate(
    new_world_region = case_when(
      World_region %in% c("Western Europe", "Eastern Europe") ~ "Europe",
#      World_region == "Middle East" ~ "Asia",
#      World_region %in% c("Central America", "Caribbean") ~ "Central America",
      TRUE ~ World_region
    ))

library(ggplot2)
library(dplyr)
library(treemapify)
library(forcats)

# Okabe–Ito palette
okabe_ito <- c(
  "#E69F00", "#56B4E9", "#009E73",
  "#F0E442", "#0072B2", "#D55E00",
  "#CC79A7", "#999999"
)

region_totals <- total_arrivals_July_August_by_country %>%
  mutate(year = as.character(year)) %>%
  filter(year == "2025", total > 0) %>%
  group_by(new_world_region) %>%
  summarise(region_total = sum(total), .groups = "drop")

df_plot <- total_arrivals_July_August_by_country %>% select(-World_region) %>%
  mutate(year = as.character(year),
         total = as.numeric(total)) %>%
  filter(year == "2025", total > 0) %>%
  left_join(region_totals, by = "new_world_region") %>%
  mutate(new_world_region = fct_reorder(new_world_region, region_total, .desc = TRUE))

ggplot(df_plot,aes(area = total,label = Country,fill = Country)) +
  geom_treemap(color = "white", linewidth = 0.3) +
  geom_treemap_text(colour = "black",place = "centre",reflow = TRUE,min.size = 3) +
  facet_wrap(~ new_world_region, scales = "free",ncol=4) +
  scale_fill_manual(values = rep(okabe_ito, length.out = n_distinct(df_plot$Country))) +
  theme_void() +
  theme(strip.text = element_text(face = "bold", size = 15),legend.position = "none")

df_top80 <- df_plot %>% group_by(new_world_region) %>% arrange(desc(total), .by_group = TRUE) %>%
  mutate(ver = 100 * total / region_total,ver_esto = cumsum(ver),
         keep80 = ver_esto <= 80 | lag(ver_esto, default = 0) < 80) %>%
  filter(keep80) %>% select(-keep80)

df_plot80 <- df_top80 %>% group_by(new_world_region) %>%
  mutate(Country = fct_reorder(Country, total)) %>%
  ungroup()

ggplot(df_plot80, aes(x = Country, y = total)) +
  geom_col(fill = "#4E79A7", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", ver)),hjust = -0.1,size = 3) +
  coord_flip() +facet_wrap(~ new_world_region, scales = "free",ncol=4) +
  theme_minimal(base_size = 20) +
  theme(panel.grid.major.y = element_blank(),panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"),axis.title.y = element_blank()) +
  labs(y = "Total arrivals (June–July 2025)",
       title = "Top contributing countries by world region",
       subtitle = "Countries shown until cumulative share reaches ~80% of regional arrivals") +
  scale_y_continuous(labels = scales::label_scientific(digits = 2),expand = expansion(mult = c(0, 0.15)))

ggsave(last_plot(),file="Figures/countries_arriving_summer.png",width = 25,height = 12)
## Now, we start!!!

###############################################################
# Assign County + FIPS to BTS-ranked airports
# -------------------------------------------------------------
# What this script does:
#   1) Read BTS airport ranking file (contains strings like "City, ST: Airport")
#   2) Read OurAirports global airport table (ourairports.com/data/)
#   3) Parse BTS strings into city/state + airport name
#   4) Normalize airport names and match to OurAirports within a "state block"
#      (handles cross-state metros like Washington DC, NYC, etc.)
#   5) Flag weak matches for manual review
#   6) Remove NAs + a short list of manually verified wrong matches
#   7) Use coordinates to spatially join airports to U.S. counties and FIPS
#
# Required input files (in your working directory):
#   - Annual_Airport_Ranking_2023.csv
#   - airports_information.csv   (downloaded from https://ourairports.com/data/)
#
# Outputs (objects in R):
#   - matched_clean              (all matches; includes NAs)
#   - bad                        (rows to review)
#   - matched_clean_filtered     (clean matches used downstream)
#   - airports_with_county_fips  (final table with county + FIPS)
###############################################################

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(stringdist)
  library(sf)
  library(tigris)
})

options(tigris_use_cache = TRUE)

# ----------------------------
# USER SETTINGS
# ----------------------------
review_threshold <- 0.12

# I manually verified these are WRONG matches (exclude them)
bad_airport_raw <- c(
  "Metro Oakland International",
  "Bob Hope",
  "Lovell Field",
  "Snohomish County",
  "West Virginia International Yeager",
  "Eglin AFB Destin Fort Walton Beach"
)

# ----------------------------
# 1) Read input files
# ----------------------------
ranking <- read_csv("Data/Annual_Airport_Ranking_2023.csv", show_col_types = FALSE)
airports_info <- read_csv("Data/airports_information.csv", show_col_types = FALSE)

ranking %>% glimpse()
airports_info %>% glimpse()

# ----------------------------
# 2) Helper: normalize airport names for fuzzy-ish matching
#    (Keeps the match simple + robust by removing common words)
# ----------------------------
norm_airport <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("&", " and ") %>%
    str_replace_all("[^a-z0-9 ]", " ") %>%
    str_squish() %>%
    str_replace_all("\\b(international|intl|airport|field|municipal|regional|city|county)\\b", "") %>%
    str_squish()
}

# ----------------------------
# 3) Parse BTS airport strings into city/state + airport name
#    Example: "Atlanta, GA: Hartsfield-Jackson Atlanta International"
# ----------------------------
ranking2 <- ranking %>%
  mutate(
    left        = str_trim(str_split_fixed(Airport, ":", 2)[, 1]),
    airport_raw = str_trim(str_split_fixed(Airport, ":", 2)[, 2]),
    city        = str_trim(str_split_fixed(left, ",", 2)[, 1]),
    state       = str_trim(str_split_fixed(left, ",", 2)[, 2]),
    airport_key = norm_airport(airport_raw)
  )

# ----------------------------
# 4) Prepare OurAirports candidate table (US + territories, large/medium airports)
#    iso_region looks like "US-TX", "US-CA", etc.
# ----------------------------
air_us <- airports_info %>%
  filter(
    type %in% c("large_airport", "medium_airport"),
    iso_country %in% c("US", "PR", "VI", "GU", "MP")
  ) %>%
  mutate(
    state = str_replace(iso_region, "^US-", ""),   # "US-TX" -> "TX"
    airport_key = norm_airport(name)
  ) %>%
  select(
    name, iata_code, latitude_deg, longitude_deg,
    state, airport_key
  )

# ----------------------------
# 5) Define a "state block" to allow cross-state metro matching
#    (BTS labels sometimes use metro state, not physical airport state)
# ----------------------------
ranking2 <- ranking2 %>%
  mutate(
    state_block = case_when(
      state == "DC" ~ "DC|VA|MD",
      city %in% c("New York", "Newark", "White Plains", "Islip") ~ "NY|NJ|CT",
      city %in% c("Kansas City") ~ "MO|KS",
      city %in% c("Cincinnati") ~ "OH|KY|IN",
      city %in% c("St. Louis") ~ "MO|IL",
      city %in% c("Philadelphia") ~ "PA|NJ|DE",
      TRUE ~ state
    )
  )

# ----------------------------
# 6) Row-wise matcher:
#    - restrict to allowed states (state_block)
#    - compute JW distance against candidate airport_key
#    - pick the closest
# ----------------------------
match_one <- function(airport_key, state_block) {
  
  # Allowed states for this row (e.g., "DC|VA|MD")
  allowed_states <- str_split(state_block, "\\|", simplify = TRUE)
  allowed_states <- as.character(allowed_states[allowed_states != ""])
  
  # Candidate airports restricted to allowed states
  cand <- air_us %>% filter(state %in% allowed_states)
  
  # If no candidates or no key, return NA row
  if (nrow(cand) == 0 || is.na(airport_key) || airport_key == "") {
    return(tibble(
      matched_airport_name = NA_character_,
      iata_code = NA_character_,
      latitude_deg = NA_real_,
      longitude_deg = NA_real_,
      match_dist = NA_real_
    ))
  }
  
  # Compute distances; choose best candidate
  d <- stringdist::stringdist(airport_key, cand$airport_key, method = "jw")
  j <- which.min(d)
  
  tibble(
    matched_airport_name = cand$name[j],
    iata_code = cand$iata_code[j],
    latitude_deg = cand$latitude_deg[j],
    longitude_deg = cand$longitude_deg[j],
    match_dist = d[j]
  )
}

# ----------------------------
# 7) Apply matcher to all BTS airports
# ----------------------------

matched_clean <- ranking2 %>%
  rowwise() %>%
  mutate(tmp = list(match_one(airport_key, state_block))) %>%
  unnest_wider(tmp) %>%
  ungroup() %>%
  select(
    Airport,Enplaned=3, airport_raw, city, state, state_block,
    matched_airport_name, iata_code,
    latitude_deg, longitude_deg, match_dist
  )

# Diagnostics: distribution of match quality
print(summary(matched_clean$match_dist))

# ----------------------------
# 8) Flag rows for manual review (weak matches or no match)
# ----------------------------
bad <- matched_clean %>%
  filter(is.na(match_dist) | match_dist > review_threshold) %>%
  select(Airport, airport_raw, matched_airport_name, state, state_block, match_dist)

# Inspect "bad" matches (non-NA only), best-to-worst among flagged
bad %>%
  drop_na(match_dist) %>%
  arrange(match_dist) %>%
  print(n = 50)

# ----------------------------
# 9) Create filtered match table for downstream county/FIPS linking
#    - drop rows without coordinates / match
#    - remove your manually confirmed wrong matches
# ----------------------------
matched_clean_filtered <- matched_clean %>%
  drop_na(latitude_deg, longitude_deg, matched_airport_name, match_dist) %>%
  filter(!airport_raw %in% bad_airport_raw)

matched_clean_filtered %>%
  summarise(
    n_total = n(),
    max_dist = max(match_dist, na.rm = TRUE),
    n_iata_missing = sum(is.na(iata_code))
  ) %>%
  print()

# ----------------------------
# 10) County + FIPS assignment via spatial join
#     (airport point within county polygon)
# ----------------------------

# Airports -> sf points (WGS84)
airports_sf <- matched_clean_filtered %>%
  st_as_sf(coords = c("longitude_deg", "latitude_deg"), crs = 4326, remove = FALSE)

# U.S. counties polygons (Census TIGER/Cartographic Boundaries)
counties_sf <- tigris::counties(cb = TRUE, year = 2023) %>%
  select(
    county_name = NAME,
    county_fips = GEOID,   # 5-digit county FIPS
    state_fips  = STATEFP
  ) %>%
  st_transform(4326)

# Spatial join: assign county to each airport point
airports_with_county_fips <- st_join(airports_sf, counties_sf, join = st_within) %>%
  st_drop_geometry() %>%
  select(
    Airport,Enplaned, airport_raw, city, state,
    matched_airport_name, iata_code,
    latitude_deg, longitude_deg, match_dist,
    county_name, county_fips, state_fips
  )

# Sanity check
airports_with_county_fips %>%
  summarise(
    n_total = n(),
    n_with_county = sum(!is.na(county_fips)),
    n_missing_county = sum(is.na(county_fips))
  ) %>%
  print()

# Optional: save output for reuse
# write_csv(airports_with_county_fips, "airports_with_county_fips.csv")

#I have airports, volume and FIPS of location, I will estimate the probability
#of entering through that airport proportional to the volume

data_with_prob_of_entry<-airports_with_county_fips %>% 
  filter(!state %in% c("AK","HI","PR","VI","GU","MP","AS")) %>%
  select(Airport,contains("deg"),county_fips,Enplaned) %>%
  mutate(prob_entry=Enplaned/sum(Enplaned))

library(dplyr)
library(stringr)

# 1) Make sure both keys match in NAME and TYPE (two-digit character)
counties_sf2 <- counties_sf %>%
  mutate(state_fips = str_pad(as.character(state_fips), width = 2, pad = "0"))

states_sf <- tigris::states(cb = TRUE, year = 2023)  # any recent year is fine
state_lu <- states_sf %>%
  st_drop_geometry() %>%
  distinct(STATEFP, STUSPS)

state_lu2 <- state_lu %>%
  rename(state_fips = STATEFP) %>%
  mutate(state_fips = str_pad(as.character(state_fips), width = 2, pad = "0"))

# 2) Join to add STUSPS to counties_sf
counties_sf2 <- counties_sf2 %>%
  left_join(state_lu2, by = "state_fips")

# quick check
counties_sf2 %>%
  count(STUSPS, sort = TRUE) %>%
  head(10)

library(ggplot2)
library(ggrepel)
library(grid)   # for unit()

venues_position<-read_csv("Data/world_cup_2026_stadiums_coordinates.csv") %>% filter(country == "USA") %>% 
  rename("latitude_deg"="latitude","longitude_deg"="longitude")

counties_contig <- counties_sf2 %>%
  filter(!STUSPS %in% c("AK","HI","PR","VI","GU","MP","AS"))

bb <- st_bbox(counties_contig)

pad_x <- (bb$xmax - bb$xmin) * 0.02  # how far outside the border (2% of width)

venues_labels <- venues_position %>%
  transmute(
    venue = stadium,               # <-- change if needed
    lon = longitude_deg,
    lat = latitude_deg
  ) %>%
  mutate(
    side = if_else(lon < median(lon, na.rm = TRUE), "left", "right"),
    label_lon = if_else(side == "left", bb$xmin - pad_x, bb$xmax + pad_x),
    label_lat = lat
  )

venues_labels

move_label <- function(df, venue_name, new_lon = NULL, new_lat = NULL) {
  df %>%
    mutate(
      label_lon = ifelse(
        venue == venue_name & !is.null(new_lon),
        new_lon,
        label_lon
      ),
      label_lat = ifelse(
        venue == venue_name & !is.null(new_lat),
        new_lat,
        label_lat
      )
    )
}

venues_labels <- venues_labels %>%
  move_label("Gillette Stadium", new_lon = -80,new_lat = 46) %>%
  move_label("Arrowhead Stadium",new_lon = -94.5,new_lat = 40) %>%
  move_label("MetLife Stadium", new_lon = -71, new_lat = 40) %>%
  move_label("Lincoln Financial Field",new_lon = -73,new_lat = 38) %>%
  move_label("Levi's Stadium",new_lon = -124) %>%
  move_label("NRG Stadium",new_lon = -90,new_lat = 28) %>%
  move_label("AT&T Stadium",new_lon = -102,new_lat = 29) %>%
  move_label("SoFi Stadium",new_lon = -120,new_lat = 32) %>%
  move_label("Hard Rock Stadium",new_lon = -90) %>%
  move_label("Mercedes-Benz Stadium",new_lon = -79,new_lat = 32)

ggplot() +
  # ----------------------------
# Base map
# ----------------------------
geom_sf(data = counties_contig,fill = "white",color = "gray70",linewidth = 0.1) +
  # ----------------------------
# Airport points
# ----------------------------
geom_point(data = data_with_prob_of_entry %>% slice_head(n = 100),
  aes(x = longitude_deg,y = latitude_deg,size = prob_entry,fill = prob_entry),
  shape = 21,color = "black",stroke = 1,alpha = 0.6) +
  # ----------------------------
# World Cup venues (points)
# ----------------------------
geom_point(data = venues_position,aes(x = longitude_deg, y = latitude_deg),
           color = "red",size = 2) +
  # ----------------------------
# Connector lines (venue → label)
# ----------------------------
geom_segment(data = venues_labels,
             aes(x = lon, y = lat, xend = label_lon, yend = label_lat),
             linewidth = 0.4,color = "black") +
  # ----------------------------
# Venue labels (outside map) — BOXED
# ----------------------------
geom_label(data = venues_labels,
           aes(x = label_lon,y = label_lat,label = venue),
           label.size = 0.25,          # box border thickness
           label.padding = unit(0.15, "lines"),
           fill = "white",             # box fill
           color = "black",            # text color
           size = 4,
           hjust = ifelse(venues_labels$side == "left", 1, 0))+
  # ----------------------------
# Scales & styling
# ----------------------------
scale_size_continuous(range = c(1.5, 8), guide = "none") +
  scale_fill_viridis_c(name = "Probability of entry",option = "plasma",
                       guide = guide_colourbar(title.position = "top",   # title above bar
                                               title.hjust = 0.5,barwidth = unit(8, "cm"),
                                               barheight = unit(0.4, "cm"))) +
  coord_sf(datum = NA, clip = "off") +  # 🔴 IMPORTANT
  theme_void() +
  theme(legend.position = "top",legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        plot.margin = margin(0, 5, 0, 30))  # 🔴 space for outside labels

ggsave(last_plot(),file="Figures/map_probs_and_venues.png")

###

population_of_world <-read_csv("Data/population2020.csv") %>%
  rename("Country"="COUNTRY","population_country"="POPULATION") %>%
  mutate(Country=ifelse(Country=="DR Congo","Zaire (formerly DRC)",Country))

visits_and_population_countries<-total_arrivals_July_August_by_country %>% 
  filter(year=="2025") %>% mutate(Country=ifelse(Country=="Bahamas, the","Bahamas",Country)) %>%
  left_join(population_of_world) %>% drop_na() %>% rename("total_visits_to_US"="total") %>%
  select(-year)

#This is to clean and fix names in dengue data
library(dplyr)
library(lubridate)
library(cowplot)

#I am getting total cases in three months: June, July and August.
#Then, I am selecting the year with complete data, where we had the three months.
dengue_JJA_by_country_year <- dengue_data_world_selected %>% drop_na() %>%
  mutate(
    year  = year(date),
    month = month(date)
  ) %>%
  # Keep only June, July, August
  filter(month %in% c(6, 7, 8)) %>%
  group_by(country, year) %>%
  summarise(
    cases_JJA = sum(cases, na.rm = TRUE),
    n_months  = n(),                # how many months contributed (1–3)
    .groups = "drop"
  ) %>%
  arrange(country, year) %>% 
  rename("year_of_cases"="year","Country"="country")

dengue_data_world_clean<-dengue_JJA_by_country_year %>% filter(n_months==3) %>% 
  group_by(Country) %>% slice_max(year_of_cases) %>%
  mutate(
    Country = recode(
      Country,
      "Venezuela (Bolivarian Republic of)" = "Venezuela",
      "Bolivia (Plurinational State of)"   = "Bolivia",
      "Iran (Islamic Republic of)"         = "Iran",
      "United Republic of Tanzania"        = "Tanzania"
    )
  )

# #I used this to detect inconsistency in some names and feed it to the recode
# countries_of_interest<-visits_and_population_countries %>% 
#   left_join(dengue_data_world_clean %>% ungroup() %>% select(-n_months)) %>%
#   filter(is.na(cases_JJA)) %>% pull(Country)
# library(stringr)
# pattern <- str_c(countries_of_interest, collapse = "|")
# dengue_data_world_clean %>% filter(str_detect(Country, pattern))

expected_imp_dengue_country_region<-visits_and_population_countries %>%
  left_join(dengue_data_world_clean %>% ungroup() %>% select(-n_months)) %>%
  drop_na() %>%
  mutate(expected_imp_by_country=cases_JJA*(total_visits_to_US/population_country)) %>%
  mutate(prob_at_least_one=1-exp(-expected_imp_by_country)) %>%
  arrange(desc(expected_imp_by_country)) %>% 
  select(Country,World_region,expected_imp_by_country,prob_at_least_one) %>%
  mutate(Country = reorder(Country, expected_imp_by_country)) 

expected_imp_dengue_country_region <- expected_imp_dengue_country_region %>%
  mutate(
    new_world_region = case_when(
      World_region %in% c("Western Europe", "Eastern Europe") ~ "Europe",
      World_region == "Middle East" ~ "Asia",
      World_region %in% c("Central America", "Caribbean") ~ "Central America",
      TRUE ~ World_region
    )
  )

cb_palette_named <- c(
  "North America"              = "#E69F00",
  "South America"            = "#56B4E9",
  "Central America" = "#009E73",
  "Africa"              = "#F0E442",
  "Asia"     = "#0072B2",
  "Oceania"    = "#D55E00",
  "Europe"               = "#CC79A7"
)

library(scales)

exp_imp_dengue<-
expected_imp_dengue_country_region %>%
  filter(expected_imp_by_country > 0) %>%   # REQUIRED for log scale
  mutate(Country = fct_reorder(Country, expected_imp_by_country)) %>%
  head(n=30) %>%
  ggplot(aes(x = Country,y = expected_imp_by_country,fill = new_world_region)) +
  geom_col() +
  scale_y_log10(
    name   = "Expected dengue importations",
    breaks = 10^seq(-5, 3),
    labels = trans_format("log10", math_format(10^.x))) +
  scale_fill_manual(values = cb_palette_named) + coord_flip() + theme_bw() +
  theme(legend.position = c(0.7,0.3),text = element_text(size=15)) +
  labs(x = "",fill = NULL)

prob_imp_dengue<-expected_imp_dengue_country_region %>%
  filter(expected_imp_by_country >0) %>%
  head(n=30) %>%
  ggplot(aes(x = Country,y = prob_at_least_one,fill = new_world_region)) + geom_col() +
  scale_fill_manual(values = cb_palette_named) + coord_flip() + theme_bw() +
  theme(legend.position = "none",text = element_text(size=15))+#,axis.text.y = element_text(size = 10)) +
  labs(x = "",fill = NULL,y="Prob. of at least one importation")

plot_grid(exp_imp_dengue,prob_imp_dengue)

ggsave(last_plot(),file="Figures/figure_dengue.png",width=20,height = 15)

# 
# 
# expected_imp_dengue_country_region %>%
# #  filter(expected_imp_by_country>0,expected_imp_by_country < 1) %>%
#   ggplot(aes(x = Country,y = prob_at_least_one,fill = World_region)) +
#   geom_col() + coord_flip() + theme_bw() +theme(legend.position = "top")

data_with_prob_of_entry

expected_imp_country_airport <- expected_imp_dengue_country_region %>%
  select(Country, World_region, expected_imp_by_country) %>%
  tidyr::crossing(
    data_with_prob_of_entry %>%
      select(Airport, latitude_deg, longitude_deg, prob_entry)
  ) %>%
  mutate(
    expected_imp_country_airport =
      expected_imp_by_country * prob_entry
  )

expected_imp_by_airport <- expected_imp_country_airport %>%
  group_by(Airport, latitude_deg, longitude_deg) %>%
  summarise(
    expected_importations = sum(expected_imp_country_airport),
    .groups = "drop"
  )

dengue_map<-ggplot() +
  # ----------------------------
# Base map: contiguous U.S. counties
# ----------------------------
geom_sf(data = counties_contig,fill = "white",color = "gray70",linewidth = 0.1) +
  # ----------------------------
# Airport-level expected importations
# ----------------------------
geom_point(data = expected_imp_by_airport %>% 
             arrange(desc(expected_importations)) %>% slice_head(n = 100),
  aes(x = longitude_deg,y = latitude_deg,size = expected_importations,
    fill = expected_importations),
  shape = 21,color = "black",stroke = 1,alpha = 0.6) +
  # ----------------------------
# Scales
# ----------------------------
scale_size_continuous(
  #trans = "sqrt",
  name = "Expected dengue importations",range = c(1.5, 8))+
  scale_fill_viridis_c(name = "Expected dengue importations",option = "plasma") +
  # ----------------------------
# Map styling
# ----------------------------
coord_sf(datum = NA) +theme_void() +
  theme(legend.position = c(0.9, 0.4),legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)) +
  guides(size = "none",fill = guide_colourbar(title = "Expected dengue importations"))+
  # ----------------------------
# Labels
# ----------------------------
labs(title = "Expected dengue importations by U.S. airport",
  subtitle = "Country-level importation risk allocated proportionally to international arrivals")

#### Now, I want to make the same map with Measles data

visits_and_population_countries

measles_data <- read_xlsx("Data/Measles reported cases and incidence 2025-09-12 14-18 UTC.xlsx") %>%
  select(Country=1,cases_per1M=4) %>% drop_na()

measles_country_import <- visits_and_population_countries %>%
  left_join(measles_data %>% select(Country, cases_per1M), by = "Country") %>%
  mutate(cases_per1M = as.numeric(gsub(",", "", cases_per1M)),
         exp_import_country =(cases_per1M / 1e6) * total_visits_to_US) %>%
  mutate(prob_at_least_one=1-exp(-exp_import_country)) %>%
  drop_na(exp_import_country)

exp_imp_measles<-
measles_country_import %>%
  filter(exp_import_country > 0) %>%   # REQUIRED for log scale
  mutate(Country = fct_reorder(Country, exp_import_country)) %>%
  arrange(desc(exp_import_country)) %>%
  head(n=30) %>%
  ggplot(aes(x = Country,y = exp_import_country,fill = new_world_region)) +
  geom_col() + scale_y_log10(name   = "Expected measles importations") +
  # scale_y_log10(
  #   name   = "Expected dengue importations",
  #   breaks = 10^seq(-1, 0),
  #   labels = trans_format("log10", math_format(10^.x))) +
  scale_fill_manual(values = cb_palette_named) + coord_flip() + theme_bw() +
  theme(legend.position = c(0.7,0.3),text = element_text(size=15)) +
  labs(x = "",fill = NULL)

prob_imp_measles<-
measles_country_import %>%
  filter(exp_import_country >0) %>%
  mutate(Country = fct_reorder(Country, exp_import_country)) %>%
  arrange(desc(exp_import_country)) %>%
  head(n=30) %>%
  ggplot(aes(x = Country,y = prob_at_least_one,fill = new_world_region)) + geom_col() +
  scale_fill_manual(values = cb_palette_named) + coord_flip() + theme_bw() +
  theme(legend.position = "none",text = element_text(size=15))+#,axis.text.y = element_text(size = 10)) +
  labs(x = "",fill = NULL,y="Prob. of at least one importation")

plot_grid(exp_imp_measles,prob_imp_measles)

ggsave(last_plot(),file="Figures/figure_dengue.png",width=20,height = 15)

measles_country_import

# 2) Allocate those country totals across airports using prob_entry
#    E[c,a] = E[c] * prob_entry[a]
measles_country_airport <- measles_country_import %>%
  select(Country, new_world_region, total_visits_to_US, cases_per1M, exp_import_country) %>%
  crossing(
    data_with_prob_of_entry %>%
      select(Airport, latitude_deg, longitude_deg, Enplaned, prob_entry)
  ) %>%
  mutate(
    expected_measles_importations = exp_import_country * prob_entry
  )

# 3) Aggregate to AIRPORT level
measles_by_airport <- measles_country_airport %>%
  group_by(Airport, latitude_deg, longitude_deg, Enplaned, prob_entry) %>%
  summarise(
    expected_measles_importations = sum(expected_measles_importations),
    .groups = "drop"
  )

measles_map<-ggplot() +
  # ----------------------------
# Base map: contiguous U.S. counties
# ----------------------------
geom_sf(data = counties_contig,fill = "white",color = "gray70",linewidth = 0.1) +
  # ----------------------------
# Airport-level expected measles importations
# ----------------------------
geom_point(data = measles_by_airport %>% 
             arrange(desc(expected_measles_importations)) %>% slice_head(n = 100),
           aes(x = longitude_deg,y = latitude_deg,
    size = expected_measles_importations,
    fill = expected_measles_importations),
  shape = 21,color = "black",stroke = 1,alpha = 0.6) +
  # ----------------------------
# Scales
# ----------------------------
scale_size_continuous(name = "Expected measles importations",range = c(1.5, 8)) +
  scale_fill_viridis_c(name = "Expected measles importations",option = "plasma") +
  # ----------------------------
# Map styling
# ----------------------------
coord_sf(datum = NA) +theme_void() +
  theme(legend.position = c(0.9, 0.4),legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)) +
  guides(size = "none",
    fill = guide_colourbar(title = "Expected measles importations")) +
  # ----------------------------
# Labels
# ----------------------------
labs(title = "Expected measles importations by U.S. airport",
  subtitle = "Country-level importation risk allocated proportionally to international arrivals")


## Now for Malaria

malaria_cases

visits_and_population_countries %>% left_join(malaria_cases) %>% drop_na()

# 1) Country-level expected malaria importations into the US
malaria_country_import <- visits_and_population_countries %>%
  left_join(malaria_cases %>% select(Country, cases_per1K), by = "Country") %>%
  mutate(
    cases_per1K = as.numeric(cases_per1K),
    exp_import_country = (cases_per1K / 1000) * total_visits_to_US) %>%
  mutate(prob_at_least_one=1-exp(-exp_import_country)) %>%
  drop_na(exp_import_country)

exp_imp_malaria<-
malaria_country_import %>%
  filter(exp_import_country > 0) %>%   # REQUIRED for log scale
  mutate(Country = fct_reorder(Country, exp_import_country)) %>%
  arrange(desc(exp_import_country)) %>%
  head(n=30) %>%
  ggplot(aes(x = Country,y = exp_import_country,fill = new_world_region)) +
  geom_col() + scale_y_log10(name   = "Expected malaria importations") +
  # scale_y_log10(
  #   name   = "Expected dengue importations",
  #   breaks = 10^seq(-1, 0),
  #   labels = trans_format("log10", math_format(10^.x))) +
  scale_fill_manual(values = cb_palette_named) + coord_flip() + theme_bw() +
  theme(legend.position = c(0.8,0.3),text = element_text(size=15)) +
  labs(x = "",fill = NULL)

prob_imp_malaria<-
malaria_country_import %>%
  filter(exp_import_country >0) %>%
  mutate(Country = fct_reorder(Country, exp_import_country)) %>%
  arrange(desc(exp_import_country)) %>%
  head(n=30) %>%
  ggplot(aes(x = Country,y = prob_at_least_one,fill = new_world_region)) + geom_col() +
  scale_fill_manual(values = cb_palette_named) + coord_flip() + theme_bw() +
  theme(legend.position = "none",text = element_text(size=15))+#,axis.text.y = element_text(size = 10)) +
  labs(x = "",fill = NULL,y="Prob. of at least one importation")

plot_grid(exp_imp_malaria,prob_imp_malaria)

ggsave(last_plot(),file="Figures/figure_malaria.png",width=20,height = 15)

# 2) Allocate across airports using prob_entry
malaria_country_airport <- malaria_country_import %>%
  select(Country, World_region, total_visits_to_US, cases_per1K, exp_import_country) %>%
  crossing(
    data_with_prob_of_entry %>%
      select(Airport, latitude_deg, longitude_deg, Enplaned, prob_entry)
  ) %>%
  mutate(
    expected_malaria_importations = exp_import_country * prob_entry
  )

# 3) Aggregate to AIRPORT level
malaria_by_airport <- malaria_country_airport %>%
  group_by(Airport, latitude_deg, longitude_deg, Enplaned, prob_entry) %>%
  summarise(
    expected_malaria_importations = sum(expected_malaria_importations),
    .groups = "drop"
  )


malaria_map<-ggplot() +
  # ----------------------------
# Base map: contiguous U.S. counties
# ----------------------------
geom_sf(data = counties_contig,fill = "white",color = "gray70",linewidth = 0.1) +
  # ----------------------------
# Airport-level expected malaria importations
# ----------------------------
geom_point(data = malaria_by_airport %>% 
             arrange(desc(expected_malaria_importations)) %>% slice_head(n = 100),
  aes(x = longitude_deg,y = latitude_deg,
    size = expected_malaria_importations,fill = expected_malaria_importations),
  shape = 21,color = "black",stroke = 1,alpha = 0.6) +
  # ----------------------------
# Scales
# ----------------------------
scale_size_continuous(name = "Expected malaria importations",range = c(1.5, 8)) +
  scale_fill_viridis_c(name = "Expected malaria importations",option = "plasma") +
  # ----------------------------
# Map styling
# ----------------------------
coord_sf(datum = NA) + theme_void() +
  theme(legend.position = c(0.9, 0.4),legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)) +
  guides(size = "none",
         fill = guide_colourbar(title = "Expected malaria importations")) +
  # ----------------------------
# Labels
# ----------------------------
labs(title = "Expected malaria importations by U.S. airport",
  subtitle = "Country-level importation risk allocated proportionally to international arrivals")

#### Now, I want to make the same analysis for Pertussis

visits_and_population_countries

pertussis_data <- read_xlsx("Data/Pertussis reported cases and incidence 2025-22-12 14-46 UTC.xlsx") %>%
  select(Country=1,cases_per1M=4) %>% drop_na()

pertussis_country_import <- visits_and_population_countries %>%
  left_join(pertussis_data %>% select(Country, cases_per1M), by = "Country") %>%
  mutate(cases_per1M = as.numeric(gsub(",", "", cases_per1M)),
         exp_import_country =(cases_per1M / 1e6) * total_visits_to_US) %>%
  mutate(prob_at_least_one=1-exp(-exp_import_country)) %>%
  drop_na(exp_import_country)

exp_imp_pertussis<-
  pertussis_country_import %>%
  filter(exp_import_country > 0) %>%   # REQUIRED for log scale
  mutate(Country = fct_reorder(Country, exp_import_country)) %>%
  arrange(desc(exp_import_country)) %>%
  head(n=30) %>%
  ggplot(aes(x = Country,y = exp_import_country,fill = new_world_region)) +
  geom_col() + scale_y_log10(name   = "Expected pertussis importations") +
  # scale_y_log10(
  #   name   = "Expected dengue importations",
  #   breaks = 10^seq(-1, 0),
  #   labels = trans_format("log10", math_format(10^.x))) +
  scale_fill_manual(values = cb_palette_named) + coord_flip() + theme_bw() +
  theme(legend.position = c(0.7,0.3),text = element_text(size=15)) +
  labs(x = "",fill = NULL)

prob_imp_pertussis<-
  pertussis_country_import %>%
  filter(exp_import_country >0) %>%
  mutate(Country = fct_reorder(Country, exp_import_country)) %>%
  arrange(desc(exp_import_country)) %>%
  head(n=30) %>%
  ggplot(aes(x = Country,y = prob_at_least_one,fill = new_world_region)) + geom_col() +
  scale_fill_manual(values = cb_palette_named) + coord_flip() + theme_bw() +
  theme(legend.position = "none",text = element_text(size=15))+#,axis.text.y = element_text(size = 10)) +
  labs(x = "",fill = NULL,y="Prob. of at least one importation")

plot_grid(exp_imp_pertussis,prob_imp_pertussis)

plot_grid(dengue_map,malaria_map,measles_map,ncol=2)

###

plot_grid(exp_imp_dengue,exp_imp_measles,exp_imp_malaria,exp_imp_pertussis,ncol=4,
          labels = "AUTO",label_size = 30)

ggsave(last_plot(),file="Figures/expected_importations_top30.png",width = 25,height = 7)

#Data about cities in the US

# install.packages("tidycensus")
library(tidycensus)
library(dplyr)
library(sf)

# 0) Set your Census API key once (uncomment and run once)
# census_api_key("YOUR_KEY_HERE", install = TRUE)
# readRenviron("~/.Renviron")

# 1) Get US "places" (cities/towns) + population from ACS 5-year
#    NOTE: choose the year you want. Example: 2023 ACS 5-year.
cities_us <- get_acs(
  geography = "place",
  variables = c(pop = "B01003_001"),
  year = 2023,
  survey = "acs5",
  geometry = TRUE
) %>%
  select(
    GEOID,            # place FIPS (state+place code)
    NAME,             # e.g., "Austin city, Texas"
    population = estimate,
    geometry
  ) %>%
  st_transform(4326) # lon/lat CRS

# 2) (Optional) Add centroid lon/lat for mapping points
cities_us_pts <- cities_us %>%
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(
    lon = st_coordinates(centroid)[,1],
    lat = st_coordinates(centroid)[,2]
  ) %>%
  st_drop_geometry()

# View
cities_us
cities_us_pts %>% arrange(desc(population)) %>% head(10)

contig_states <- sprintf("%02d", c(
  1:56,               # all states
  72                  # PR (we will remove it)
))

# Remove AK (02), HI (15), PR (72), territories if present
contig_states <- setdiff(contig_states, c("02", "15", "72"))
cities_us_contig <- cities_us %>%
  filter(substr(GEOID, 1, 2) %in% contig_states)

cities_us_pts_contig <- cities_us_pts %>%
  filter(substr(GEOID, 1, 2) %in% contig_states)

# library(ggplot2)
# library(sf)
# library(dplyr)
library(viridis)

ggplot() +
  # ----------------------------
# City polygons
# ----------------------------
geom_sf(data = cities_us_contig,fill = "grey95",color = "white",linewidth = 0.05) +
  # ----------------------------
# City centroids
# ----------------------------
geom_point(data = cities_us_pts_contig,aes(x=lon,y=lat,size=population,color=population),alpha = 0.6) +
  # ----------------------------
# Scales
# ----------------------------
scale_size_continuous(name = "Population",range = c(0.3, 6),breaks = c(1e5, 5e5, 1e6, 5e6),labels = scales::comma) +
  scale_color_viridis_c(name = "Population",option = "plasma",trans = "log",labels = scales::comma) +
  # ----------------------------
# Styling
# ----------------------------
coord_sf(datum = NA) + theme_void() +
  theme(legend.position = "right",legend.title = element_text(size = 12),legend.text  = element_text(size = 10),
    plot.title   = element_text(size = 16, face = "bold"),plot.subtitle = element_text(size = 12)) #+
  
  # ----------------------------
# Labels
# ----------------------------
# labs(
#   title = "U.S. Cities and Population Distribution (Contiguous U.S.)",
#   subtitle = "City boundaries and population-weighted centroids",
#   caption = "Source: U.S. Census Bureau, ACS 5-year estimates (2023)"
# )

cities_us_contig %>% filter(str_detect(NAME,regex("west chicago", ignore_case = TRUE)))

# I have downloaded this data (from: https://www.trade.gov/us-international-air-travel-statistics-i-92-data)
#Columns:
##-  Date – Year: Calendar year in which the international air passenger movements were recorded.
##-  Month Number: Numeric representation of the calendar month (1 = January, …, 12 = December).
##-  Date – Month: Calendar month name corresponding to the reported passenger movements.
##-  Foreign Originating: Non-U.S. citizen passengers whose international trip begins 
#    outside the United States and who arrive to the United States.
##-  Foreign Returning: Non-U.S. citizen passengers returning (re-entering) the 
#    United States after prior travel abroad.
##-  U.S. Citizen Originating: U.S. citizen passengers departing the United States to 
#    begin international travel.
##-  U.S. Citizen Returning: U.S. citizen passengers arriving back to the United States 
#    after international travel abroad.

Sys.glob("Data/Selected_cities_and_origins/*")

ny_from_world<-read_excel("Data/Selected_cities_and_origins/data_world_to_ny.xlsx") %>% drop_na()

ny_from_africa<-read_excel("Data/Selected_cities_and_origins/data_africa_to_ny.xlsx") %>% drop_na()
ny_from_asia<-read_excel("Data/Selected_cities_and_origins/data_asia_to_ny.xlsx") %>% drop_na()
ny_from_canada<-read_excel("Data/Selected_cities_and_origins/data_canada_to_ny.xlsx") %>% drop_na()
ny_from_caribbean<-read_excel("Data/Selected_cities_and_origins/data_caribbean_to_ny.xlsx") %>% drop_na()
ny_from_centAmerica<-read_excel("Data/Selected_cities_and_origins/data_central_america_to_ny.xlsx") %>% drop_na()
ny_from_europe<-read_excel("Data/Selected_cities_and_origins/data_europe_to_ny.xlsx") %>% drop_na()
ny_from_mexico<-read_excel("Data/Selected_cities_and_origins/data_mexico_to_ny.xlsx") %>% drop_na()
ny_from_mideast<-read_excel("Data/Selected_cities_and_origins/data_mideast_to_ny.xlsx") %>% drop_na()
ny_from_oceania<-read_excel("Data/Selected_cities_and_origins/data_oceania_to_ny.xlsx") %>% drop_na()
ny_from_southAmerica<-read_excel("Data/Selected_cities_and_origins/data_south_america_to_ny.xlsx") %>% drop_na()

usa_from_world<-read_excel("Data/Selected_cities_and_origins/data_world_to_usa.xlsx") %>% drop_na()

#For correspondence country - region
arrivals_COR <- read_csv("Data/Monthly_Arrivals_Country_of_Residence_COR_1.csv")

arrivals_COR %>% filter(str_detect(Country,"uerto"))

correspondence_country_region<-arrivals_COR %>% select(Country,region=World_region) %>% 
  drop_na() %>% unique() %>%
  mutate(Country = if_else(
    Country == "Zaire ( formerly Congo, Democratic Republic of)",
    "Zaire (formerly DRC)",Country)) %>%
  mutate(
    new_region = case_when(
      region %in% c("Western Europe", "Eastern Europe") ~ "Europe",
      TRUE ~ region
    )) %>% left_join(population_of_world) %>% drop_na() %>%
  mutate(new_region=case_when(
    Country=="Mexico" ~ "Mexico",
    Country=="Canada" ~ "Canada",
    TRUE ~ new_region
  ))

population_by_region<-correspondence_country_region %>%
  select(new_region,population_country) %>%
  group_by(new_region) %>% summarise(popu_region=sum(population_country))

### Dengue data

dengue_cases_region_and_popu<-dengue_data_world_selected %>% drop_na() %>%
  mutate(year  = year(date),month = month(date)) %>%
  select(year,month,Country=country,cases) %>%
  mutate(
    Country = recode(
      Country,
      "Venezuela (Bolivarian Republic of)" = "Venezuela",
      "Bolivia (Plurinational State of)"   = "Bolivia",
      "Iran (Islamic Republic of)"         = "Iran",
      "United Republic of Tanzania"        = "Tanzania"
    )) %>% left_join(correspondence_country_region) %>% drop_na()

#population and dengue cases by region - I am excluding Canada, no cases
dengue_popu_agg_regions<-dengue_cases_region_and_popu %>% select(-region,-Country,-population_country) %>%
  group_by(year,month,new_region) %>% summarise(cases_region=sum(cases)) %>% 
  left_join(population_by_region) %>% filter(year==2024) %>% filter(new_region!="Canada")


cb_palette_named <- c(
  "Mexico"              = "#E69F00",
  "South America"            = "#56B4E9",
  "Central America" = "#009E73",
  "Africa"              = "#F0E442",
  "Asia"     = "#0072B2",
  "Middle East"    = "#D55E00",
  "Europe"               = "#CC79A7",
  "Caribbean" = "black",
  "Oceania" = "gray"
)

dengue_popu_agg_regions

usa_from_world %>% drop_na() %>% 
  mutate(all_arrivals=`Foreign Originating`+`Foreign Returning`+`U.S. Citizen Returning`) %>%
  select(year=1,month=2,all_arrivals) %>% mutate(year=as.numeric(year)) %>%
  filter(year==2024)

library(scales)

exp_imp_dengue<-
  expected_imp_dengue_country_region %>%
  filter(expected_imp_by_country > 0) %>%   # REQUIRED for log scale
  mutate(Country = fct_reorder(Country, expected_imp_by_country)) %>%
  head(n=30) %>%
  ggplot(aes(x = Country,y = expected_imp_by_country,fill = new_world_region)) +
  geom_col() +
  scale_y_log10(
    name   = "Expected dengue importations",
    breaks = 10^seq(-5, 3),
    labels = trans_format("log10", math_format(10^.x))) +
  scale_fill_manual(values = cb_palette_named) + coord_flip() + theme_bw() +
  theme(legend.position = c(0.7,0.3),text = element_text(size=15)) +
  labs(x = "",fill = NULL)



ny_from_world %>% drop_na() %>% 
  mutate(all_arrivals=`Foreign Originating`+`Foreign Returning`+`U.S. Citizen Returning`) %>%
  select(year=`Date - Year`,month=`Month Number`,all_arrivals)

library(dplyr)
library(stringr)
library(purrr)
library(readr)

# ----------------------------
# 1) Build a lookup table from your file paths
# ----------------------------
# file_vec should be your character vector of file paths
# e.g. 
file_vec <- list.files("Data/Data_International_arrivals", full.names = TRUE)

files_df <- tibble(file_path = file_vec) %>%
  mutate(
    file_name = basename(file_path),
    # extract text after "data_" and before ".xlsx"
    city_slug = str_match(file_name, "^data_(.*)\\.xlsx$")[,2],
    # replace "_" with spaces for matching
    city_query = str_replace_all(city_slug, "_", " "),
    # a nice display version (optional)
    city_pretty = str_to_title(city_query)
  ) %>%
  filter(!is.na(city_slug))

# ----------------------------
# 2) Optional: restrict airports_info to likely relevant records
# ----------------------------
# Adjust these filters to your needs:
# - Keep only airports with scheduled service and IATA codes
# - Keep only US + common territories
# ---- airports subset (adjust as needed)
air_us <- airports_info %>%
  filter(
    iso_country %in% c("US","PR","VI","GU","MP","AS"),
    type %in% c("large_airport","medium_airport")
  )

air_us_international <- air_us %>%
  filter(str_detect(name, regex("international", ignore_case = TRUE)))

# ---- matcher (DO NOT include city/file columns here)
find_airport_matches <- function(city_query, strict_international = TRUE) {
  
  base_tbl <- if (strict_international) air_us_international else air_us
  
  out <- base_tbl %>%
    filter(str_detect(name, regex(city_query, ignore_case = TRUE)) |
        str_detect(municipality, regex(city_query, ignore_case = TRUE))) %>%
    transmute(airport_name = name,municipality,iso_region,iso_country,type,iata_code,
      icao_code,latitude_deg,longitude_deg)
  
  # keep a single NA row if no matches (so you can see failures)
  if (nrow(out) == 0) {
    out <- tibble(
      airport_name = NA_character_,
      municipality = NA_character_,
      iso_region = NA_character_,
      iso_country = NA_character_,
      type = NA_character_,
      iata_code = NA_character_,
      icao_code = NA_character_,
      latitude_deg = NA_real_,
      longitude_deg = NA_real_
    )
  }
  
  out
}

# ---- run for all files/cities
airport_candidates <- files_df %>%
  mutate(matches = map(city_query, ~ find_airport_matches(.x, strict_international = TRUE))) %>%
  unnest(matches)

potential_airport_matches<-airport_candidates %>% drop_na(airport_name) %>% drop_na(iata_code) %>%
  filter(iso_country=="US") %>% drop_na()

ver_diffs<-potential_airport_matches %>% filter(city_pretty!=municipality)


repeated_paths <- potential_airport_matches %>%
  count(file_path) %>%
  filter(n > 1)

ver_esto<-potential_airport_matches %>%
  semi_join(repeated_paths, by = "file_path") %>%
  arrange(file_path, type, desc(!is.na(iata_code))) %>% print(n=35)

###

library(dplyr)
library(stringr)
library(tibble)
library(sf)

# ------------------------------------------------------------
# 1) Read file paths and extract city name from each file
# ------------------------------------------------------------
file_vec <- list.files("Data/Data_International_arrivals", full.names = TRUE)

files_df <- tibble(file_path = file_vec) %>%
  mutate(
    file_name  = basename(file_path),
    
    # text after "data_" and before ".xlsx"
    city_slug  = str_match(file_name, "^data_(.*)\\.xlsx$")[, 2],
    
    # turn underscores into spaces (for matching)
    city_query = city_slug %>%
      str_replace_all("_", " ") %>%
      str_squish() %>%
      str_to_lower(),
    
    # optional pretty display name
    city_pretty = str_to_title(city_query)
  ) %>%
  filter(!is.na(city_slug))

# ------------------------------------------------------------
# 2) Build a normalized Census city-name lookup for matching
#    (cities_us_contig must already exist as an sf object)
# ------------------------------------------------------------
cities_lu <- cities_us_contig %>%
  mutate(
    city_name = NAME %>%
      str_to_lower() %>%
      str_remove(",.*$") %>%                      # drop ", State"
      str_remove("\\s+(city|town|village)$") %>%  # drop place type
      str_squish()
  ) %>%
  select(GEOID, census_name = NAME, population, geometry, city_name)

# ------------------------------------------------------------
# 3) Match file cities to Census cities + compute centroid lon/lat
# ------------------------------------------------------------
cities_from_files <- files_df %>%
  left_join(cities_lu, by = c("city_query" = "city_name")) %>%
  filter(!is.na(GEOID)) %>%
  mutate(
    centroid = st_centroid(geometry),
    lon = st_coordinates(centroid)[, 1],
    lat = st_coordinates(centroid)[, 2]
  ) %>%
  st_drop_geometry() %>%
  select(file_path, file_name, city_slug, city_query, city_pretty,
         GEOID, census_name, population, lon, lat) %>%
  distinct()

# Result: one row per APIS file-city that successfully matches a Census place
cities_from_files %>% select(-file_path)

airports_info
######
Sys.glob("Data/Data_International_arrivals/*.xlsx")

cities_us_contig %>% 
  filter(str_detect(NAME, regex("yuma", ignore_case = TRUE)))

ranking %>% filter(str_detect(Airport,regex("yuma", ignore_case = TRUE)))
  
airports_info %>% filter(str_detect(municipality,regex("yuma", ignore_case = TRUE))) %>%
  select(type,name,municipality,iso_region) %>% filter(str_detect(type,regex("airport",ignore_case = T)))

airports_info %>% #filter(str_detect(name,regex("international",ignore_case = T))) %>%
  filter(str_detect(municipality,regex("addison", ignore_case = TRUE))) %>%
  select(type,name,municipality,iso_region)

airports_info %>% filter(str_detect(name,regex("international",ignore_case = T))) %>%
  filter(str_detect(municipality,regex("albuquerque", ignore_case = TRUE))) %>%
  select(type,name,municipality,iso_region)

airports_info %>% filter(str_detect(name,regex("international",ignore_case = T))) %>%
  filter(str_detect(municipality,regex("aspen", ignore_case = TRUE))) %>%
  select(type,name,municipality,iso_region)

name_city<-"austin"

airports_info %>% filter(str_detect(name,regex("international",ignore_case = T))) %>% #pull(iso_country) %>% unique()
#  filter(str_detect(iso_region,"US")) %>% #pull(iso_country) %>% unique()
  filter(str_detect(name,regex(name_city, ignore_case = TRUE))|
           str_detect(municipality,regex(name_city, ignore_case = TRUE))) %>%
  select(type,name,municipality,iso_region)

### Games, teams, and venues

teams_dates_venues<-read_excel("Data/WorldCup2026_games_template.xlsx")

teams_dates_venues %>% 
  mutate(Country = fct_relevel(Country, c("Mexico","Canada","USA")),
         City    = fct_reorder(City, as.numeric(Country))) %>%
  ggplot(aes(x = City, fill = Country)) + geom_bar() + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.1,0.8))

rbind(teams_dates_venues %>% 
        filter(City %in% c("East Rutherford","Philadelphia","Foxborough")) %>% select(-`Team 2`) %>%
        rename("Team"="Team 1"),
      teams_dates_venues %>% 
        filter(City %in% c("East Rutherford","Philadelphia","Foxborough")) %>% select(-`Team 1`) %>%
        rename("Team"="Team 2")) %>%
  filter(Team != "TBD")

#Source: https://dentonrc.com/sports/fifa-to-start-notifying-winners-of-world-cup-ticket-lottery/article_c9d70ecc-9758-4a05-91df-86009f382033.html
#More than 500 million ticket requests were submitted for the World Cup, FIFA announced in January
#Aside from the three host countries — the United States, Mexico and Canada — most 
#applications came from Germany, England, Brazil, Spain, Portugal, Argentina and Colombia.
#Germany and Portugal are among the teams that will play group-stage matches in Houston.  

#Also, I have a nice map of previous world cup attendance save in the directory that I 
#got from here: https://www.reuters.com/sports/soccer/us-tourism-expected-score-big-with-fifa-world-cup-2025-11-19/

countries_most_demand_tickets<-c("Argentina","Germany","England","Colombia",
                                 "Brazil","Spain","Portugal","Ecuador","Scotland")

rbind(teams_dates_venues %>% select(-`Team 2`) %>% rename("Team"="Team 1"),
      teams_dates_venues %>% select(-`Team 1`) %>% rename("Team"="Team 2")) %>%
  filter(Team %in% countries_most_demand_tickets) %>% rename("match_date"="Match Date") %>%
  arrange(match_date)

#Other piece of information that might be useful. Estimated length of stay by region
#Latin & South America: 16 nights (highest of any region).
#Europe: 14 nights.
#Asia Pacific: 13 nights.



