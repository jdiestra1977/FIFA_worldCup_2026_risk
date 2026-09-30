# FIFA World Cup 2026 — Infectious Disease Importation Risk

Code and data to reproduce a schedule-driven, uncertainty-aware framework for
estimating infectious disease importation risk at mass gathering events,
demonstrated using the June 2026 FIFA World Cup group stage as a case study.
Five diseases (dengue, malaria, measles, pertussis, influenza) are modeled
across the 11 US host cities using three nested Poisson models with Monte
Carlo uncertainty, plus a diaspora network extension estimating co-national
community risk.

**This repository contains only the code and data needed to reproduce the
calculations and figures.** Manuscript text and journal-submission materials
are kept out of GitHub and are not part of this repo.

## Pipeline

Run in this order:

1. **`Code/t100_routing_prep.R`**
   Builds city-level routing fractions from raw BTS T-100 international
   segment data. Writes `Data/t100_routing_fractions.csv`.

2. **`Code/importationRisk_main_with_uncertainty.R`**
   Core Poisson importation model (Models 1–3: Baseline, WC-adjusted,
   Schedule-driven) with 5,000-draw Monte Carlo uncertainty propagation.
   Produces the main heatmap, intensity/CI, excess-importation, and
   source-country figures, and saves `Data/model_outputs.RData` for the next
   script.

3. **`Code/importationRisk_diaspora_extension.R`**
   Loads `Data/model_outputs.RData` and estimates co-national diaspora
   network risk (Mechanism A: local social mixing; Mechanism B: diaspora
   return seeding) using US Census ACS data. Produces the diaspora figures.
   Requires a free Census API key (see below).

### Legacy scripts (not part of the current pipeline)

`Code/importationRisk_main.R` and `Code/importationRisk_world_cup2026.R` are
an earlier version of the pipeline **without** Monte Carlo uncertainty. They
are kept for reference but their outputs do not correspond to the current
published figures. Use `importationRisk_main_with_uncertainty.R` instead.

## Data sources

| File(s) | Source | In repo? |
|---|---|---|
| `Monthly_Arrivals_Country_of_Residence_COR_1.csv` | US CBP I-94 program | Yes |
| `Data_BTS/T_T100I_SEGMENT_ALL_CARRIER_{2023,2024,2025}.csv` | BTS T-100 International Segment (Form 41) | **No** — download from [transtats.bts.gov](https://www.transtats.bts.gov), table `T_T100I_SEGMENT_ALL_CARRIER` |
| `ntto_forecast_2026.csv` | National Travel and Tourism Office 2026 projections | Yes |
| `dengue-global-data-2025-12-10.xlsx` | WHO Global Dengue Surveillance | Yes |
| `Malaria_National_Unit_data.csv` | WHO World Malaria Report 2024 | Yes |
| `Measles reported cases...xlsx`, `Pertussis reported cases...xlsx` | WHO Immunization Data | Yes |
| `flunet_viwfnt.csv` | WHO FluNet GISRS | **No** — auto-downloaded and cached on first run |
| `census_diaspora_wc_cities.csv`, `census_diaspora_nonvenue_cities.csv` | US Census Bureau ACS 5-year (Table B05006), via `tidycensus` | Auto-downloaded and cached on first run (needs API key) |
| `population2020.csv`, `population2024.csv`, `population2026.xlsx` | Worldometers / Census population estimates | Yes |
| `wc2026_qualified_teams.csv`, `WorldCup2026_games_template.xlsx`, `world_cup_2026_stadiums_coordinates.csv` | Tournament schedule and venues | Yes |

## Requirements

R (tested with 4.4) and the following packages:
`tidyverse`, `readxl`, `lubridate`, `janitor`, `ggrepel`, `maps`, `cowplot`,
`tidycensus`.

For the diaspora extension, get a free Census API key at
<https://api.census.gov/data/key_signup.html> and run once:
```r
tidycensus::census_api_key("YOUR_KEY_HERE", install = TRUE)
```

## Output

Figures are written to `Figures/`. Note this folder currently also contains
older draft/journal-specific figure variants from earlier iterations of the
analysis alongside the current outputs; it has not yet been pruned to only
the current figure set.
