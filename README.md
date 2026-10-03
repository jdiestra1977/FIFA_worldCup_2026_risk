# FIFA World Cup 2026 — Infectious Disease Importation Risk

Code and data to reproduce a schedule-driven, uncertainty-aware framework for
estimating infectious disease importation risk at mass gathering events,
demonstrated with the June 2026 FIFA World Cup group stage. Five diseases
(dengue, malaria, measles, pertussis, influenza) are modeled at the 11 US host
cities with three nested Poisson model tiers and Monte Carlo uncertainty, plus
an exploratory diaspora network extension.

Every input is free and publicly available.

**This repository contains only the code and data needed to reproduce the
calculations and figures.** Manuscript text and journal-submission materials
are not part of this repo.

## Model tiers

| Tier | Travel volume | City routing |
|---|---|---|
| M1 — Baseline | 2026 travel without the World Cup | BTS T-100 fractions |
| M2 — WC-adjusted | 2026 projected travel | BTS T-100 fractions |
| M3 — Schedule-driven | 2026 projected travel, split into background and World Cup fan streams | T-100 for background; match schedule for fans |

## Pipeline

Open `FIFA_worldCup_2026_risk.Rproj` in RStudio first, so the working directory
is the repository root (or `setwd()` to the folder containing `Code/`, `Data/`
and `Figures/`). Then run in this order, in a single fresh R session (later
scripts use objects created by earlier ones):

| # | Script | Produces |
|---|---|---|
| 0 | `Code/t100_routing_prep.R` | `Data/t100_routing_fractions.csv` (already included; rerun only to rebuild it from the raw BTS files) |
| 1 | `Code/importationRisk_main.R` | M2 and M3, Monte Carlo summaries, Figures 1, 2, 4, S2, S3, S4, `Data/model_outputs.RData` |
| 2 | `Code/model1_baseline_2026.R` | M1 and the three-tier 11-city totals |
| 3 | `Code/figure3_figureS1.R` | Figures 3 and S1 |
| 4 | `Code/importationRisk_diaspora_extension.R` | Diaspora extension (Appendix B), Figures S5–S8 |
| 5 | `Code/figureS9_diaspora_combined.R` | Figure S9 |
| 6 | `Code/validation_dengue_2024_annual.R` | Dengue evaluation (Table 2) |
| 7 | `Code/validation_measles_2022_annual.R` | Measles evaluation (Table 2) |
| 8 | `Code/validation_malaria_2022_annual.R` | Malaria comparison (reported in the text) |
| 9 | `Code/validation_pertussis_2022_annual.R` | Pertussis comparison (reported in the text) |
| 10 | `Code/table_s1_sensitivity.R` | Table S1 |
| 11 | `Code/export_manuscript_numbers.R` | `Data/manuscript_numbers.txt`, every number quoted in the paper |

Monte Carlo draws use fixed seeds, so reruns reproduce the published values
exactly.

## Figures

All figures are written to `Figures/` as PNG (300 dpi) and vector PDF, named by
their number in the paper (`Figure1` to `Figure4`, `FigureS1` to `FigureS9`).

PDF export uses `grDevices::quartz` (macOS). On Linux or Windows, replace
`device = grDevices::quartz, type = "pdf"` with `device = cairo_pdf` in the
`ggsave()` calls.

## Data sources

| File(s) in `Data/` | Source | In repo? |
|---|---|---|
| `Monthly_Arrivals_Country_of_Residence_COR_1.csv` | US CBP I-94 arrivals by country of residence (NTTO, trade.gov) | Yes |
| `Data_BTS/T_T100I_SEGMENT_ALL_CARRIER_{2023,2024,2025}.csv` | BTS T-100 International Segment (all carriers) | **No.** Download from [transtats.bts.gov](https://www.transtats.bts.gov) (instructions in `t100_routing_prep.R`). Needed by `t100_routing_prep.R` and by the main script (background growth factor). |
| `t100_routing_fractions.csv` | Built from T-100 by `t100_routing_prep.R` | Yes |
| `ntto_forecast_2026.csv` | National Travel and Tourism Office 2026 forecasts | Yes |
| `dengue-global-data-2025-12-10.xlsx` | WHO Global Dengue Surveillance | Yes |
| `Malaria_National_Unit_data.csv` | WHO World Malaria Report 2024 | Yes |
| `Measles reported cases ... .xlsx`, `Pertussis reported cases ... .xlsx` | WHO Immunization Data | Yes |
| `flunet_viwfnt.csv` | WHO FluNet (GISRS) | **No.** Downloaded and cached on the first run. |
| `population2026.csv` | Worldometers national population projections, 2026 | Yes |
| `census_diaspora_wc_cities.csv`, `census_diaspora_nonvenue_cities.csv` | US Census Bureau ACS 5-year, Table B05006, via `tidycensus` | Yes (cached; a fresh download needs a Census API key) |
| `wc2026_qualified_teams.csv`, `WorldCup2026_games_template.xlsx`, `world_cup_2026_stadiums_coordinates.csv` | Qualified teams, group-stage schedule template (released before the playoffs), and venues | Yes |
| `model_outputs.RData`, `manuscript_numbers.txt` | Pipeline outputs | Yes |

US surveillance counts used in the evaluation scripts (CDC MMWR, NNDSS and the
2022 pertussis surveillance report) are entered directly in those scripts, with
their sources.

## Requirements

R (tested with 4.4) and the packages `tidyverse`, `readxl`, `janitor`,
`lubridate`, `cowplot`, `maps`, `ggrepel` and `tidycensus`.

The cached Census files are included. To download them again, get a free
Census API key at <https://api.census.gov/data/key_signup.html> and run once:
```r
tidycensus::census_api_key("YOUR_KEY_HERE", install = TRUE)
```

## License

The code is released under the MIT License (see `LICENSE`). Input data remain
subject to the terms of their original providers, listed under Data sources.
