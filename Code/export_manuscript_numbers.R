# ============================================================
# Export every number quoted in the manuscript to one text file
# ============================================================
#
# PURPOSE
# -------
# Writes Data/manuscript_numbers.txt so that all values cited in the
# text, tables, and captions can be checked against the pipeline
# output in one place. Read-only: nothing is recomputed except
# simple sums and the dengue Spearman correlation.
#
# PREREQUISITE
# ------------
# Run in the same R session, after:
#   1. importationRisk_main.R
#   2. model1_baseline_2026.R
#   3. figure3_figureS1.R
#   4. importationRisk_diaspora_extension.R
#   5. figureS9_diaspora_combined.R
#   6. validation_dengue_2024_annual.R, validation_measles_2022_annual.R,
#      validation_malaria_2022_annual.R, validation_pertussis_2022_annual.R
# Missing objects are reported in the file instead of stopping.
# ============================================================

out_file <- "Data/manuscript_numbers.txt"
sink(out_file)
options(width = 200, pillar.sigfig = 6)

show <- function(name, expr) {
  cat("\n\n==================== ", name, " ====================\n", sep = "")
  res <- tryCatch(expr, error = function(e) paste("NOT AVAILABLE:", conditionMessage(e)))
  if (is.character(res) && length(res) == 1) {
    cat(res, "\n")
  } else if (inherits(res, "tbl_df")) {
    print(res, n = Inf)
  } else {
    print(res)
  }
}

diseases <- c("dengue", "malaria", "measles", "pertussis", "influenza")
tot <- function(mc) mc %>% summarise(median = sum(lambda_median), lo = sum(lambda_lo), hi = sum(lambda_hi))

# ---- 11-city totals and increments --------------------------
show("Three-tier 11-city totals (Monte Carlo medians) and increments", three_tier_totals)

show("Exact increments from central totals (should equal the row above)",
  tibble::tibble(
    disease = diseases,
    M1 = sapply(diseases, function(d) sum(get(paste0(d, "_m1new_results"))$importation$imp_intensity)),
    M2 = sapply(diseases, function(d) sum(get(paste0(d, "_wc_results"))$importation$imp_intensity)),
    M3 = sapply(diseases, function(d) sum(get(paste0(d, "_sched_results"))$importation$imp_intensity))
  ) %>% mutate(pct_M3_vs_M1 = 100 * (M3 / M1 - 1), pct_M2_vs_M1 = 100 * (M2 / M1 - 1),
               M1_share_of_M3 = 100 * M1 / M3))

show("M3 11-city totals with 95% CI",
  bind_rows(lapply(diseases, function(d) tot(get(paste0(d, "_mc_sched"))) %>% mutate(disease = d))))

# ---- Per-city values (Figures 1, 2, S1, S2) -----------------
for (tier in c("sched", "m1new", "wc")) {
  for (d in diseases) {
    show(paste0("Per-city ", d, " (", tier, ")"),
      get(paste0(d, "_mc_", tier)) %>%
        group_by(destination_city) %>%
        summarise(lambda_median = sum(lambda_median), lambda_lo = sum(lambda_lo),
                  lambda_hi = sum(lambda_hi), .groups = "drop") %>%
        mutate(P_ge1 = 1 - exp(-lambda_median)) %>%
        arrange(desc(lambda_median)))
  }
}

# ---- Source countries (Figure 4) ----------------------------
show("Top source countries by disease (Figure 4)", top_countries_region)

# ---- Validation (Table 2) -----------------------------------
show("Validation dengue 2024", validation_comparison_2024)
show("Spearman dengue, all nine states",
  cor.test(validation_comparison_2024$lambda_median, validation_comparison_2024$reported_cases,
           method = "spearman", exact = TRUE))
show("Spearman dengue, excluding GA",
  with(subset(validation_comparison_2024, state != "GA"),
       cor.test(lambda_median, reported_cases, method = "spearman", exact = TRUE)))
show("Validation measles 2022", validation_comparison_measles_2022)
show("Validation malaria 2022", validation_comparison_malaria_2022)
show("Validation pertussis 2022", validation_comparison_pertussis_2022)

# ---- Diaspora extension (Appendix B, Figures S5-S9) ---------
show("Mechanism A probabilities by city (Figures S7, S9A)", omega_A_summary)
show("Mechanism B hubs, all diseases (Figures S5, S9B)", omega_B_ext_summary)

sink()
message("Saved: ", out_file)
