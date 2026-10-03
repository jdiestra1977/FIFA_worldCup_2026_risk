# ============================================================
# Table S1 — Sensitivity of expected importations to p_d
# ============================================================
# Lambda is proportional to p_d / rho_d. The 11-city M3 total at the
# central values (rho_c, p_c) is rescaled exactly to rho_d fixed at the
# midpoint of its range and p_d at the minimum, midpoint and maximum of
# its range:
#   Lambda(rho_mid, p) = central_total * (p / rho_mid) / (p_c / rho_c)
#
# PREREQUISITE: run importationRisk_main.R first, in the same R session.
# ============================================================

sens_params <- tibble(
  disease  = c("Dengue",  "Malaria", "Measles",  "Pertussis", "Influenza"),
  rho_min  = c(0.06,      0.10,      0.40,       0.01,        0.01),
  rho_max  = c(0.26,      0.35,      0.80,       0.10,        0.10),
  p_min    = c(p_min_dengue, p_min_malaria, p_min_measles, p_min_pertussis, p_min_influenza),
  p_max    = c(p_max_dengue, p_max_malaria, p_max_measles, p_max_pertussis, p_max_influenza)
) %>%
  mutate(
    rho_mid       = (rho_min + rho_max) / 2,
    p_mid         = (p_min   + p_max)   / 2
  )

# Central 11-city M3 totals, computed at (rho_c, p_c).
central_ref <- tibble(
  disease       = c("Dengue",  "Malaria", "Measles",  "Pertussis",   "Influenza"),
  central_total = c(sum(dengue_sched_results$importation$imp_intensity),
                    sum(malaria_sched_results$importation$imp_intensity),
                    sum(measles_sched_results$importation$imp_intensity),
                    sum(pertussis_sched_results$importation$imp_intensity),
                    sum(influenza_sched_results$importation$imp_intensity)),
  rho_c         = c(under_rho_dengue, under_rho_malaria, under_rho_measles,
                    under_rho_pertussis, under_rho_influenza),
  p_c           = c(p_travel_inf_dengue, p_travel_inf_malaria, p_travel_inf_measles,
                    p_travel_inf_pertussis, p_travel_inf_influenza)
)

sens_results <- sens_params %>%
  left_join(central_ref, by = "disease") %>%
  mutate(
    lambda_det_mid  = central_total * (p_mid / rho_mid) / (p_c / rho_c),  # at (rho_mid, p_mid)
    lambda_det_pmin = lambda_det_mid * (p_min / p_mid),                   # at (rho_mid, p_min)
    lambda_det_pmax = lambda_det_mid * (p_max / p_mid),                   # at (rho_mid, p_max)
    fold            = p_max / p_min
  )

cat("\n=== TABLE S1: SENSITIVITY ANALYSIS: p_d (rho_d fixed at midpoint) ===\n")
cat("11-city total expected importations -- M3 schedule-driven model\n")
cat("Lambda values: deterministic at fixed rho_mid, varying p_d\n\n")
cat(sprintf("%-10s  %5s  %5s  %5s  %8s  %8s  %8s  %5s\n",
            "Disease", "p_min", "p_mid", "p_max",
            "L(p_min)", "L(p_mid)", "L(p_max)", "Fold"))
cat(strrep("-", 72), "\n")
for (i in seq_len(nrow(sens_results))) {
  r <- sens_results[i, ]
  cat(sprintf("%-10s  %5.2f  %5.2f  %5.2f  %8.2f  %8.2f  %8.2f  %5.1fx\n",
              r$disease, r$p_min, r$p_mid, r$p_max,
              r$lambda_det_pmin, r$lambda_det_mid, r$lambda_det_pmax, r$fold))
}
cat("\nFold = p_max/p_min.\n")
cat("Use Lambda(p_mid) and Lambda(p_min/p_max) to populate Table S1 in the manuscript.\n")
