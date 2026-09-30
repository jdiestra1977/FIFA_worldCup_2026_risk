# ============================================================
# Table S1 — Sensitivity of expected importations to p_d
# (rho_d direction CORRECTED)
# ============================================================
#
# PREREQUISITE
# ------------
# Run importationRisk_main_with_uncertainty_rho_corrected.R first
# (through the point where dengue_mc_sched, malaria_mc_sched,
# measles_mc_sched, pertussis_mc_sched, influenza_mc_sched, and
# total_lambda() exist in the session) — this script only builds
# Table S1 from those already-corrected Monte Carlo objects.
#
# WHAT THIS REPLACES
# -------------------
# The original Table S1 generator lived in
# Submission_IJID/importationRisk_IJID.R (Section 5, "SENSITIVITY
# ANALYSIS: p_d"), which inherited the same rho_d direction bug as
# the main pipeline: it treated Lambda as proportional to rho * p,
# when rho_d should divide (see the header note in
# importationRisk_main_with_uncertainty_rho_corrected.R for the
# literature justification). This script fixes that proportionality
# throughout. The p_min/p_mid/p_max scaling itself (varying p_d
# while holding rho_d fixed at its literature midpoint) is
# unaffected by the rho direction and is unchanged from the
# original logic.
#
# Also dropped: the original script's influenza-specific rescaling
# patch, which compensated for lambda_mc having been computed under
# a since-abandoned rho_d range [0.033, 0.20]. That patch is
# unnecessary here because a fresh run of the corrected main script
# already computes influenza_mc_sched under the current, published
# range [0.01, 0.10] (see Table 1), so no retrospective rescaling
# is needed. The influenza three-way range comparison (Section 6 of
# the original script) was exploratory groundwork for choosing that
# range and is not part of reproducing the published Table S1; it
# is not reproduced here.
#
# METHOD
# ------
# Lambda is proportional to p_d / rho_d (holding all other factors
# fixed). The Monte Carlo median for a given disease
# (e.g. total_lambda(dengue_mc_sched)$median) reflects the median of
# (p_i / rho_i) under joint Uniform draws over the literature ranges,
# not the value at any single point. To convert that MC median into
# a deterministic value at (rho_mid, p_mid), we scale by the ratio of
# "(p_mid / rho_mid)" to "the median of (p_i / rho_i) under the same
# joint draws" — this correction is needed because
# median(p_i / rho_i) != p_mid / rho_mid for the ratio of two
# independent uniforms. Once Lambda(rho_mid, p_mid) is established,
# Lambda(rho_mid, p_min) and Lambda(rho_mid, p_max) follow exactly by
# linear scaling in p_d alone (rho_d held fixed throughout).
# ============================================================

set.seed(2026)
N_sim <- 1e5   # draws to estimate median(p_d / rho_d)

sens_params <- tibble(
  disease  = c("Dengue",  "Malaria", "Measles",  "Pertussis", "Influenza"),
  rho_min  = c(0.06,      0.10,      0.40,       0.01,        0.01),
  rho_max  = c(0.26,      0.35,      0.80,       0.10,        0.10),
  p_min    = c(0.30,      0.10,      0.02,       0.50,        0.30),
  p_max    = c(0.70,      0.50,      0.10,       0.90,        0.70)
) %>%
  mutate(
    rho_mid       = (rho_min + rho_max) / 2,
    p_mid         = (p_min   + p_max)   / 2,
    # median of p_d / rho_d under joint Uniform sampling
    med_p_over_rho = mapply(function(rmin, rmax, pmin, pmax)
      median(runif(N_sim, pmin, pmax) / runif(N_sim, rmin, rmax)),
      rho_min, rho_max, p_min, p_max),
    # correction converts MC median -> deterministic value at (rho_mid, p_mid)
    correction = (p_mid / rho_mid) / med_p_over_rho
  )

# MC medians (M3, schedule-driven), from the CORRECTED pipeline.
mc_ref <- tibble(
  disease   = c("Dengue",  "Malaria", "Measles",  "Pertussis",   "Influenza"),
  lambda_mc = c(
    total_lambda(dengue_mc_sched)$median,
    total_lambda(malaria_mc_sched)$median,
    total_lambda(measles_mc_sched)$median,
    tryCatch(total_lambda(pertussis_mc_sched)$median, error = function(e) NA_real_),
    tryCatch(total_lambda(influenza_mc_sched)$median, error = function(e) NA_real_)
  )
)

sens_results <- sens_params %>%
  left_join(mc_ref, by = "disease") %>%
  mutate(
    lambda_det_mid  = lambda_mc      * correction,      # at (rho_mid, p_mid)
    lambda_det_pmin = lambda_det_mid * (p_min / p_mid),  # at (rho_mid, p_min)
    lambda_det_pmax = lambda_det_mid * (p_max / p_mid),  # at (rho_mid, p_max)
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
  if (is.na(r$lambda_mc)) {
    cat(sprintf("%-10s  %5.2f  %5.2f  %5.2f  %8s  %8s  %8s  %5.1fx\n",
                r$disease, r$p_min, r$p_mid, r$p_max,
                "  n/a", "  n/a", "  n/a", r$fold))
  } else {
    cat(sprintf("%-10s  %5.2f  %5.2f  %5.2f  %8.2f  %8.2f  %8.2f  %5.1fx\n",
                r$disease, r$p_min, r$p_mid, r$p_max,
                r$lambda_det_pmin, r$lambda_det_mid, r$lambda_det_pmax, r$fold))
  }
}
cat("\nFold = p_max/p_min (exact, model-independent -- unchanged by the rho_d fix).\n")
cat("Use Lambda(p_mid) and Lambda(p_min/p_max) to populate Table S1 in the manuscript.\n")
