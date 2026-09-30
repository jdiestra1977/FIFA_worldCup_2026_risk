# ============================================================
# Redefine M1 (Baseline): 2026 projected travel WITHOUT the World
# Cup, instead of raw, unprojected 2024 travel
# ============================================================
#
# PURPOSE
# -------
# Comment 5 from the senior collaborator: use projected 2026 travel
# without the World Cup as the baseline for estimating the
# tournament's contribution, rather than raw 2024 travel, so the
# comparison is 2026-vs-2026 (clean) instead of 2026-vs-2024
# (which would conflate the WC effect with ordinary travel growth).
#
# WHAT CHANGES
# ------------
# OLD M1 = A_c * f^T_{c,v}                      (raw 2024, no growth)
# NEW M1 = N_c^bg * f^T_{c,v}                    (2026, background
#                                                  growth only, no WC)
# M2 and M3 are UNCHANGED.
#
# N_c^bg (background-only 2026 arrivals) is NOT new: it is already
# computed in Section 9b/9d of the main script as `arrivals_bg_city`,
# for exactly this purpose (it's M3's background stream). This script
# does not duplicate any growth-factor logic -- it just runs that
# already-computed quantity through the same Poisson + Monte Carlo
# machinery used for the other two tiers, so it can stand on its own
# as a baseline rather than only existing inside M3's total.
#
# IMPORTANT CORRECTION (this script previously mislabeled this)
# ---------------------------------------------------------------
# NEW M1 already includes background growth (it IS the 2026,
# no-WC total). So the gap between NEW M1 and EITHER M2 or M3 is
# driven entirely by the WC-specific increment (phi_c - g_c), not
# by background growth -- there is no comparison in this three-tier
# setup that isolates background growth alone (that would require
# the old, raw-2024 M1 as a separate third reference point).
# M2 and M3 differ only in how that WC-specific increment is
# *routed*, not in how much of it exists:
#   M1_new vs M3  = WC-specific increment, schedule-driven routing
#                   (the accurate, realistic estimate)
#   M1_new vs M2  = WC-specific increment, naive T-100 routing
#                   (as if WC travelers dispersed like ordinary
#                   tourists instead of following the match
#                   schedule; overstates the US-specific
#                   contribution because it can't redirect fan
#                   travel to Canadian/Mexican venues)
#
# PREREQUISITE
# ------------
# Run importationRisk_main_with_uncertainty_rho_corrected.R IN FULL
# first (through Section 9, so that arrivals_bg_city, the five
# country_inc tables, the five under_rho_*/p_travel_inf_* constants,
# compute_importation_country_level(), and compute_mc_summary() all
# exist in the session). This script only adds the redefined M1 on
# top of that -- it does not recompute anything already built.
# ============================================================

# ---- 1. Reshape arrivals_bg_city for compute_importation_country_level ----
arrivals_m1_new <- arrivals_bg_city %>%
  rename(destination_city   = venue_city,
         arrivals_june_2026 = arrivals_bg)

# ---- 2. Central (deterministic) new-M1 estimates, all five diseases ----
dengue_m1new_results <- compute_importation_country_level(
  arrivals_df    = arrivals_m1_new,
  country_inc_df = dengue_june_country,
  p_travel_inf   = p_travel_inf_dengue,
  title_text     = "Dengue -- new M1 (2026, background growth only, no WC)"
)

malaria_m1new_results <- compute_importation_country_level(
  arrivals_df    = arrivals_m1_new,
  country_inc_df = malaria_country_inc,
  p_travel_inf   = p_travel_inf_malaria,
  title_text     = "Malaria -- new M1 (2026, background growth only, no WC)"
)

measles_m1new_results <- compute_importation_country_level(
  arrivals_df    = arrivals_m1_new,
  country_inc_df = measles_country_inc,
  p_travel_inf   = p_travel_inf_measles,
  title_text     = "Measles -- new M1 (2026, background growth only, no WC)"
)

pertussis_m1new_results <- compute_importation_country_level(
  arrivals_df    = arrivals_m1_new,
  country_inc_df = pertussis_country_inc,
  p_travel_inf   = p_travel_inf_pertussis,
  title_text     = "Pertussis -- new M1 (2026, background growth only, no WC)"
)

influenza_m1new_results <- compute_importation_country_level(
  arrivals_df    = arrivals_m1_new,
  country_inc_df = influenza_june_country,
  p_travel_inf   = p_travel_inf_influenza,
  title_text     = "Influenza -- new M1 (2026, background growth only, no WC)"
)

# ---- 3. Monte Carlo uncertainty, same literature ranges as the
#         other two tiers --------------------------------------------
dengue_mc_m1new    <- compute_mc_summary(dengue_m1new_results$importation$imp_intensity,    dengue_m1new_results$importation$destination_city,    under_rho_dengue,    p_travel_inf_dengue,    0.06, 0.26, 0.30, 0.70, n_mc)
malaria_mc_m1new   <- compute_mc_summary(malaria_m1new_results$importation$imp_intensity,   malaria_m1new_results$importation$destination_city,   under_rho_malaria,   p_travel_inf_malaria,   0.10, 0.35, 0.10, 0.50, n_mc)
measles_mc_m1new   <- compute_mc_summary(measles_m1new_results$importation$imp_intensity,   measles_m1new_results$importation$destination_city,   under_rho_measles,   p_travel_inf_measles,   0.40, 0.80, 0.02, 0.10, n_mc)
pertussis_mc_m1new <- compute_mc_summary(pertussis_m1new_results$importation$imp_intensity, pertussis_m1new_results$importation$destination_city, under_rho_pertussis, p_travel_inf_pertussis, 0.01, 0.10, 0.50, 0.90, n_mc)
influenza_mc_m1new <- compute_mc_summary(influenza_m1new_results$importation$imp_intensity, influenza_m1new_results$importation$destination_city, under_rho_influenza, p_travel_inf_influenza, 0.01, 0.10, 0.30, 0.70, n_mc)

# ---- 4. Three-tier comparison, 11-city totals, with the redefined M1 ----
# NEW M1 vs M3   = WC-specific increment, schedule-driven routing (accurate)
# NEW M1 vs M2   = WC-specific increment, naive T-100 routing (overstates
#                  the US-specific contribution; see header note above)
three_tier_totals <- bind_rows(
  total_lambda(dengue_mc_m1new)    %>% mutate(disease = "Dengue",    model = "M1_new"),
  total_lambda(dengue_mc_wc)       %>% mutate(disease = "Dengue",    model = "M2"),
  total_lambda(dengue_mc_sched)    %>% mutate(disease = "Dengue",    model = "M3"),
  total_lambda(malaria_mc_m1new)   %>% mutate(disease = "Malaria",   model = "M1_new"),
  total_lambda(malaria_mc_wc)      %>% mutate(disease = "Malaria",   model = "M2"),
  total_lambda(malaria_mc_sched)   %>% mutate(disease = "Malaria",   model = "M3"),
  total_lambda(measles_mc_m1new)   %>% mutate(disease = "Measles",   model = "M1_new"),
  total_lambda(measles_mc_wc)      %>% mutate(disease = "Measles",   model = "M2"),
  total_lambda(measles_mc_sched)   %>% mutate(disease = "Measles",   model = "M3"),
  total_lambda(pertussis_mc_m1new) %>% mutate(disease = "Pertussis", model = "M1_new"),
  total_lambda(pertussis_mc_wc)    %>% mutate(disease = "Pertussis", model = "M2"),
  total_lambda(pertussis_mc_sched) %>% mutate(disease = "Pertussis", model = "M3"),
  total_lambda(influenza_mc_m1new) %>% mutate(disease = "Influenza", model = "M1_new"),
  total_lambda(influenza_mc_wc)    %>% mutate(disease = "Influenza", model = "M2"),
  total_lambda(influenza_mc_sched) %>% mutate(disease = "Influenza", model = "M3")
) %>%
  select(disease, model, median) %>%
  pivot_wider(names_from = model, values_from = median) %>%
  mutate(
    wc_increment_naive_pct = 100 * (M2 - M1_new) / M1_new,   # M1_new -> M2
    wc_increment_pct       = 100 * (M3 - M1_new) / M1_new    # M1_new -> M3
  )

cat("\n=== THREE-TIER COMPARISON WITH REDEFINED M1 (2026, no WC) ===\n")
cat("wc_increment_pct       = WC-specific increment, schedule-driven\n")
cat("                         routing (M1_new -> M3, the accurate estimate)\n")
cat("wc_increment_naive_pct = the SAME WC-specific increment, but under\n")
cat("                         naive T-100 routing (M1_new -> M2), which\n")
cat("                         overstates it because it can't redirect fan\n")
cat("                         travel to Canadian/Mexican venues. Neither\n")
cat("                         column isolates background growth alone --\n")
cat("                         M1_new already includes it.\n\n")
print(three_tier_totals, n = Inf)
