# ============================================================
# M1 — Baseline: 2026 travel without the World Cup
# ============================================================
#
# M1 = N_c^bg routed by T-100 fractions, where N_c^bg is the background
# stream of M3 (main script, Sections 9b and 9d): projected June 2026
# arrivals minus the World Cup fan increment. M1 therefore includes
# ordinary travel growth to 2026, and the tier differences measure only
# the World Cup contribution:
#   M3 - M1 = World Cup increment, routed by the match schedule
#   M2 - M1 = the same increment, routed by T-100 fractions
# The two differ by N_WC * (F_c - S_c) per country (Appendix A.7).
#
# PREREQUISITE
# ------------
# Run importationRisk_main.R first, in the same R session.
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
dengue_mc_m1new    <- compute_mc_summary(dengue_m1new_results$importation$imp_intensity,    dengue_m1new_results$importation$destination_city,    under_rho_dengue,    p_travel_inf_dengue,    0.06,  0.26,  p_min_dengue,  p_max_dengue, n_mc)
malaria_mc_m1new   <- compute_mc_summary(malaria_m1new_results$importation$imp_intensity,   malaria_m1new_results$importation$destination_city,   under_rho_malaria,   p_travel_inf_malaria,   0.10,  0.35,  p_min_malaria,  p_max_malaria, n_mc)
measles_mc_m1new   <- compute_mc_summary(measles_m1new_results$importation$imp_intensity,   measles_m1new_results$importation$destination_city,   under_rho_measles,   p_travel_inf_measles,   0.40,  0.80,  p_min_measles,  p_max_measles, n_mc)
pertussis_mc_m1new <- compute_mc_summary(pertussis_m1new_results$importation$imp_intensity, pertussis_m1new_results$importation$destination_city, under_rho_pertussis, p_travel_inf_pertussis, 0.01,  0.10,  p_min_pertussis,  p_max_pertussis, n_mc)
influenza_mc_m1new <- compute_mc_summary(influenza_m1new_results$importation$imp_intensity, influenza_m1new_results$importation$destination_city, under_rho_influenza, p_travel_inf_influenza, 0.01,  0.10,  p_min_influenza,  p_max_influenza, n_mc)

# ---- 4. Three-tier comparison, 11-city totals ----
# M3 - M1 = World Cup increment, routed by the match schedule
# M2 - M1 = the same increment, routed by T-100 fractions
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
cat("                         routing (M1_new -> M3)\n")
cat("wc_increment_naive_pct = the SAME WC-specific increment under\n")
cat("                         ordinary T-100 routing (M1_new -> M2). The\n")
cat("                         two differ by N_WC * (F_c - S_c) per country\n")
cat("                         (manuscript Appendix A.7): F_c = share of\n")
cat("                         ordinary travel landing in host cities,\n")
cat("                         S_c = share of matches played in the US.\n\n")
print(three_tier_totals, n = Inf)
