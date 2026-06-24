# ============================================================
# FIFA World Cup 2026 — IJID Submission Figure Pipeline
#
# Author : Jose Herrera-Diestra
# Created: June 2026
#
# PURPOSE
# -------
# Sources the full main pipeline (importationRisk_main_with_uncertainty.R)
# and the diaspora extension (importationRisk_diaspora_extension.R), then
# re-saves all publication figures with the _IJID suffix for submission
# to the International Journal of Infectious Diseases (IJID).
#
# USAGE
# -----
# Set your working directory to the project root and run:
#   source("Submission_IJID/importationRisk_IJID.R")
# Or open this file and run line by line interactively.
#
# PREREQUISITES
# -------------
# 1. All data files in Data/ must be present (see README).
# 2. A Census API key must be installed for the diaspora section:
#    census_api_key("YOUR_KEY_HERE", install = TRUE)
#    (required only once; data is cached to Data/census_diaspora_wc_cities.csv)
# ============================================================


# ============================================================
# 1. WORKING DIRECTORY
# ============================================================
setwd("~/Documents/GitHub/FIFA_worldCup_2026_risk/")


# ============================================================
# 2. RUN MAIN PIPELINE (M1 – M3 + MC uncertainty)
# ============================================================
# Produces all model objects, MC summaries, and saves original
# figures to Figures/. Also writes Data/model_outputs.RData
# for the diaspora extension.
source("Code/importationRisk_main_with_uncertainty.R")


# ============================================================
# 3. RUN DIASPORA EXTENSION (Mechanisms A and B)
# ============================================================
# Loads model_outputs.RData (already in memory from step 2 —
# the load() call inside the script is harmless), downloads or
# reads cached ACS B05006 diaspora data, computes Omega_A and
# Omega_B, and saves original diaspora figures to Figures/.
source("Code/importationRisk_diaspora_extension.R")


# ============================================================
# 4. SAVE PUBLICATION FIGURES WITH _IJID SUFFIX
# ============================================================
# The sourced scripts already saved figures under their default
# names. Here we re-save only the figures that appear in the
# IJID manuscript (main text + supplementary) with the _IJID
# suffix so they are clearly identified as the submission set.
#
# Main text figures
# -----------------
#   Fig 1 — Risk heatmap             (P≥1, schedule-driven M3)
#   Fig 2 — Expected importations    (Lambda ± 95% CI, M3)
#   Fig 3 — Three-model comparison   (M1 / M2 / M3, top 6 cities)
#   Fig 4 — WC excess stacked bar    (baseline vs. WC increment, all diseases)
#   Fig 5 — Source-country drivers   (top 10 per disease, M3)
#   Fig 6 — Diaspora two-panel       (Mechanism A heatmap + Mechanism B bar, dengue & malaria)
#
# Supplementary figures
# ---------------------
#   Fig S1 — Baseline heatmap        (P≥1, M1)
#   Fig S2 — WC-adjusted heatmap     (P≥1, M2)
#   Fig S3 — CI asymmetry plot       (upside vs. downside ratio)
#   Fig S4 — Venue map               (16 WC stadiums)
#   Fig S5 — Hub seeding bar chart   (Omega^B, Mechanism B, all 5 diseases)
#   Fig S6 — Country drivers Omega^A (top 10 per disease, M3 diaspora)
#   Fig S7 — Full diaspora heatmap   (P^A≥1, all 5 diseases, Mechanism A)

# ============================================================
# 3b. IJID REVISION: colorblind palettes + remove plot titles
# ============================================================
# Per collaborator request:
#   (1) Remove figure-level titles and subtitles (the manuscript
#       captions already carry this information).
#   (2) Ensure colorblind-safe palettes. The heatmaps already use the
#       viridis/magma scales, which are colorblind-safe by design. The
#       only categorical palette that was NOT colorblind-safe was
#       region_colors (several near-identical greens/teals plus
#       red/orange collisions); it is replaced here with the
#       Okabe-Ito palette. Per-disease panel names are kept because
#       they identify each panel.

# ---- Colorblind-safe region palette (Okabe-Ito) -------------
region_colors <- c(
  "Africa"        = "#E69F00",  # orange
  "Latin America" = "#009E73",  # bluish green
  "Caribbean"     = "#56B4E9",  # sky blue
  "Europe"        = "#0072B2",  # blue
  "Asia"          = "#CC79A7",  # reddish purple
  "North America" = "#D55E00",  # vermillion (Canada + Mexico, the WC co-hosts)
  "Mideast"       = "#F0E442",  # yellow
  "Middle East"   = "#F0E442",  # yellow
  "Oceania"       = "#999999",  # medium gray
  "Other"         = "#DDDDDD"   # light gray
)

# ---- Colorblind-safe disease palette (Okabe-Ito) -----------
# Replaces the original red/green pairing (Dengue/Measles), which
# was indistinguishable under red-green color vision deficiency.
# Set before fig6 is built (Section 4b) so its panels inherit it.
disease_colors <- c(
  "Dengue"    = "#D55E00",  # vermillion
  "Influenza" = "#0072B2",  # blue
  "Pertussis" = "#CC79A7",  # reddish purple
  "Malaria"   = "#E69F00",  # orange
  "Measles"   = "#009E73"   # bluish green
)

# ---- Colorblind-safe host-nation palette (venue map) -------
host_nation_colors_cb <- c(
  "USA"    = "#0072B2",  # blue
  "Canada" = "#D55E00",  # vermillion
  "Mexico" = "#009E73"   # bluish green
)

# ---- Helper: strip title + subtitle from a single ggplot ----
strip_titles <- function(p) p + labs(title = NULL, subtitle = NULL)

# ---- Helper: colorblind cividis fill for P(>=1) heatmaps ----
# cividis is optimized specifically for color-vision deficiency and
# is near-identical for CVD and non-CVD viewers. Replaces the
# plasma/magma scales baked into the heatmaps during sourcing.
cividis_prob <- function(name   = expression(P(X >= 1)),
                         labels = c("0.00", "0.25", "0.50", "0.75", "1.00")) {
  scale_fill_viridis_c(option = "cividis", name = name,
                       limits = c(0, 1),
                       breaks = c(0, 0.25, 0.5, 0.75, 1),
                       labels = labels)
}

# ---- Fix: legend (colorbar) title clipped at the figure edge ----
# The P(X>=1) title sits atop the colorbar and was cropped by the
# top/right image boundary. Add margin so it is no longer clipped.
legend_crop_fix <- theme(plot.margin = margin(t = 16, r = 14, b = 6, l = 6))

# ---- Collapse Canada + Mexico into "North America" ----------
# Consistency fix: every other category is a continent/macro-region,
# so the two co-host countries are grouped into a single "North
# America" region rather than carrying their own colors. The USA is
# the destination and never appears as a source country.
to_north_america <- function(x) if_else(x %in% c("Canada", "Mexico"),
                                         "North America", x)
top_countries_region <- top_countries_region %>%
  mutate(new_region = to_north_america(new_region))
top_countries_omegaA <- top_countries_omegaA %>%
  mutate(new_region = to_north_america(new_region))

# ---- Rebuild the two cowplot country-driver grids -----------
# make_country_panel() / make_omegaA_panel() read region_colors from
# the global environment, so rebuilding picks up the colorblind-safe
# palette above. Only the colors change; panel names are preserved.
fig4_country_drivers <- cowplot::plot_grid(
  make_country_panel("Dengue"),
  make_country_panel("Influenza"),
  make_country_panel("Pertussis"),
  make_country_panel("Malaria"),
  make_country_panel("Measles"),
  cowplot::get_legend(
    ggplot(top_countries_region,
           aes(x = Country, y = total_imports, fill = new_region)) +
      geom_col() +
      scale_fill_manual(values = region_colors, name = "World region") +
      theme_minimal(base_size = 17) +
      theme(legend.position = "right",
            legend.title    = element_text(size = 15, face = "bold"),
            legend.text     = element_text(size = 14),
            legend.key.size = unit(0.6, "cm"))
  ),
  ncol = 2, labels = c("A", "B", "C", "D", "E", ""), label_size = 14
)

figC <- cowplot::plot_grid(
  make_omegaA_panel("Dengue"),
  make_omegaA_panel("Influenza"),
  make_omegaA_panel("Pertussis"),
  make_omegaA_panel("Malaria"),
  make_omegaA_panel("Measles"),
  cowplot::get_legend(
    ggplot(top_countries_omegaA,
           aes(x = Country, y = omega_A_total, fill = new_region)) +
      geom_col() +
      scale_fill_manual(values = region_colors, name = "World region") +
      theme_minimal(base_size = 17) +
      theme(legend.position = "right",
            legend.title    = element_text(size = 15, face = "bold"),
            legend.text     = element_text(size = 14),
            legend.key.size = unit(0.6, "cm"))
  ),
  ncol = 2, labels = c("A", "B", "C", "D", "E", ""), label_size = 14
)

# ---- Remove figure-level titles/subtitles (single ggplots) --
fig1_risk_heatmap     <- strip_titles(fig1_risk_heatmap)     + cividis_prob() + legend_crop_fix
fig2_lambda_ci        <- strip_titles(fig2_lambda_ci)
fig3_model_comparison <- strip_titles(fig3_model_comparison)
fig1_heatmap_baseline <- strip_titles(fig1_heatmap_baseline) + cividis_prob() + legend_crop_fix
fig1_heatmap_wc       <- strip_titles(fig1_heatmap_wc)       + cividis_prob() + legend_crop_fix
ci_asymmetry_plot     <- strip_titles(ci_asymmetry_plot) +
  scale_color_manual(values = disease_colors, name = "")
venue_map             <- strip_titles(venue_map) +
  scale_color_manual(values = host_nation_colors_cb, name = "Host nation")
figA                  <- strip_titles(figA) +
  cividis_prob(name   = "P(≥1 importation\ninto diaspora\nnetwork)",
               labels = c("0", "0.25", "0.50", "0.75", "1"))
# figS5 now uses figB_ext (extended hub set, venue + non-venue, all
# five diseases), built in Section 7f of the diaspora extension. It
# has no title/subtitle, so no strip_titles needed.


message("\nSaving IJID figure set...")

# ---- Main figures -------------------------------------------

ggsave(fig1_risk_heatmap,
       file   = "Submission_IJID/fig1_risk_heatmap_IJID.png",
       height = 4.5, width = 12.5, dpi = 300)

ggsave(fig2_lambda_ci,
       file   = "Submission_IJID/fig2_lambda_ci_IJID.png",
       height = 9, width = 14, dpi = 300)

ggsave(fig3_model_comparison,
       file   = "Submission_IJID/fig3_model_comparison_IJID.png",
       height = 8.5, width = 14, dpi = 300)

ggsave(fig4_country_drivers,
       file   = "Submission_IJID/fig4_country_drivers_IJID.png",
       height = 13, width = 15, dpi = 300)

ggsave(figA,
       file   = "Submission_IJID/fig5_diaspora_probA_IJID.png",
       height = 4.5, width = 13, dpi = 300)

# ---- Supplementary figures ----------------------------------

ggsave(fig1_heatmap_baseline,
       file   = "Submission_IJID/figS1_heatmap_baseline_IJID.png",
       height = 4.5, width = 12.5, dpi = 300)

ggsave(fig1_heatmap_wc,
       file   = "Submission_IJID/figS2_heatmap_wc_IJID.png",
       height = 4.5, width = 12.5, dpi = 300)

ggsave(ci_asymmetry_plot,
       file   = "Submission_IJID/figS3_ci_asymmetry_IJID.png",
       height = 6, width = 9, dpi = 300)

ggsave(venue_map,
       file   = "Submission_IJID/figS4_venue_map_IJID.png",
       height = 8, width = 12, dpi = 300)

ggsave(figB_ext,
       file   = "Submission_IJID/figS5_diaspora_hubseeding_IJID.png",
       height = 8, width = 13, dpi = 300)

ggsave(figC,
       file   = "Submission_IJID/figS6_country_drivers_omegaA_IJID.png",
       height = 13, width = 15, dpi = 300)

ggsave(fig_comparison,
       file   = "Submission_IJID/figS7_diaspora_design_comparison_IJID.png",
       height = 22, width = 18, dpi = 300)

message("Done. All IJID figures saved to Figures/ with _IJID suffix.")
message("Main text: fig1 – fig5_IJID.png")
message("Supplementary: figS1 – figS7_IJID.png")


# ============================================================
# 4b. NEW FIGURES FOR REVISED MANUSCRIPT
# ============================================================
# Revised figure numbering:
#   Fig 4 (NEW)  — WC excess stacked bar (Option D)
#   Fig 5 (ex-4) — Source country drivers
#   Fig 6 (NEW)  — Two-panel diaspora, dengue + malaria (Option A)
#   Fig S8 (NEW) — Full 5-disease diaspora heatmap (ex-Fig 5)

library(tidyr)

# ---- New Figure 4: WC travel increment above baseline -------

wc_totals <- comparison_all %>%
  group_by(disease, model) %>%
  summarise(total = sum(lambda_median), .groups = "drop") %>%
  filter(model %in% c("Baseline", "Schedule-driven")) %>%
  pivot_wider(names_from = model, values_from = total) %>%
  rename(M1 = Baseline, M3 = `Schedule-driven`) %>%
  mutate(
    wc_excess = M3 - M1,
    pct_label = sprintf("+%.0f%%", wc_excess / M1 * 100),
    disease   = factor(disease, levels = names(disease_colors))
  )

wc_long <- wc_totals %>%
  select(disease, M1, wc_excess, pct_label) %>%
  pivot_longer(cols     = c(M1, wc_excess),
               names_to = "component", values_to = "lambda") %>%
  mutate(component = factor(component,
    levels = c("M1", "wc_excess"),
    labels = c("Baseline (M1)", "WC increment")))

fig4_wc_excess <- ggplot(wc_long,
                          aes(x = component, y = lambda, fill = component)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(
    data        = wc_totals,
    aes(x = 1.5, y = M3 * 1.10, label = pct_label),
    inherit.aes = FALSE, size = 3.8, color = "gray30", fontface = "bold"
  ) +
  facet_wrap(~ disease, scales = "free_y", nrow = 1) +
  scale_fill_manual(values = c(
    "Baseline (M1)" = "steelblue3",
    "WC increment"  = "tomato3"
  )) +
  scale_x_discrete(labels = c("Baseline\n(M1)", "WC\nincrement")) +
  labs(
    x        = NULL,
    y        = expression(Expected~importations~(Lambda))
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    strip.text         = element_text(face = "bold", size = 11),
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(size = 9.5, color = "gray45"),
    axis.text.x        = element_text(size = 9)
  )

ggsave(fig4_wc_excess,
       file   = "Submission_IJID/fig4_wc_excess_IJID.png",
       height = 5, width = 12, dpi = 300)

# ---- Figure 5 (renamed from Fig 4): source country drivers --
ggsave(fig4_country_drivers,
       file   = "Submission_IJID/fig5_country_drivers_IJID.png",
       height = 13, width = 15, dpi = 300)

# ---- New Figure 6: Two-panel diaspora (dengue + malaria) ----

fig6_panelA_data <- omega_A_summary %>%
  filter(disease %in% c("Dengue", "Malaria")) %>%
  mutate(
    city     = factor(city, levels = city_order_main),
    disease  = factor(disease, levels = c("Dengue", "Malaria")),
    label    = sprintf("%.2f", prob_A),
    text_col = if_else(prob_A < 0.5, "white", "gray15")
  )

fig6_panelA <- ggplot(fig6_panelA_data,
                       aes(x = city, y = disease, fill = prob_A)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = label, color = text_col), size = 3.5) +
  scale_color_identity() +
  scale_fill_viridis_c(
    option = "cividis",
    name   = expression(P^A*(phantom(x) >= 1)),
    limits = c(0, 1),
    breaks = c(0, 0.25, 0.5, 0.75, 1)
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x       = element_text(angle = 35, hjust = 1, size = 10),
    axis.text.y       = element_text(size = 11, face = "italic"),
    panel.grid        = element_blank(),
    legend.position   = "right",
    legend.key.height = unit(1.4, "cm"),
    plot.title        = element_text(size = 12, face = "bold")
  )

# Extended hub set (venue + non-venue), from Section 7 of the
# diaspora extension. Shade by hub type to show that non-venue hubs
# carry much of the secondary seeding risk. reorder_within (defined
# in the main pipeline) orders bars correctly within each facet.
hub_type_colors <- c("Venue" = "#0072B2", "Non-venue" = "#D55E00")  # CB-safe

fig6_panelB_data <- omega_B_ext_summary %>%
  filter(disease %in% c("Dengue", "Malaria")) %>%
  group_by(disease) %>%
  slice_max(omega_B, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    disease  = factor(disease, levels = c("Dengue", "Malaria")),
    hub_type = factor(if_else(hub_type == "venue", "Venue", "Non-venue"),
                      levels = c("Venue", "Non-venue"))
  )

fig6_panelB <- ggplot(fig6_panelB_data,
                       aes(x = reorder_within(hub_city, omega_B, disease),
                           y = omega_B, fill = hub_type)) +
  geom_col() +
  facet_wrap(~ disease, scales = "free", nrow = 1) +
  coord_flip() +
  scale_x_discrete(labels = function(x) gsub("___.+$", "", x)) +
  scale_fill_manual(values = hub_type_colors, name = NULL) +
  labs(x = NULL,
       y     = expression(Seeding~index~(Omega[B]))) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    strip.text         = element_text(face = "bold"),
    legend.position    = "bottom"
  )

fig6_diaspora <- cowplot::plot_grid(
  fig6_panelA, fig6_panelB,
  ncol = 1, rel_heights = c(1, 1.3),
  labels = c("A", "B"), label_size = 14
)

ggsave(fig6_diaspora,
       file   = "Submission_IJID/fig6_diaspora_combined_IJID.png",
       height = 10, width = 13, dpi = 300)

# ---- Fig S8: full 5-disease diaspora heatmap (was Fig 5) ----
ggsave(figA,
       file   = "Submission_IJID/figS8_diaspora_full_IJID.png",
       height = 4.5, width = 13, dpi = 300)

message("New figures saved:")
message("  Submission_IJID/fig4_wc_excess_IJID.png")
message("  Submission_IJID/fig5_country_drivers_IJID.png")
message("  Submission_IJID/fig6_diaspora_combined_IJID.png")
message("  Submission_IJID/figS8_diaspora_full_IJID.png")


# ============================================================
# 5. SENSITIVITY ANALYSIS: p_d (mild-symptom travel probability)
# ============================================================
# p_d enters the model multiplicatively, so the 11-city total Λ
# at fixed (ρ_mid, p_test) equals:
#
#   Λ_det(ρ_mid, p_test) = Λ_MC × (ρ_mid × p_test) / median(ρ·p)
#
# where Λ_MC = sum(lambda_median) from the MC objects, and
# median(ρ·p) is the median of the joint product of the two
# Uniform draws. This converts the MC median (≈ S·median(ρ·p))
# to the deterministic value at fixed ρ_mid and varying p_test.
# The fold-change p_max/p_min is analytically exact regardless
# of model or parameter values.

set.seed(2026)
N_sim <- 1e5   # draws to estimate median(ρ·p)

sens_params <- tibble(
  disease  = c("Dengue",  "Malaria", "Measles",  "Pertussis", "Influenza"),
  rho_min  = c(0.06,      0.10,      0.40,       0.01,        0.01),
  rho_max  = c(0.26,      0.35,      0.80,       0.10,        0.10),
  p_min    = c(0.30,      0.10,      0.02,       0.50,        0.30),
  p_max    = c(0.70,      0.50,      0.10,       0.90,        0.70)
) %>%
  mutate(
    rho_mid    = (rho_min + rho_max) / 2,
    p_mid      = (p_min   + p_max)   / 2,
    # median of the ρ·p product under joint Uniform sampling
    med_rho_p  = mapply(function(a1, a2, b1, b2)
      median(runif(N_sim, a1, a2) * runif(N_sim, b1, b2)),
      rho_min, rho_max, p_min, p_max),
    # correction converts MC median → deterministic value at (ρ_mid, p_mid)
    correction = (rho_mid * p_mid) / med_rho_p
  )

# MC medians (M3, schedule-driven) for the three main diseases.
# Extend to pertussis and influenza if those MC objects are in memory.
get_mc_total <- function(obj) {
  if (exists(deparse(substitute(obj)))) {
    obj %>% summarise(med = sum(lambda_median)) %>% pull(med)
  } else {
    NA_real_
  }
}

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
    lambda_det_mid  = lambda_mc  * correction,          # at (ρ_mid, p_mid)
    lambda_det_pmin = lambda_det_mid * (p_min / p_mid), # at (ρ_mid, p_min)
    lambda_det_pmax = lambda_det_mid * (p_max / p_mid), # at (ρ_mid, p_max)
    fold            = p_max / p_min
  )

# Influenza correction: lambda_mc was generated under the original rho_d
# range [0.033, 0.20]. The revised range [0.01, 0.10] is supported by
# expansion-factor literature (McCarthy et al. 2020; Hayward et al. 2014).
# Since the model is proportional in rho_d, lambda_det scales exactly by
# the ratio of new to old rho_d midpoints.
flu_rho_mid_old <- (0.033 + 0.20) / 2   # 0.1165 — original
flu_rho_mid_new <- (0.01  + 0.10) / 2   # 0.055  — revised
flu_scale       <- flu_rho_mid_new / flu_rho_mid_old  # exact scaling factor

sens_results <- sens_results %>%
  mutate(across(
    c(lambda_det_mid, lambda_det_pmin, lambda_det_pmax),
    ~ if_else(disease == "Influenza", .x * flu_scale, .x)
  ))

cat("\n=== SENSITIVITY ANALYSIS: p_d (ρ_d fixed at midpoint) ===\n")
cat("11-city total expected importations — M3 schedule-driven model\n")
cat("Λ values: deterministic at fixed ρ_mid, varying p_d\n\n")
cat(sprintf("%-10s  %5s  %5s  %5s  %8s  %8s  %8s  %5s\n",
            "Disease", "p_min", "p_mid", "p_max",
            "Λ(p_min)", "Λ(p_mid)", "Λ(p_max)", "Fold"))
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
cat("\nFold = p_max/p_min (exact, model-independent).\n")
cat("Use Λ(p_mid) and Λ(p_min/p_max) to populate Table S1 in manuscript_IJID.tex.\n")


# ============================================================
# 6. INFLUENZA rho_d: THREE-WAY RANGE COMPARISON
# ============================================================
# Three candidate ranges for influenza rho_d (surveillance detection
# fraction), all computed as exact linear scaling from the original MC:
#
#   Original  [0.033, 0.20 ] — used in initial model runs
#   Chosen    [0.01,  0.10 ] — literature-supported middle ground
#                              McCarthy et al. 2020 (detection 1.2–2.6%)
#                              Hayward et al. 2014 (community/sentinel 22×)
#   Option A  [0.005, 0.033] — lower bound; strict expansion-factor lit.
#                              (expansion 30–200× → rho_d 0.005–0.033)

flu_chosen <- filter(sens_results, disease == "Influenza")
# Recover original lambda_det_mid (pre-scaling) to anchor the comparison
flu_lambda_det_original_mid  <- flu_chosen$lambda_det_mid  / flu_scale
flu_lambda_det_original_pmin <- flu_chosen$lambda_det_pmin / flu_scale
flu_lambda_det_original_pmax <- flu_chosen$lambda_det_pmax / flu_scale

rho_configs <- tibble(
  Range    = c("Original  [0.033, 0.20 ]",
               "Chosen    [0.01,  0.10 ]",
               "Option A  [0.005, 0.033]"),
  rho_min  = c(0.033, 0.01,  0.005),
  rho_max  = c(0.20,  0.10,  0.033),
  rho_mid  = c((0.033+0.20)/2, (0.01+0.10)/2, (0.005+0.033)/2)
) %>%
  mutate(
    rel_scale = rho_mid / ((0.033 + 0.20) / 2),
    L_pmin    = flu_lambda_det_original_pmin * rel_scale,
    L_pmid    = flu_lambda_det_original_mid  * rel_scale,
    L_pmax    = flu_lambda_det_original_pmax * rel_scale,
    P_geq1    = (1 - exp(-L_pmid)) * 100
  )

cat("\n=== INFLUENZA rho_d: THREE-WAY RANGE COMPARISON ===\n")
cat("p_d fixed at mid = 0.50; p_d fold = 2.3× throughout.\n\n")
cat(sprintf("%-26s  %6s  %8s  %8s  %8s  %7s\n",
            "Range", "rho_mid", "Λ(p_min)", "Λ(p_mid)", "Λ(p_max)", "P(≥1)"))
cat(strrep("-", 72), "\n")
for (i in seq_len(nrow(rho_configs))) {
  r <- rho_configs[i, ]
  cat(sprintf("%-26s  %6.4f  %8.2f  %8.2f  %8.2f  %6.1f%%\n",
              r$Range, r$rho_mid, r$L_pmin, r$L_pmid, r$L_pmax, r$P_geq1))
}
cat("\nΛ = 11-city total expected importations (M3, deterministic at rho_mid × p_mid).\n")
cat("P(≥1) = probability of at least one importation across all 11 cities.\n")
cat("Chosen range selected; Table S1 and Table 1 updated accordingly.\n")
