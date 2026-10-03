# ============================================================
# Figure 3: World Cup increment above the M1 baseline (11-city totals)
# Figure S1: P(>=1) heatmap under M1
# ============================================================
# PREREQUISITE: run importationRisk_main.R and model1_baseline_2026.R
# first, in the same R session.
# ============================================================

total_lambda <- function(mc_obj) {
  mc_obj %>%
    summarise(
      median = sum(lambda_median),
      lo     = sum(lambda_lo),
      hi     = sum(lambda_hi)
    )
}

excess_totals_m1new <- bind_rows(
  total_lambda(dengue_mc_m1new)    %>% mutate(disease = "Dengue",    model = "Baseline"),
  total_lambda(dengue_mc_sched)    %>% mutate(disease = "Dengue",    model = "Schedule-driven"),
  total_lambda(malaria_mc_m1new)   %>% mutate(disease = "Malaria",   model = "Baseline"),
  total_lambda(malaria_mc_sched)   %>% mutate(disease = "Malaria",   model = "Schedule-driven"),
  total_lambda(pertussis_mc_m1new) %>% mutate(disease = "Pertussis", model = "Baseline"),
  total_lambda(pertussis_mc_sched) %>% mutate(disease = "Pertussis", model = "Schedule-driven"),
  total_lambda(influenza_mc_m1new) %>% mutate(disease = "Influenza", model = "Baseline"),
  total_lambda(influenza_mc_sched) %>% mutate(disease = "Influenza", model = "Schedule-driven"),
  total_lambda(measles_mc_m1new)   %>% mutate(disease = "Measles",   model = "Baseline"),
  total_lambda(measles_mc_sched)   %>% mutate(disease = "Measles",   model = "Schedule-driven")
)

excess_wide_m1new <- excess_totals_m1new %>%
  select(disease, model, median) %>%
  pivot_wider(names_from = model, values_from = median) %>%
  mutate(
    increment  = `Schedule-driven` - Baseline,
    pct_label  = sprintf("+%.1f%%", 100 * increment / Baseline)
  ) %>%
  arrange(desc(Baseline)) %>%
  mutate(disease = factor(disease, levels = disease))

excess_long_m1new <- excess_wide_m1new %>%
  select(disease, Baseline, increment) %>%
  pivot_longer(c(Baseline, increment), names_to = "segment", values_to = "value") %>%
  mutate(segment = factor(segment, levels = c("Baseline", "increment"),
                          labels = c("2026 baseline without WC (M1)", "WC increment (M3 − M1)")))

excess_colors <- c(
  "2026 baseline without WC (M1)" = "#4393c3",
  "WC increment (M3 − M1)"        = "#c0392b"
)

# ---- main panel ----
fig3_excess_main_m1new <- ggplot(excess_long_m1new, aes(x = disease, y = value, fill = segment)) +
  geom_col(width = 0.65, position = position_stack(reverse = TRUE)) +
  geom_text(data = excess_wide_m1new,
            aes(x = disease, y = `Schedule-driven`, label = pct_label),
            inherit.aes = FALSE, vjust = -0.6, size = 4.3, color = "gray20") +
  scale_fill_manual(values = excess_colors, name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
  labs(x = "Disease", y = "Expected importations (Λ)") +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position   = "none",
    axis.title.x      = element_text(size = 12)
  )

# ---- inset panel: zoomed measles bar (own y-scale) ----
measles_row_m1new  <- excess_wide_m1new %>% filter(disease == "Measles")
measles_long_m1new <- excess_long_m1new %>% filter(disease == "Measles")

fig3_excess_inset_m1new <- ggplot(measles_long_m1new, aes(x = disease, y = value, fill = segment)) +
  geom_col(width = 0.65, position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = excess_colors, guide = "none") +
  scale_y_continuous(limits = c(0, measles_row_m1new$`Schedule-driven` * 1.15),
                      expand = expansion(mult = c(0, 0.05))) +
  labs(x = NULL, y = NULL) +
  theme_bw(base_size = 8.5) +
  theme(
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid   = element_blank(),
    plot.background = element_rect(color = "black", linewidth = 0.6)
  )

inset_xmin <- nlevels(excess_wide_m1new$disease) - 1.4
inset_xmax <- nlevels(excess_wide_m1new$disease) + 0.6
inset_ymin <- max(excess_wide_m1new$`Schedule-driven`) * 0.66
inset_ymax <- max(excess_wide_m1new$`Schedule-driven`) * 0.90
measles_x  <- which(levels(excess_wide_m1new$disease) == "Measles")

fig3_excess_importation_m1new <- fig3_excess_main_m1new +
  annotation_custom(
    grob = ggplotGrob(fig3_excess_inset_m1new),
    xmin = inset_xmin, xmax = inset_xmax,
    ymin = inset_ymin, ymax = inset_ymax
  ) +
  annotate("segment", x = measles_x - 0.35, xend = inset_xmin,
           y = measles_row_m1new$`Schedule-driven`, yend = inset_ymin,
           linetype = "dashed", color = "gray30") +
  annotate("segment", x = measles_x + 0.35, xend = inset_xmax,
           y = measles_row_m1new$`Schedule-driven`, yend = inset_ymin,
           linetype = "dashed", color = "gray30")

ggsave(fig3_excess_importation_m1new,
       file   = "Figures/Figure3.png",
       height = 6.5, width = 10, dpi = 300)
# Vector PDF for journal submission (Elsevier line-art requirement)
ggsave(fig3_excess_importation_m1new,
       file   = "Figures/Figure3.pdf",
       height = 6.5, width = 10, device = grDevices::quartz, type = "pdf")  # macOS native vector PDF (cairo_pdf needs XQuartz)
print(fig3_excess_importation_m1new)

cat("\n=== FIGURE 3 totals and percentages ===\n")
print(excess_wide_m1new, n = Inf)

# ============================================================
# Figure S1: M1 heatmap. make_heatmap() and city_order_main come from
# importationRisk_main.R.
# ============================================================

mc_all_m1new <- bind_rows(
  dengue_mc_m1new    %>% mutate(disease = "Dengue"),
  malaria_mc_m1new   %>% mutate(disease = "Malaria"),
  measles_mc_m1new   %>% mutate(disease = "Measles"),
  pertussis_mc_m1new %>% mutate(disease = "Pertussis"),
  influenza_mc_m1new %>% mutate(disease = "Influenza")
) %>%
  mutate(
    disease    = factor(disease,
                        levels = c("Dengue","Influenza","Pertussis","Malaria","Measles")),
    city_clean = str_to_title(destination_city)
  )

fig1_heatmap_baseline_m1new <- make_heatmap(mc_all_m1new)

ggsave(fig1_heatmap_baseline_m1new,
       file   = "Figures/FigureS1.png",
       height = 4.5, width = 12.5, dpi = 300)
# Vector PDF for journal submission (Elsevier line-art requirement)
ggsave(fig1_heatmap_baseline_m1new,
       file   = "Figures/FigureS1.pdf",
       height = 4.5, width = 12.5, device = grDevices::quartz, type = "pdf")  # macOS native vector PDF (cairo_pdf needs XQuartz)
print(fig1_heatmap_baseline_m1new)
