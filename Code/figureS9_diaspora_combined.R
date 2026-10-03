# ============================================================
# Figure S9: diaspora risk for dengue and malaria
#   Panel A: P^A(>=1) heatmap (Mechanism A)
#   Panel B: top 10 hub cities by Omega_B, venue vs. non-venue
# ============================================================
# PREREQUISITE: run importationRisk_main.R and
# importationRisk_diaspora_extension.R first, in the same R session.
# Uses omega_A_summary, omega_B_ext_summary, city_order_main and
# reorder_within().
# ============================================================

fig5_panelA_data <- omega_A_summary %>%
  filter(disease %in% c("Dengue", "Malaria")) %>%
  mutate(
    city     = factor(city, levels = city_order_main),
    disease  = factor(disease, levels = c("Dengue", "Malaria")),
    label    = sprintf("%.2f", prob_A),
    text_col = if_else(prob_A < 0.5, "white", "gray15")
  )

fig5_panelA <- ggplot(fig5_panelA_data,
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

hub_type_colors <- c("Venue" = "#0072B2", "Non-venue" = "#D55E00")  # CB-safe

fig5_panelB_data <- omega_B_ext_summary %>%
  filter(disease %in% c("Dengue", "Malaria")) %>%
  group_by(disease) %>%
  slice_max(omega_B, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    disease  = factor(disease, levels = c("Dengue", "Malaria")),
    hub_type = factor(if_else(hub_type == "venue", "Venue", "Non-venue"),
                      levels = c("Venue", "Non-venue"))
  )

fig5_panelB <- ggplot(fig5_panelB_data,
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

fig5_diaspora <- cowplot::plot_grid(
  fig5_panelA, fig5_panelB,
  ncol = 1, rel_heights = c(1, 1.3),
  labels = c("A", "B"), label_size = 14
)

ggsave(fig5_diaspora,
       file   = "Figures/FigureS9.png",
       height = 12, width = 13, dpi = 300)
# Vector PDF for journal submission (Elsevier line-art requirement)
ggsave(fig5_diaspora,
       file   = "Figures/FigureS9.pdf",
       height = 12, width = 13, device = grDevices::quartz, type = "pdf")  # macOS native vector PDF (cairo_pdf needs XQuartz)

message("Saved: Figures/FigureS9.png")
