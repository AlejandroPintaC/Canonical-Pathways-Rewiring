pacman::p_load(dplyr, ggplot2)

# Construir dataframe con los resultados de las 4 vías
df_summary <- data.frame(
  Pathway = rep(c("Neurexins &\nNeuroligins", 
                  "Synaptogenesis", 
                  "GABAergic", 
                  "SNARE"), each = 2),
  Group = rep(c("AD", "Control"), times = 4),
  Mean_dist = c(
    1.846154, 1.571429,   # Neurexins
    2.707581, 1.893617,   # Synaptogenesis
    2.434783, 2.293103,   # GABAergic
    2.764706, 2.183333    # SNARE
  ),
  P_value = c(
    0.029, 0.222,         # Neurexins
    0.004, 0.062,         # Synaptogenesis
    0.002, 0.000,         # GABAergic
    0.000, 0.002          # SNARE
  )
) %>%
  mutate(
    Group = factor(Group, levels = c("Control", "AD")),
    Pathway = factor(Pathway, levels = c("Neurexins &\nNeuroligins",
                                         "Synaptogenesis",
                                         "GABAergic",
                                         "SNARE")),
    Significant = P_value < 0.05
  )

# Figura
p_summary <- ggplot(df_summary, aes(x = Group, y = Mean_dist, group = Pathway)) +
  geom_line(aes(group = Pathway), color = "gray60", linewidth = 0.8) +
  geom_point(aes(color = Pathway, shape = Significant), size = 4) +
  geom_text(
    data = df_summary %>% filter(Group == "AD"),
    aes(label = Pathway, color = Pathway),
    hjust = -0.15,
    size = 3.2,
    show.legend = FALSE
  ) +
  scale_shape_manual(
    values = c("TRUE" = 16, "FALSE" = 1),
    labels = c("TRUE" = "p < 0.05", "FALSE" = "p ≥ 0.05"),
    name = "Significance"
  ) +
  scale_color_manual(
    values = c(
      "Neurexins &\nNeuroligins" = "#E05C5C",
      "Synaptogenesis"           = "#5B8DB8",
      "GABAergic"                = "#2CA02C",
      "SNARE"                    = "#FF7F0E"
    ),
    name = "Pathway"
  ) +
  scale_x_discrete(expand = expansion(mult = c(0.15, 0.35))) +
  labs(
    title    = "Mean shortest path distance across canonical pathways",
    subtitle = "AD vs Control — label permutation test (1,000 permutations)",
    x        = NULL,
    y        = "Mean shortest path distance",
    caption  = "Filled points: p < 0.05 | Open points: p ≥ 0.05"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

p_summary

ggsave(
  "/Users/alejandropintacastro/Downloads/INMEGEN/distancias/summary_shortest_path_distances.png",
  plot   = p_summary,
  width  = 8,
  height = 6,
  dpi    = 300
)
