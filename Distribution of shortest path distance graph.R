pacman::p_load(ggplot2, dplyr, tidyr)

# Función para extraer distancias finitas de una matriz
extraer_distancias <- function(mat, grupo, pathway) {
  valores <- mat[upper.tri(mat)]
  valores <- valores[is.finite(valores) & valores > 0]
  data.frame(
    distancia = valores,
    Group     = grupo,
    Pathway   = pathway
  )
}

# Extraer distancias de todas las vías
df_dist <- bind_rows(
  extraer_distancias(dist_matrix_AD_Neurexins,   "AD",      "Neurexins &\nNeuroligins"),
  extraer_distancias(dist_matrix_ctrl_Neurexins, "Control", "Neurexins &\nNeuroligins"),
  extraer_distancias(dist_matrix_AD_Synap,       "AD",      "Synaptogenesis"),
  extraer_distancias(dist_matrix_ctrl_Synap,     "Control", "Synaptogenesis"),
  extraer_distancias(dist_matrix_AD_GABAergic,   "AD",      "GABAergic"),
  extraer_distancias(dist_matrix_ctrl_GABAergic, "Control", "GABAergic"),
  extraer_distancias(dist_matrix_AD_SNARE,       "AD",      "SNARE"),
  extraer_distancias(dist_matrix_ctrl_SNARE,     "Control", "SNARE")
) %>%
  mutate(
    Group   = factor(Group,   levels = c("Control", "AD")),
    Pathway = factor(Pathway, levels = c("Neurexins &\nNeuroligins",
                                         "Synaptogenesis",
                                         "GABAergic",
                                         "SNARE"))
  )

# Figura
p_dist <- ggplot(df_dist, aes(x = Group, y = distancia, fill = Group)) +
  geom_violin(alpha = 0.6, trim = TRUE) +
  geom_boxplot(width = 0.15, alpha = 0.8, outlier.shape = NA) +
  geom_point(
    data = data.frame(
      Group   = rep(c("Control", "AD"), 4),
      Pathway = rep(c("Neurexins &\nNeuroligins",
                      "Synaptogenesis",
                      "GABAergic",
                      "SNARE"), each = 2),
      distancia = c(obs_ctrl_NN,       obs_AD_NN,
                    obs_ctrl_Synap,    obs_AD_Synap,
                    obs_ctrl_GABAergic, obs_AD_GABAergic,
                    obs_ctrl_SNARE,    obs_AD_SNARE)
    ) %>% mutate(Group = factor(Group, levels = c("Control", "AD"))),
    aes(x = Group, y = distancia),
    shape = 23, size = 3, fill = "white", color = "black", stroke = 1.2
  ) +
  scale_fill_manual(values = c("Control" = "#5B8DB8", "AD" = "#E05C5C")) +
  facet_wrap(~ Pathway, scales = "free_y", ncol = 4) +
  labs(
    title    = "Distribution of shortest path distances across canonical pathways",
    subtitle = "AD vs Control — pairwise distances between pathway genes",
    x        = NULL,
    y        = "Shortest path distance",
    caption  = "Diamond indicates mean observed distance | Violin shows full distribution"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", hjust = 0.5),
    plot.subtitle    = element_text(hjust = 0.5, color = "gray40"),
    plot.caption     = element_text(color = "gray50", size = 8),
    legend.position  = "none",
    strip.background = element_rect(fill = "gray95"),
    strip.text       = element_text(face = "bold")
  )

p_dist

ggsave(
  file.path(ruta_final, "distance_distribution_comparison.png"),
  plot   = p_dist,
  width  = 14,
  height = 6,
  dpi    = 300,
  bg     = "white"
)
