pacman::p_load(ggplot2, ggrepel, dplyr, pals, patchwork)

# Genes que cambian de cuadrante en Synaptogenesis
genes_cambian_Synap <- c("SYN2", "WASF1", "EPHA4", "NLGN4Y")

# Umbrales correctos tomados de synaptogenesis_wide
umbral_grado_ctrl_Synap <- median(synaptogenesis_wide$Grado_Control,   na.rm = TRUE)
umbral_expr_ctrl_Synap  <- median(synaptogenesis_wide$Expr_Control,    na.rm = TRUE)
umbral_grado_AD_Synap   <- median(synaptogenesis_wide$Grado_Alzheimer, na.rm = TRUE)
umbral_expr_AD_Synap    <- median(synaptogenesis_wide$Expr_Alzheimer,  na.rm = TRUE)

# Paleta
mis_colores       <- alphabet(26)
niveles_comunidad <- levels(synaptogensis_plot_expr$Comunidad)

# Actualizar cambio_cuadrante en synaptogensis_plot_expr
synaptogensis_plot_expr <- synaptogensis_plot_expr %>%
  mutate(cambio_cuadrante = Symbol %in% genes_cambian_Synap)

# Data por grupo
df_ctrl <- synaptogensis_plot_expr %>% filter(grupo == "Control")
df_AD   <- synaptogensis_plot_expr %>% filter(grupo == "Alzheimer")

# Dataframe con etiquetas para AD
df_AD_labels <- df_AD %>% filter(Symbol %in% genes_cambian_Synap)

# Límites para etiquetas de cuadrantes
xmin <- min(synaptogensis_plot_expr$Grado, na.rm = TRUE)
xmax <- max(synaptogensis_plot_expr$Grado, na.rm = TRUE)
ymin <- min(synaptogensis_plot_expr$Expr,  na.rm = TRUE)
ymax <- max(synaptogensis_plot_expr$Expr,  na.rm = TRUE)

# Plot Control
p_ctrl <- ggplot(df_ctrl, aes(x = Grado, y = Expr, color = Comunidad)) +
  geom_vline(xintercept = umbral_grado_ctrl_Synap, linetype = "dashed",
             color = "darkgray", linewidth = 0.8) +
  geom_hline(yintercept = umbral_expr_ctrl_Synap, linetype = "dashed",
             color = "darkgray", linewidth = 0.8) +
  geom_point(size = 3, alpha = 0.6) +
  geom_point(
    data = df_ctrl %>% filter(cambio_cuadrante),
    color = "black", size = 4, shape = 1, stroke = 1.2
  ) +
  geom_text_repel(
    data = df_ctrl %>% filter(cambio_cuadrante),
    aes(label = Symbol),
    size = 3.2, fontface = "bold",
    max.overlaps = 100,
    segment.color = "black",
    show.legend = FALSE
  ) +
  annotate("text", x = xmin, y = ymax, label = "II",
           size = 5, color = "gray60", fontface = "bold", hjust = 0, vjust = 1) +
  annotate("text", x = xmax, y = ymax, label = "I",
           size = 5, color = "gray60", fontface = "bold", hjust = 1, vjust = 1) +
  annotate("text", x = xmin, y = ymin, label = "IV",
           size = 5, color = "gray60", fontface = "bold", hjust = 0, vjust = 0) +
  annotate("text", x = xmax, y = ymin, label = "III",
           size = 5, color = "gray60", fontface = "bold", hjust = 1, vjust = 0) +
  scale_x_log10() +
  scale_y_continuous(labels = function(x) ifelse(x >= 10000, scales::comma(x), as.character(x))) +
  scale_color_manual(
    values = setNames(mis_colores[1:n_synap], niveles_comunidad),
    name = "Community"
  ) +
  labs(title = "Control", x = "Degree", y = "Average Expression") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background  = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor = element_line(color = "gray95", linewidth = 0.25),
    plot.title       = element_text(face = "bold", hjust = 0.5),
    legend.position  = "none"
  )

# Plot AD
p_AD <- ggplot(df_AD, aes(x = Grado, y = Expr, color = Comunidad)) +
  geom_vline(xintercept = umbral_grado_AD_Synap, linetype = "dashed",
             color = "darkgray", linewidth = 0.8) +
  geom_hline(yintercept = umbral_expr_AD_Synap, linetype = "dashed",
             color = "darkgray", linewidth = 0.8) +
  geom_point(size = 3, alpha = 0.6) +
  geom_point(
    data = df_AD %>% filter(cambio_cuadrante),
    color = "black", size = 4, shape = 1, stroke = 1.2
  ) +
  geom_text_repel(
    data = df_AD_labels,
    aes(label = Symbol),
    size = 3.2, fontface = "bold",
    max.overlaps = 100,
    segment.color = "black",
    show.legend = FALSE
  ) +
  annotate("text", x = xmin, y = ymax, label = "II",
           size = 5, color = "gray60", fontface = "bold", hjust = 0, vjust = 1) +
  annotate("text", x = xmax, y = ymax, label = "I",
           size = 5, color = "gray60", fontface = "bold", hjust = 1, vjust = 1) +
  annotate("text", x = xmin, y = ymin, label = "IV",
           size = 5, color = "gray60", fontface = "bold", hjust = 0, vjust = 0) +
  annotate("text", x = xmax, y = ymin, label = "III",
           size = 5, color = "gray60", fontface = "bold", hjust = 1, vjust = 0) +
  scale_x_log10() +
  scale_y_continuous(labels = function(x) ifelse(x >= 10000, scales::comma(x), as.character(x))) +
  scale_color_manual(
    values = setNames(mis_colores[1:n_synap], niveles_comunidad),
    name = "Community"
  ) +
  labs(title = "Alzheimer", x = "Degree", y = NULL) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background  = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor = element_line(color = "gray95", linewidth = 0.25),
    plot.title       = element_text(face = "bold", hjust = 0.5),
    legend.position  = "right"
  )

# Combinar con patchwork
p_final_Synap <- p_ctrl + p_AD +
  plot_annotation(
    title   = "Average expression vs Degree - Synaptogenesis pathway",
    caption = "Circled genes indicate quadrant redistribution between Control and Alzheimer networks",
    theme = theme(
      plot.title   = element_text(face = "bold", hjust = 0.5, size = 13),
      plot.caption = element_text(color = "gray50", size = 8, hjust = 0.5)
    )
  )

p_final_Synap

ggsave(
  file.path(ruta_Synap, "Synaptogenesis_cambios_cuadricula_v2.png"),
  plot   = p_final_Synap,
  width  = 12,
  height = 6,
  dpi    = 300,
  bg     = "white"
)
