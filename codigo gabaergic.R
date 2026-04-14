GABAergic_plot_expr <- gaba_con_top %>%
  filter(
    !is.na(`Expr Intensity/RPKM/FPKM/Counts...4`),  # Expresión
    !is.na(`Expr Intensity/RPKM/FPKM/Counts...6`),  # Grado
    !is.na(grupo)                                   # Asegurar que 'grupo' exista
  ) %>%
  mutate(
    Grado = as.numeric(`Expr Intensity/RPKM/FPKM/Counts...6`),
    Expr = `Expr Intensity/RPKM/FPKM/Counts...4`,
    Comunidad = as.factor(as.character(`Expr Intensity/RPKM/FPKM/Counts...7`))
  ) %>%
  # Asegurar valores positivos para log10
  filter(Grado > 0, Expr > 0) %>%
  # Asegurar orden en el factor 'grupo'
  mutate(grupo = factor(grupo, levels = c("Control", "Alzheimer")))


# Calcular umbrales globales (mismos en ambos paneles)
umbral_grado <- median(GABAergic_plot_expr$Grado, na.rm = TRUE)
umbral_expr  <- median(GABAergic_plot_expr$Expr, na.rm = TRUE)

# Gráfico final con cuadrícula y cuadrantes
p_gaba_expr <- ggplot(GABAergic_plot_expr, 
                      aes(x = Grado, y = Expr,
                          color = Comunidad, label = Symbol)) +
  # Líneas divisorias para los 4 cuadrantes
  geom_vline(xintercept = umbral_grado, linetype = "dashed", color = "darkgray", linewidth = 0.8) +
  geom_hline(yintercept = umbral_expr, linetype = "dashed", color = "darkgray", linewidth = 0.8) +
  
  geom_point(size = 3, alpha = 0.8) +
  geom_text_repel(
    size = 3, 
    max.overlaps = 300,
    segment.color = "gray70",
    show.legend = FALSE
  ) + 
  scale_x_log10(name = "Grado (log10)") +
  scale_y_continuous(name = "Average Expression") + 
  scale_color_viridis_d(option = "plasma", name = "Comunidad") + 
  labs(
    title = "Expresión promedio vs Grado en la red - Vía GABAérgica",
    x = "Grado (log10)",
    y = "Average Expression"
  ) + 
  facet_wrap(~ grupo, ncol = 2) +  
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor = element_line(color = "gray95", linewidth = 0.25),
    panel.spacing = unit(2.5, "cm"),  
    legend.position = "right",
    strip.text = element_text(size = 12, face = "bold")  
  )

p_gaba_expr
ggsave("/Users/alejandropintacastro/Downloads/INMEGEN/IPA/Alejandr@s-EHL/plots mod/GABAergic_expr_scatter_base_Mean.png",  #### para guardarlo
       plot = p_gaba_expr,
       width = 12,
       height = 7,
       dpi = 300)
