install.packages("pals")
pacman::p_load(
  igraph,
  dplyr,
  tidyr,
  ggplot2,
  ggrepel,
  pals,
  pheatmap,
  ggraph
)
pals::pal.bands(alphabet(26))



mis_colores <- alphabet(26)
# Esto es nada máspara verificar cuantos colores necesita cada vía
n_gaba    <- length(unique(GABAergic_plot_expr$Comunidad))
n_neurex  <- length(unique(Neurexins_plot_expr$Comunidad))
n_snare   <- length(unique(SNARE_plot_expr$Comunidad))
n_synap   <- length(unique(synaptogensis_plot_expr$Comunidad))

library(pals)

# GABAergic
p_gaba_expr_cambios <- ggplot(GABAergic_plot_expr, 
                              aes(x = Grado, y = Expr,
                                  color = Comunidad, 
                                  label = Symbol)) +
  geom_vline(xintercept = umbral_grado, linetype = "dashed", color = "darkgray", linewidth = 0.8) +
  geom_hline(yintercept = umbral_expr, linetype = "dashed", color = "darkgray", linewidth = 0.8) +
  geom_point(aes(color = Comunidad), size = 3, alpha = 0.6) +
  geom_point(
    data = ~ filter(.x, cambio_cuadrante),
    color = "black", size = 4, shape = 1, stroke = 1.5, alpha = 1,
    show.legend = FALSE
  ) +
  geom_text_repel(
    data = ~ filter(.x, cambio_cuadrante),
    aes(label = Symbol),
    size = 3.5, max.overlaps = 100,
    segment.color = "black", fontface = "bold",
    show.legend = FALSE
  ) +
  scale_x_log10(name = "Degree") +
  scale_y_continuous(name = "Average Expression") +
  scale_color_manual(
    values = setNames(mis_colores[1:n_gaba], levels(GABAergic_plot_expr$Comunidad)),
    name = "Community"
  ) +
  labs(title = "Average expression vs Degree - GABAergic pathway") +
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

ggsave("/Users/alejandropintacastro/Downloads/INMEGEN/IPA/Alejandr@s-EHL/plots mod/GABAergic_cambios_cuadricula.png",
       plot = p_gaba_expr_cambios, width = 12, height = 7, dpi = 300)



