pacman::p_load(igraph, ggraph, ggplot2, dplyr)

set.seed(123)

g <- sample_sbm(
  n = 30,
  pref.matrix = matrix(c(
    0.8, 0.02, 0.02, 0.02,
    0.02, 0.8, 0.02, 0.02,
    0.02, 0.02, 0.8, 0.02,
    0.02, 0.02, 0.02, 0.8
  ), nrow = 4),
  block.sizes = c(8, 7, 8, 7)
)

V(g)$community <- c(rep("Module 1", 8), rep("Module 2", 7),
                    rep("Module 3", 8), rep("Module 4", 7))
V(g)$degree    <- degree(g)
V(g)$is_hub    <- V(g)$degree >= quantile(V(g)$degree, 0.85)
V(g)$node_size <- ifelse(V(g)$is_hub, 6, 4)

community_colors <- c(
  "Module 1" = "#E05C5C",
  "Module 2" = "#5B8DB8",
  "Module 3" = "#2CA02C",
  "Module 4" = "#FF7F0E"
)

layout <- create_layout(g, layout = "fr")

centros <- layout %>%
  mutate(community = V(g)$community) %>%
  group_by(community) %>%
  summarise(x = mean(x), y = mean(y))

p <- ggraph(layout) +
  
  geom_edge_link(alpha = 0.2, color = "gray60", width = 0.4) +
  
  geom_node_point(
    aes(size = node_size, color = community),
    alpha = 0.85
  ) +
  
  # Círculo exterior más proporcional
  geom_node_point(
    data = . %>% filter(V(g)$is_hub),
    aes(size = node_size + 1),
    shape = 1, stroke = 1.2,
    color = "black", alpha = 1
  ) +
  
  # Etiquetas sin borde de color
  # Reemplaza el annotate de centros por este
  annotate("text",
           x = c(centros$x[centros$community == "Module 1"],
                 centros$x[centros$community == "Module 2"] + 0.5,
                 centros$x[centros$community == "Module 3"] - 0.5,
                 centros$x[centros$community == "Module 4"]),
           y = c(centros$y[centros$community == "Module 1"] - 1.8,
                 centros$y[centros$community == "Module 2"] + 1.8,
                 centros$y[centros$community == "Module 3"] + 1.8,
                 centros$y[centros$community == "Module 4"] + 1.8),
           label = c("Module 1", "Module 2", "Module 3", "Module 4"),
           fontface = "bold",
           size = 3.8,
           color = c(community_colors["Module 1"],
                     community_colors["Module 2"],
                     community_colors["Module 3"],
                     community_colors["Module 4"])) +
  
  
  scale_color_manual(values = community_colors, name = "Community / Module") +
  scale_size_identity() +
  
  labs(
    title    = "Conceptual representation of a co-expression network",
    subtitle = "Nodes represent genes; colors represent communities (modules)\nLarger nodes with hollow circles indicate hub genes (top 15% by degree)",
    caption  = "Edges represent statistically significant co-expression relationships"
  ) +
  
  theme_void(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
    plot.caption  = element_text(size = 9, color = "gray50", hjust = 0.5),
    legend.position = "bottom",
    plot.margin = margin(20, 20, 20, 20)
  )

p

ggsave(
  "/Users/alejandropintacastro/Downloads/INMEGEN/conceptual_network_figure.png",
  plot   = p,
  width  = 10,
  height = 8,
  dpi    = 300,
  bg = "white"
)
