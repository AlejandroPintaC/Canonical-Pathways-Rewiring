library(igraph)
library(dplyr)
library(ggplot2)
library(pheatmap)
library(ggraph)

# 1. Cargamos las redes
ruta_NN <- "/Users/alejandropintacastro/Downloads/INMEGEN/distancias/Neurexins"

grafo_AD <- read_graph("/Users/alejandropintacastro/Desktop/Redes_Pau/graphADnodes_membership.graphml",
                       format = "graphml")
grafo_ctrl <- read_graph("/Users/alejandropintacastro/Desktop/Redes_Pau/graphnoADnodes_membership.graphml",
                         format = "graphml")

# 2. Cargar genes de mis vias canónicas

Neurexins_pathways_AD <- unique(neurexins_con_top$Ensembl)
Neurexins_pathways_AD <- trimws(Neurexins_pathways_AD) # Solo para verficar que no haya espacios dentro del grafo
Neurexins_pathways_AD <- Neurexins_pathways_AD[!is.na(Neurexins_pathways_AD)]

Genes_in_AD_Neurexins <- intersect(V(grafo_AD)$name, Neurexins_pathways_AD) # Interseccion de mis grafos
Genes_in_ctr_Neurexins <- intersect(V(grafo_ctrl)$name, Neurexins_pathways_AD)

# 3. Hacer subgrafos
grafo_AD_sub_Neurexins <- induced_subgraph(grafo_AD, vids = Genes_in_AD_Neurexins)
grafo_ctrl_sub_Neurexins <- induced_subgraph(grafo_ctrl, vids = Genes_in_ctr_Neurexins)

# 4. Ya hacemos el calculo de distacias
dist_matrix_AD_Neurexins <- distances(grafo_AD_sub_Neurexins)
dist_matrix_ctrl_Neurexins <- distances(grafo_ctrl_sub_Neurexins)

# 5. Distancias promedio
mean_dist_AD_Neurexins <- mean(dist_matrix_AD_Neurexins[is.finite(dist_matrix_AD_Neurexins)])
mean_dist_ctrl_Neurexins <- mean(dist_matrix_ctrl_Neurexins[is.finite(dist_matrix_ctrl_Neurexins)])

# 6. Degree
deg_AD_Neurexins <- degree(grafo_AD_sub_Neurexins)
deg_ctrl_Neurexins <- degree(grafo_ctrl_sub_Neurexins)

df_AD_Neurexins <- data.frame(
  gene = names(deg_AD_Neurexins),
  degree = deg_AD_Neurexins,
  group = "AD"
)

df_ctrl_Neurexins <- data.frame(
  gene = names(deg_ctrl_Neurexins),
  degree = deg_ctrl_Neurexins,
  group = "Control"
)

df_all_Neurexins <- bind_rows(df_AD_Neurexins, df_ctrl_Neurexins)

# Guardar csv de distancias 
write.csv(
  dist_matrix_AD_Neurexins,
  file = file.path(ruta_NN, "distancias_AD_Neurexins.csv"),
  row.names = TRUE
)

write.csv(
  dist_matrix_ctrl_Neurexins,
  file = file.path(ruta_NN, "distancias_ctrl_Neurexins.csv"),
  row.names = TRUE
)

# Graficar AD
dist_matrix_AD_clean_Neurexins <- dist_matrix_AD_Neurexins

# Reemplazar inf por numero
dist_matrix_AD_clean_Neurexins[is.infinite(dist_matrix_AD_clean_Neurexins)] <- max(
  dist_matrix_AD_Neurexins[is.finite(dist_matrix_AD_Neurexins)]
) + 1

# Ordenar por orden alafabetico AD Neurexins
dist_matrix_AD_clean_Neurexins <- dist_matrix_AD_clean_Neurexins[
  order(rownames(dist_matrix_AD_clean_Neurexins)),
  order(colnames(dist_matrix_AD_clean_Neurexins))
]

# Heatmap AD
pheatmap(dist_matrix_AD_clean_Neurexins,
         color = my_colors,
         show_rownames = TRUE,
         show_colnames = TRUE,
         clustering_method = "complete",
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         main = "Neurexins and Neuroligins network distances (AD)")



# Graficar control 
dist_matrix_ctrl_clean_Neurexins <- dist_matrix_ctrl_Neurexins
dist_matrix_ctrl_clean_Neurexins[is.infinite(dist_matrix_ctrl_clean_Neurexins)] <- max(dist_matrix_ctrl_Neurexins[is.finite(dist_matrix_ctrl_Neurexins)]) + 1

# Ordenar por orden alafabetico AD Neurexins
dist_matrix_ctrl_clean_Neurexins <- dist_matrix_ctrl_clean_Neurexins[
  order(rownames(dist_matrix_ctrl_clean_Neurexins)),
  order(colnames(dist_matrix_ctrl_clean_Neurexins))
]

# Heatmap control
pheatmap(dist_matrix_ctrl_clean_Neurexins,
         color = my_colors,
         show_rownames = TRUE,
         show_colnames = TRUE,
         clustering_method = "complete",
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         main = "Neurexins and Neuroligins network distances (control)")

# Graficar la red AD y ctrl
par(mfrow = c(1,2))

plot(grafo_AD_sub_Neurexins,
     layout = layout_with_kk(grafo_AD_sub_Neurexins),
     vertex.size = 3,
     vertex.label = V(grafo_AD_sub_Neurexins)$name,
     vertex.label.cex = 0.5,
     vertex.label.color = "black",
     vertex.label.dist = 1,
     main = "AD")

plot(grafo_ctrl_sub_Neurexins,
     layout = layout_with_kk(grafo_ctrl_sub_Neurexins),
     vertex.size = 3,
     vertex.label = V(grafo_ctrl_sub_Neurexins)$name,
     vertex.label.cex = 0.5,
     vertex.label.color = "black",
     vertex.label.dist = 1,
     main = "Control")

# Guardar heatmap AD
png(file.path(ruta_NN, "heatmap_AD_Neurexins.png"),
    width = 2000, height = 2000, res = 300)

pheatmap(dist_matrix_AD_clean_Neurexins,
         color = my_colors,
         show_rownames = TRUE,
         show_colnames = TRUE,
         clustering_method = "complete",
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         main = "Neurexins and Neuroligins network distances (AD)")

dev.off()

# Guardar heatmap control
png(file.path(ruta_NN, "heatmap_Control_Neurexins.png"),
    width = 2000, height = 2000, res = 300)

pheatmap(dist_matrix_ctrl_clean_Neurexins,
         color = my_colors,
         show_rownames = TRUE,
         show_colnames = TRUE,
         clustering_method = "complete",
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         main = "Neurexins and Neuroligins network distances (control)")


dev.off()

# Guardar plots de redes
png(file.path(ruta_NN, "network_comparison_Neurexins.png"),
    width = 2400, height = 1200, res = 200)

par(mfrow = c(1,2))

plot(grafo_AD_sub_Neurexins,
     layout = layout_with_kk(grafo_AD_sub_Neurexins),
     vertex.size = 3,
     vertex.label = V(grafo_AD_sub_Neurexins)$name,
     vertex.label.cex = 0.5,
     vertex.label.dist = 1,
     main = "AD")

plot(grafo_ctrl_sub_Neurexins,
     layout = layout_with_kk(grafo_ctrl_sub_Neurexins),
     vertex.size = 3,
     vertex.label = V(grafo_ctrl_sub_Neurexins)$name,
     vertex.label.cex = 0.5,
     vertex.label.dist = 1,
     main = "Control")

dev.off()

# CSV con los resultados 
summary_results_Neurexins <- data.frame(
  mean_distance_AD = mean_dist_AD_Neurexins,
  mean_distance_Control = mean_dist_ctrl_Neurexins)

write.csv(
  summary_results_Neurexins,
  file = file.path(ruta_NN, "summary_results_Neurexins.csv"),
  row.names = FALSE
)


