library(igraph)
library(dplyr)
library(ggplot2)
library(pheatmap)
library(ggraph)

# 1. Cargamos las redes
ruta_GABAergic <- "/Users/alejandropintacastro/Downloads/INMEGEN/distancias/GABAergic"

grafo_AD <- read_graph("/Users/alejandropintacastro/Desktop/Redes_Pau/graphADnodes_membership.graphml",
                      format = "graphml")
grafo_ctrl <- read_graph("/Users/alejandropintacastro/Desktop/Redes_Pau/graphnoADnodes_membership.graphml",
                        format = "graphml")

# 2. Cargar genes de mis vias canónicas

GABAergic_pathways_AD <- unique(gaba_con_top$Ensembl)
GABAergic_pathways_AD <- trimws(GABAergic_pathways_AD) # Solo para verficar que no haya espacios dentro del grafo
GABAergic_pathways_AD <- GABAergic_pathways_AD[!is.na(GABAergic_pathways_AD)]

Genes_in_AD_GABAergic <- intersect(V(grafo_AD)$name, GABAergic_pathways_AD) # Interseccion de mis grafos
Genes_in_ctr_GABAergic <- intersect(V(grafo_ctrl)$name, GABAergic_pathways_AD)

# 3. Hacer subgrafos
grafo_AD_sub <- induced_subgraph(grafo_AD, vids = Genes_in_AD_GABAergic)
grafo_ctrl_sub <- induced_subgraph(grafo_ctrl, vids = Genes_in_ctr_GABAergic)

# 4. Ya hacemos el calculo de distacias
dist_matrix_AD_GABAergic <- distances(grafo_AD_sub)
dist_matrix_ctrl_GABAergic <- distances(grafo_ctrl_sub)

# 5. Distancias promedio
mean_dist_AD_GABAergic <- mean(dist_matrix_AD_GABAergic[is.finite(dist_matrix_AD_GABAergic)])
mean_dist_ctrl_GABAergic <- mean(dist_matrix_ctrl_GABAergic[is.finite(dist_matrix_ctrl_GABAergic)])

# 6. Degree
deg_AD_GABAergic <- degree(grafo_AD_sub)
deg_ctrl_GABAergic <- degree(grafo_ctrl_sub)

df_AD_GABAergic <- data.frame(
  gene = names(deg_AD_GABAergic),
  degree = deg_AD_GABAergic,
  group = "AD"
)

df_ctrl_GABAergic <- data.frame(
  gene = names(deg_ctrl_GABAergic),
  degree = deg_ctrl_GABAergic,
  group = "Control"
)

df_all_GABAergic <- bind_rows(df_AD_GABAergic, df_ctrl_GABAergic)

# Guardar csv de distancias 
write.csv(
  dist_matrix_AD_GABAergic,
  file = file.path(ruta_GABAergic, "distancias_AD_GABAergic.csv"),
  row.names = TRUE
)

write.csv(
  dist_matrix_ctrl_GABAergic,
  file = file.path(ruta_GABAergic, "distancias_ctrl_GABAergic.csv"),
  row.names = TRUE
)

# Graficar AD
dist_matrix_AD_clean_GABAergic <- dist_matrix_AD_GABAergic

# Reemplazar inf por numero
dist_matrix_AD_clean_GABAergic[is.infinite(dist_matrix_AD_clean_GABAergic)] <- max(
  dist_matrix_AD_GABAergic[is.finite(dist_matrix_AD_GABAergic)]
) + 1

# Ordenar por orden alfabetico AD GABAergic
dist_matrix_AD_clean_GABAergic <- dist_matrix_AD_clean_GABAergic[
  order(rownames(dist_matrix_AD_clean_GABAergic)),
  order(colnames(dist_matrix_AD_clean_GABAergic))
]
# Heatmap AD
pheatmap(dist_matrix_AD_clean_GABAergic,
         color = my_colors,
         show_rownames = TRUE,
         show_colnames = TRUE,
         clustering_method = "complete",
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         main = "GABAergic network distances (AD)")

# Graficar control 
dist_matrix_ctrl_clean_GABAergic <- dist_matrix_ctrl_GABAergic
dist_matrix_ctrl_clean_GABAergic[is.infinite(dist_matrix_ctrl_clean_GABAergic)] <- max(dist_matrix_ctrl_GABAergic[is.finite(dist_matrix_ctrl_GABAergic)]) + 1

# Ordenar por orden alfabetico ctrl
dist_matrix_ctrl_clean_GABAergic <- dist_matrix_ctrl_clean_GABAergic[
  order(rownames(dist_matrix_ctrl_clean_GABAergic)),
  order(colnames(dist_matrix_ctrl_clean_GABAergic))
]
# Heatmap control
pheatmap(dist_matrix_ctrl_clean_GABAergic,
         color = my_colors,
         show_rownames = TRUE,
         show_colnames = TRUE,
         clustering_method = "complete",
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         main = "GABAergic network distances (control)")

# Graficar la red AD y ctrl
par(mfrow = c(1,2))

plot(grafo_AD_sub,
     layout = layout_with_kk(grafo_AD_sub),
     vertex.size = 3,
     vertex.label = V(grafo_AD_sub)$name,
     vertex.label.cex = 0.5,
     vertex.label.color = "black",
     vertex.label.dist = 1,
     main = "AD")

plot(grafo_ctrl_sub,
     layout = layout_with_kk(grafo_ctrl_sub),
     vertex.size = 3,
     vertex.label = V(grafo_ctrl_sub)$name,
     vertex.label.cex = 0.5,
     vertex.label.color = "black",
     vertex.label.dist = 1,
     main = "Control")

# Guardar heatmap AD
png(file.path(ruta_GABAergic, "heatmap_AD_GABAergic.png"),
    width = 2000, height = 2000, res = 300)

pheatmap(dist_matrix_AD_clean_GABAergic,
         color = my_colors,
         show_rownames = TRUE,
         show_colnames = TRUE,
         clustering_method = "complete",
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         main = "GABAergic network distances (AD)")

dev.off()

# Guardar heatmap control
png(file.path(ruta_GABAergic, "heatmap_Control_GABAergic.png"),
    width = 2000, height = 2000, res = 300)

pheatmap(dist_matrix_ctrl_clean_GABAergic,
         color = my_colors,
         show_rownames = TRUE,
         show_colnames = TRUE,
         clustering_method = "complete",
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         main = "GABAergic network distances (control)")

dev.off()

# Guardar plots de redes
png(file.path(ruta_GABAergic, "network_comparison_GABAergic.png"),
    width = 2400, height = 1200, res = 200)

par(mfrow = c(1,2))

plot(grafo_AD_sub,
     layout = layout_with_kk(grafo_AD_sub),
     vertex.size = 3,
     vertex.label = V(grafo_AD_sub)$name,
     vertex.label.cex = 0.5,
     vertex.label.dist = 1,
     main = "AD")

plot(grafo_ctrl_sub,
     layout = layout_with_kk(grafo_ctrl_sub),
     vertex.size = 3,
     vertex.label = V(grafo_ctrl_sub)$name,
     vertex.label.cex = 0.5,
     vertex.label.dist = 1,
     main = "Control")

dev.off()

# CSV con los resultados
summary_results_GABAergic <- data.frame(
  mean_distance_AD = mean_dist_AD_GABAergic,
  mean_distance_Control = mean_dist_ctrl_GABAergic)

write.csv(
  summary_results_GABAergic,
  file = file.path(ruta_GABAergic, "summary_results_GABAergic.csv"),
  row.names = FALSE
)

