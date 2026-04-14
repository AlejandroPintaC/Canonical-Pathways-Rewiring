library(igraph)
library(dplyr)
library(ggplot2)
library(pheatmap)
library(ggraph)

# 1. Cargamos las redes
ruta_Synap <- "/Users/alejandropintacastro/Downloads/INMEGEN/distancias/Synaptogenesis"

grafo_AD <- read_graph("/Users/alejandropintacastro/Desktop/Redes_Pau/graphADnodes_membership.graphml",
                       format = "graphml")
grafo_ctrl <- read_graph("/Users/alejandropintacastro/Desktop/Redes_Pau/graphnoADnodes_membership.graphml",
                         format = "graphml")

# 2. Cargar genes de mis vias canónicas

Synap_pathways_AD <- unique(synaptogenesis_con_top$Ensembl)
Synap_pathways_AD <- trimws(Synap_pathways_AD) # Solo para verficar que no haya espacios dentro del grafo
Synap_pathways_AD <- Synap_pathways_AD[!is.na(Synap_pathways_AD)]

Genes_in_AD_Synap <- intersect(V(grafo_AD)$name, Synap_pathways_AD) # Interseccion de mis grafos
Genes_in_ctr_Synap <- intersect(V(grafo_ctrl)$name, Synap_pathways_AD)

# 3. Hacer subgrafos
grafo_AD_sub_Synap <- induced_subgraph(grafo_AD, vids = Genes_in_AD_Synap)
grafo_ctrl_sub_Synap <- induced_subgraph(grafo_ctrl, vids = Genes_in_ctr_Synap)

# 4. Ya hacemos el calculo de distacias
dist_matrix_AD_Synap <- distances(grafo_AD_sub_Synap)
dist_matrix_ctrl_Synap <- distances(grafo_ctrl_sub_Synap)

# 5. Distancias promedio
mean_dist_AD_Synap <- mean(dist_matrix_AD_Synap[is.finite(dist_matrix_AD_Synap)])
mean_dist_ctrl_Synap <- mean(dist_matrix_ctrl_Synap[is.finite(dist_matrix_ctrl_Synap)])

# 6. Degree
deg_AD_Synap <- degree(grafo_AD_sub_Synap)
deg_ctrl_Synap <- degree(grafo_ctrl_sub_Synap)

df_AD_Synap <- data.frame(
  gene = names(deg_AD_Synap),
  degree = deg_AD_Synap,
  group = "AD"
)

df_ctrl_Synap <- data.frame(
  gene = names(deg_ctrl_Synap),
  degree = deg_ctrl_Synap,
  group = "Control"
)

df_all_Synap <- bind_rows(df_AD_Synap, df_ctrl_Synap)


# Guardar csv de distancias 
write.csv(
  dist_matrix_AD_Synap,
  file = file.path(ruta_Synap, "distancias_AD_Synap.csv"),
  row.names = TRUE
)

write.csv(
  dist_matrix_ctrl_Synap,
  file = file.path(ruta_Synap, "distancias_ctrl_Synap.csv"),
  row.names = TRUE
)

# Graficar AD
dist_matrix_AD_clean_Synap <- dist_matrix_AD_Synap

# Reemplazar inf por numero
dist_matrix_AD_clean_Synap[is.infinite(dist_matrix_AD_clean_Synap)] <- max(
  dist_matrix_AD_Synap[is.finite(dist_matrix_AD_Synap)]
) + 1

# Ordenar por orden alfabetico AD Synap
dist_matrix_AD_clean_Synap <- dist_matrix_AD_clean_Synap[
  order(rownames(dist_matrix_AD_clean_Synap)),
  order(colnames(dist_matrix_AD_clean_Synap))
# Heatmap AD
pheatmap(dist_matrix_AD_clean_Synap,
         color = my_colors,
         show_rownames = TRUE,
         show_colnames = TRUE,
         clustering_method = "complete",
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         main = "Synap network distances (AD)")

# Graficar control 
dist_matrix_ctrl_clean_Synap <- dist_matrix_ctrl_Synap
dist_matrix_ctrl_clean_Synap[is.infinite(dist_matrix_ctrl_clean_Synap)] <- max(dist_matrix_ctrl_Synap[is.finite(dist_matrix_ctrl_Synap)]) + 1

# Ordenar por orden alfabetico ctrl Synap
dist_matrix_ctrl_clean_Synap <- dist_matrix_ctrl_clean_Synap[
  order(rownames(dist_matrix_ctrl_clean_Synap)),
  order(colnames(dist_matrix_ctrl_clean_Synap))
]
# Heatmap control
pheatmap(dist_matrix_ctrl_clean_Synap,
         color = my_colors,
         show_rownames = TRUE,
         show_colnames = TRUE,
         clustering_method = "complete",
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         main = "Synap network distances (Control)")

# Graficar la red AD y ctrl
par(mfrow = c(1,2))

plot(grafo_AD_sub_Synap,
     layout = layout_with_kk(grafo_AD_sub_Synap),
     vertex.size = 3,
     vertex.label = V(grafo_AD_sub_Synap)$name,
     vertex.label.cex = 0.5,
     vertex.label.color = "black",
     vertex.label.dist = 1,
     main = "AD")

plot(grafo_ctrl_sub_Synap,
     layout = layout_with_kk(grafo_ctrl_sub_Synap),
     vertex.size = 3,
     vertex.label = V(grafo_ctrl_sub_Synap)$name,
     vertex.label.cex = 0.5,
     vertex.label.color = "black",
     vertex.label.dist = 1,
     main = "Control")

# Guardar heatmap AD
png(file.path(ruta_Synap, "heatmap_AD_Synap.png"),
    width = 2000, height = 2000, res = 250)

pheatmap(dist_matrix_AD_clean_Synap,
         color = my_colors,
         show_rownames = TRUE,
         show_colnames = TRUE,
         clustering_method = "complete",
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         main = "Synap network distances (AD)")

dev.off()

# Guardar heatmap control
png(file.path(ruta_Synap, "heatmap_Control_Synap.png"),
    width = 2000, height = 2000, res = 250)

pheatmap(dist_matrix_ctrl_clean_Synap,
         color = my_colors,
         show_rownames = TRUE,
         show_colnames = TRUE,
         clustering_method = "complete",
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         main = "Synap network distances (Control)")


dev.off()

# Guardar plots de redes
png(file.path(ruta_Synap, "network_comparison_Synap.png"),
    width = 2400, height = 1200, res = 200)

par(mfrow = c(1,2))

plot(grafo_AD_sub_Synap,
     layout = layout_with_kk(grafo_AD_sub_Synap),
     vertex.size = 3,
     vertex.label = V(grafo_AD_sub_Synap)$name,
     vertex.label.cex = 0.5,
     vertex.label.dist = 1,
     main = "AD")

plot(grafo_ctrl_sub_Synap,
     layout = layout_with_kk(grafo_ctrl_sub_Synap),
     vertex.size = 3,
     vertex.label = V(grafo_ctrl_sub_Synap)$name,
     vertex.label.cex = 0.5,
     vertex.label.dist = 1,
     main = "Control")

dev.off()

# CSV con los resultados 

summary_results_Synap <- data.frame(
  mean_distance_AD = mean_dist_AD_Synap,
  mean_distance_Control = mean_dist_ctrl_Synap)

write.csv(
  summary_results_Synap,
  file = file.path(ruta_Synap, "summary_results_Synap.csv"),
  row.names = FALSE
)

# definir colores
# Paleta de colores continua 
my_colors <- colorRampPalette(c("#f7fbff", "#6baed6", "#08306b"))(100)




