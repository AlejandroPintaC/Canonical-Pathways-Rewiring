library(igraph)
library(dplyr)
library(ggplot2)
library(pheatmap)
library(ggraph)

# 1. Cargamos las redes
ruta_SNARE <- "/Users/alejandropintacastro/Downloads/INMEGEN/distancias/SNARE"

grafo_AD <- read_graph("/Users/alejandropintacastro/Desktop/Redes_Pau/graphADnodes_membership.graphml",
                       format = "graphml")
grafo_ctrl <- read_graph("/Users/alejandropintacastro/Desktop/Redes_Pau/graphnoADnodes_membership.graphml",
                         format = "graphml")

# 2. Cargar genes de mis vias canónicas

SNARE_pathways_AD <- unique(snare_con_top$Ensembl)
SNARE_pathways_AD <- trimws(SNARE_pathways_AD) # Solo para verficar que no haya espacios dentro del grafo
SNARE_pathways_AD <- SNARE_pathways_AD[!is.na(SNARE_pathways_AD)]

Genes_in_AD_SNARE <- intersect(V(grafo_AD)$name, SNARE_pathways_AD) # Interseccion de mis grafos
Genes_in_ctr_SNARE <- intersect(V(grafo_ctrl)$name, SNARE_pathways_AD)

# 3. Hacer subgrafos
grafo_AD_sub_SNARE <- induced_subgraph(grafo_AD, vids = Genes_in_AD_SNARE)
grafo_ctrl_sub_SNARE <- induced_subgraph(grafo_ctrl, vids = Genes_in_ctr_SNARE)

# 4. Ya hacemos el calculo de distacias
dist_matrix_AD_SNARE <- distances(grafo_AD_sub_SNARE)
dist_matrix_ctrl_SNARE <- distances(grafo_ctrl_sub_SNARE)

# 5. Distancias promedio
mean_dist_AD_SNARE <- mean(dist_matrix_AD_SNARE[is.finite(dist_matrix_AD_SNARE)])
mean_dist_ctrl_SNARE <- mean(dist_matrix_ctrl_SNARE[is.finite(dist_matrix_ctrl_SNARE)])

# 6. Degree
deg_AD_SNARE <- degree(grafo_AD_sub_SNARE)
deg_ctrl_SNARE <- degree(grafo_ctrl_sub_SNARE)

df_AD_SNARE <- data.frame(
  gene = names(deg_AD_SNARE),
  degree = deg_AD_SNARE,
  group = "AD"
)

df_ctrl_SNARE <- data.frame(
  gene = names(deg_ctrl_SNARE),
  degree = deg_ctrl_SNARE,
  group = "Control"
)

df_all_SNARE <- bind_rows(df_AD_SNARE, df_ctrl_SNARE)


# Guardar csv de distancias 
write.csv(
  dist_matrix_AD_SNARE,
  file = file.path(ruta_SNARE, "distancias_AD_SNARE.csv"),
  row.names = TRUE
)

write.csv(
  dist_matrix_ctrl_SNARE,
  file = file.path(ruta_SNARE, "distancias_ctrl_SNARE.csv"),
  row.names = TRUE
)

# Graficar AD
dist_matrix_AD_clean_SNARE <- dist_matrix_AD_SNARE

# Reemplazar inf por numero
dist_matrix_AD_clean_SNARE[is.infinite(dist_matrix_AD_clean_SNARE)] <- max(
  dist_matrix_AD_SNARE[is.finite(dist_matrix_AD_SNARE)]
) + 1

# Ordenar por orden alafabetico AD SNARE
dist_matrix_AD_clean_SNARE <- dist_matrix_AD_clean_SNARE[
  order(rownames(dist_matrix_AD_clean_SNARE)),
  order(colnames(dist_matrix_AD_clean_SNARE))

# Heatmap AD
pheatmap(dist_matrix_AD_clean_SNARE,
         color = my_colors,
         show_rownames = TRUE,
         show_colnames = TRUE,
         clustering_method = "complete",
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         main = "SNARE network distances (AD)")

# Graficar control 
dist_matrix_ctrl_clean_SNARE <- dist_matrix_ctrl_SNARE
dist_matrix_ctrl_clean_SNARE[is.infinite(dist_matrix_ctrl_clean_SNARE)] <- max(dist_matrix_ctrl_SNARE[is.finite(dist_matrix_ctrl_SNARE)]) + 1

# Ordenar por orden alafabetico control SNARE
dist_matrix_ctrl_clean_SNARE <- dist_matrix_ctrl_clean_SNARE[
  order(rownames(dist_matrix_ctrl_clean_SNARE)),
  order(colnames(dist_matrix_ctrl_clean_SNARE))

# Heatmap control
pheatmap(dist_matrix_ctrl_clean_SNARE,
         color = my_colors,
         show_rownames = TRUE,
         show_colnames = TRUE,
         clustering_method = "complete",
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         main = "SNARE network distances (control)")

# Graficar la red AD y ctrl
par(mfrow = c(1,2))

plot(grafo_AD_sub_SNARE,
     layout = layout_with_kk(grafo_AD_sub_SNARE),
     vertex.size = 3,
     vertex.label = V(grafo_AD_sub_SNARE)$name,
     vertex.label.cex = 0.5,
     vertex.label.color = "black",
     vertex.label.dist = 1,
     main = "AD")

plot(grafo_ctrl_sub_SNARE,
     layout = layout_with_kk(grafo_ctrl_sub_SNARE),
     vertex.size = 3,
     vertex.label = V(grafo_ctrl_sub_SNARE)$name,
     vertex.label.cex = 0.5,
     vertex.label.color = "black",
     vertex.label.dist = 1,
     main = "Control")

# Guardar heatmap AD
png(file.path(ruta_SNARE, "heatmap_AD_SNARE.png"),
    width = 2000, height = 2000, res = 300)

pheatmap(dist_matrix_AD_clean_SNARE,
         color = my_colors,
         show_rownames = TRUE,
         show_colnames = TRUE,
         clustering_method = "complete",
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         main = "SNARE network distances (AD)")

dev.off()

# Guardar heatmap control
png(file.path(ruta_SNARE, "heatmap_Control_SNARE.png"),
    width = 2000, height = 2000, res = 300)

pheatmap(dist_matrix_ctrl_clean_SNARE,
         color = my_colors,
         show_rownames = TRUE,
         show_colnames = TRUE,
         clustering_method = "complete",
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         main = "SNARE network distances (control)")

dev.off()

# Guardar plots de redes
png(file.path(ruta_SNARE, "network_comparison_SNARE.png"),
    width = 2400, height = 1200, res = 200)

par(mfrow = c(1,2))

plot(grafo_AD_sub_SNARE,
     layout = layout_with_kk(grafo_AD_sub_SNARE),
     vertex.size = 3,
     vertex.label = V(grafo_AD_sub_SNARE)$name,
     vertex.label.cex = 0.5,
     vertex.label.dist = 1,
     main = "AD")

plot(grafo_ctrl_sub_SNARE,
     layout = layout_with_kk(grafo_ctrl_sub_SNARE),
     vertex.size = 3,
     vertex.label = V(grafo_ctrl_sub_SNARE)$name,
     vertex.label.cex = 0.5,
     vertex.label.dist = 1,
     main = "Control")

dev.off()

# CSV con los resultados 

summary_results_SNARE <- data.frame(
  mean_distance_AD = mean_dist_AD_SNARE,
  mean_distance_Control = mean_dist_ctrl_SNARE)

write.csv(
  summary_results_SNARE,
  file = file.path(ruta_SNARE, "summary_results_SNARE.csv"),
  row.names = FALSE
)

