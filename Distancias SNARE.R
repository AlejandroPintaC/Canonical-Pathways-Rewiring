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

# Función auxiliar - SNARE
mean_dist_subgraph_SNARE <- function(full_graph, gene_set) {
  genes_in_graph <- intersect(V(full_graph)$name, gene_set)
  if (length(genes_in_graph) < 2) return(NA)
  sub_g <- induced_subgraph(full_graph, vids = genes_in_graph)
  if (ecount(sub_g) == 0) return(NA)
  dm <- distances(sub_g)
  valores_validos <- dm[is.finite(dm) & dm > 0]
  if (length(valores_validos) == 0) return(NA)
  mean(valores_validos)
}

# Valores observados
dm_AD_SNARE   <- distances(grafo_AD_sub_SNARE)
dm_ctrl_SNARE <- distances(grafo_ctrl_sub_SNARE)

obs_AD_SNARE   <- mean(dm_AD_SNARE[is.finite(dm_AD_SNARE)     & dm_AD_SNARE   > 0])
obs_ctrl_SNARE <- mean(dm_ctrl_SNARE[is.finite(dm_ctrl_SNARE) & dm_ctrl_SNARE > 0])

# Parámetros
n_perm          <- 1000
gene_set_SNARE  <- SNARE_pathways_AD
set.seed(42)

n_AD_SNARE   <- length(Genes_in_AD_SNARE)
n_ctrl_SNARE <- length(Genes_in_ctr_SNARE)

all_genes_AD   <- V(grafo_AD)$name
all_genes_ctrl <- V(grafo_ctrl)$name

# Label permutation - SNARE
null_AD_SNARE      <- numeric(n_perm)
null_control_SNARE <- numeric(n_perm)

for (i in seq_len(n_perm)) {
  if (i %% 100 == 0) cat(sprintf("Permutación %d / %d\n", i, n_perm))
  genes_rand          <- sample(all_genes_AD, n_AD_SNARE)
  null_AD_SNARE[i]    <- mean_dist_subgraph_SNARE(grafo_AD, genes_rand)
}

for (i in seq_len(n_perm)) {
  if (i %% 100 == 0) cat(sprintf("Permutación %d / %d\n", i, n_perm))
  genes_rand               <- sample(all_genes_ctrl, n_ctrl_SNARE)
  null_control_SNARE[i]    <- mean_dist_subgraph_SNARE(grafo_ctrl, genes_rand)
}

# Estadísticos
pval_AD_SNARE   <- mean(null_AD_SNARE      >= obs_AD_SNARE,   na.rm = TRUE)
pval_ctrl_SNARE <- mean(null_control_SNARE >= obs_ctrl_SNARE, na.rm = TRUE)

pval_AD_SNARE_2t   <- 2 * min(pval_AD_SNARE,   1 - pval_AD_SNARE)
pval_ctrl_SNARE_2t <- 2 * min(pval_ctrl_SNARE, 1 - pval_ctrl_SNARE)

ci_AD_SNARE   <- quantile(null_AD_SNARE,      probs = c(0.025, 0.975), na.rm = TRUE)
ci_ctrl_SNARE <- quantile(null_control_SNARE, probs = c(0.025, 0.975), na.rm = TRUE)

z_AD_SNARE   <- (obs_AD_SNARE   - mean(null_AD_SNARE,      na.rm = TRUE)) / sd(null_AD_SNARE,      na.rm = TRUE)
z_ctrl_SNARE <- (obs_ctrl_SNARE - mean(null_control_SNARE, na.rm = TRUE)) / sd(null_control_SNARE, na.rm = TRUE)

summary_permutation_SNARE <- data.frame(
  Group              = c("AD", "Control"),
  Observed_mean_dist = c(obs_AD_SNARE,        obs_ctrl_SNARE),
  Null_mean          = c(mean(null_AD_SNARE,      na.rm = TRUE), mean(null_control_SNARE, na.rm = TRUE)),
  Null_SD            = c(sd(null_AD_SNARE,        na.rm = TRUE), sd(null_control_SNARE,   na.rm = TRUE)),
  CI_lower_95        = c(ci_AD_SNARE[1],   ci_ctrl_SNARE[1]),
  CI_upper_95        = c(ci_AD_SNARE[2],   ci_ctrl_SNARE[2]),
  Z_score            = c(z_AD_SNARE,       z_ctrl_SNARE),
  P_value_2tailed    = c(pval_AD_SNARE_2t, pval_ctrl_SNARE_2t),
  N_permutations     = n_perm
)

print(summary_permutation_SNARE)

write.csv(
  summary_permutation_SNARE,
  file = file.path(ruta_SNARE, "permutation_test_SNARE.csv"),
  row.names = FALSE
)
