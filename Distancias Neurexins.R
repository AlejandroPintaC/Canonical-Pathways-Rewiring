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
         color = Heatmap_colors,
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
         color = Heatmap_colors,
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
         color = Heatmap_colors,
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
         color = Heatmap_colors,
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

# Maslov-Sneppen degree-preserving rewiring
rewire_graph_NN <- function(g, niter = NULL) {
  if (is.null(niter)) niter <- ecount(g) * 10
  rewire(g, with = keeping_degseq(niter = niter))
}

# Calcular distancia promedio finita de un subgrafo inducido 
mean_dist_subgraph_NN <- function(full_graph, gene_set) {
  genes_in_graph <- intersect(V(full_graph)$name, gene_set)
  if (length(genes_in_graph) < 2) return(NA)
  sub_g <- induced_subgraph(full_graph, vids = genes_in_graph)
  dm <- distances(sub_g)
  mean(dm[is.finite(dm)])
}

# Parámetros
n_perm   <- 1000
alpha    <- 0.05
gene_set <- Neurexins_pathways_AD
set.seed(42)

# Valores observados
obs_AD_NN   <- mean_dist_AD_Neurexins
obs_ctrl_NN <- mean_dist_ctrl_Neurexins

# Distribución nula por rewiring (Maslov-Sneppen)
null_AD_NN      <- numeric(n_perm)
null_control_NN <- numeric(n_perm)


for (i in seq_len(n_perm)) {
  if (i %% 100 == 0) cat(sprintf("  Permutación %d / %d\n", i, n_perm))
  g_rw           <- rewire_graph_NN(grafo_AD)
  null_AD_NN[i]  <- mean_dist_subgraph_NN(g_rw, gene_set)
}

for (i in seq_len(n_perm)) {
  if (i %% 100 == 0) cat(sprintf("  Permutación %d / %d\n", i, n_perm))
  g_rw                <- rewire_graph_NN(grafo_ctrl)
  null_control_NN[i]  <- mean_dist_subgraph_NN(g_rw, gene_set)
}

#P-values e intervalos de confianza 
pval_AD_NN   <- mean(null_AD_NN      <= obs_AD_NN,   na.rm = TRUE)
pval_ctrl_NN <- mean(null_control_NN <= obs_ctrl_NN, na.rm = TRUE)

# P-value dos colas
pval_AD_NN_2t   <- 2 * min(pval_AD_NN,   1 - pval_AD_NN)
pval_ctrl_NN_2t <- 2 * min(pval_ctrl_NN, 1 - pval_ctrl_NN)

# IC 95% empíricos
ci_AD_NN   <- quantile(null_AD_NN,      probs = c(0.025, 0.975), na.rm = TRUE)
ci_ctrl_NN <- quantile(null_control_NN, probs = c(0.025, 0.975), na.rm = TRUE)

# Z-score
z_AD_NN   <- (obs_AD_NN   - mean(null_AD_NN,      na.rm = TRUE)) / sd(null_AD_NN,      na.rm = TRUE)
z_ctrl_NN <- (obs_ctrl_NN - mean(null_control_NN, na.rm = TRUE)) / sd(null_control_NN, na.rm = TRUE)

#Realizar tabla 

summary_permutation_NN <- data.frame(
  Group              = c("AD", "Control"),
  Observed_mean_dist = c(obs_AD_NN,        obs_ctrl_NN),
  Null_mean          = c(mean(null_AD_NN,      na.rm = TRUE), mean(null_control_NN, na.rm = TRUE)),
  Null_SD            = c(sd(null_AD_NN,        na.rm = TRUE), sd(null_control_NN,   na.rm = TRUE)),
  CI_lower_95        = c(ci_AD_NN[1],   ci_ctrl_NN[1]),
  CI_upper_95        = c(ci_AD_NN[2],   ci_ctrl_NN[2]),
  Z_score            = c(z_AD_NN,       z_ctrl_NN),
  P_value_2tailed    = c(pval_AD_NN_2t, pval_ctrl_NN_2t),
  N_permutations     = n_perm
)

print(summary_permutation_NN)

write.csv(
  summary_permutation_NN,
  file = file.path(ruta_NN, "permutation_test_Neurexins.csv"),
  row.names = FALSE
)

mean_dist_subgraph_NN <- function(full_graph, gene_set) {
  genes_in_graph <- intersect(V(full_graph)$name, gene_set)
  if (length(genes_in_graph) < 2) return(NA)
  sub_g <- induced_subgraph(full_graph, vids = genes_in_graph)
  if (ecount(sub_g) == 0) return(NA)
  dm <- distances(sub_g)
  
  # Solo promediamos pares que SÍ tienen camino (finitos y mayores a cero)
  valores_validos <- dm[is.finite(dm) & dm > 0]
  if (length(valores_validos) == 0) return(NA)
  
  mean(valores_validos)
}

# Recalcular valores observados correctamente
dm_AD   <- distances(grafo_AD_sub_Neurexins)
dm_ctrl <- distances(grafo_ctrl_sub_Neurexins)

obs_AD_NN   <- mean(dm_AD[is.finite(dm_AD)   & dm_AD   > 0])
obs_ctrl_NN <- mean(dm_ctrl[is.finite(dm_ctrl) & dm_ctrl > 0])

# Label permutation pero es por mientras
set.seed(42)

all_genes_AD   <- V(grafo_AD)$name
all_genes_ctrl <- V(grafo_ctrl)$name

n_AD   <- length(Genes_in_AD_Neurexins)   # 18
n_ctrl <- length(Genes_in_ctr_Neurexins)  # 16

null_AD_NN      <- numeric(n_perm)
null_control_NN <- numeric(n_perm)

for (i in seq_len(n_perm)) {
  if (i %% 100 == 0) cat(sprintf("Permutación %d / %d\n", i, n_perm))
  genes_rand     <- sample(all_genes_AD, n_AD)
  null_AD_NN[i]  <- mean_dist_subgraph_NN(grafo_AD, genes_rand)
}

for (i in seq_len(n_perm)) {
  if (i %% 100 == 0) cat(sprintf("Permutación %d / %d\n", i, n_perm))
  genes_rand          <- sample(all_genes_ctrl, n_ctrl)
  null_control_NN[i]  <- mean_dist_subgraph_NN(grafo_ctrl, genes_rand)
}
