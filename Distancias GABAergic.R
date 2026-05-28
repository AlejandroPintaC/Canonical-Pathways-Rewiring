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
         color = Heatmap_colors,
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
         color = Heatmap_colors,
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
         color = Heatmap_colors,
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
         color = Heatmap_colors,
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

#Definir funciones en GABAergic

mean_dist_subgraph_GABAergic <- function(full_graph, gene_set) {
  genes_in_graph <- intersect(V(full_graph)$name, gene_set)
  if (length(genes_in_graph) < 2) return(NA)
  sub_g <- induced_subgraph(full_graph, vids = genes_in_graph)
  if (ecount(sub_g) == 0) return(NA)
  dm <- distances(sub_g)
  valores_validos <- dm[is.finite(dm) & dm > 0]
  if (length(valores_validos) == 0) return(NA)
  mean(valores_validos)
}

# Nuestros valores observados

dm_AD_GABAergic   <- distances(grafo_AD_sub)
dm_ctrl_GABAergic <- distances(grafo_ctrl_sub)

obs_AD_GABAergic   <- mean(dm_AD_GABAergic[is.finite(dm_AD_GABAergic)     & dm_AD_GABAergic   > 0])
obs_ctrl_GABAergic <- mean(dm_ctrl_GABAergic[is.finite(dm_ctrl_GABAergic) & dm_ctrl_GABAergic > 0])

#Parametros
n_perm         <- 1000
gene_set_GABA  <- GABAergic_pathways_AD
set.seed(42)

n_AD_GABAergic   <- length(Genes_in_AD_GABAergic)
n_ctrl_GABAergic <- length(Genes_in_ctr_GABAergic)

all_genes_AD   <- V(grafo_AD)$name
all_genes_ctrl <- V(grafo_ctrl)$name

#Label permutation
null_AD_GABAergic      <- numeric(n_perm)
null_control_GABAergic <- numeric(n_perm)

for (i in seq_len(n_perm)) {
  if (i %% 100 == 0) cat(sprintf("Permutación %d / %d\n", i, n_perm))
  genes_rand             <- sample(all_genes_AD, n_AD_GABAergic)
  null_AD_GABAergic[i]   <- mean_dist_subgraph_GABAergic(grafo_AD, genes_rand)
}

for (i in seq_len(n_perm)) {
  if (i %% 100 == 0) cat(sprintf("Permutación %d / %d\n", i, n_perm))
  genes_rand                  <- sample(all_genes_ctrl, n_ctrl_GABAergic)
  null_control_GABAergic[i]   <- mean_dist_subgraph_GABAergic(grafo_ctrl, genes_rand)
}

#P- value, z-score

pval_AD_GABAergic   <- mean(null_AD_GABAergic      >= obs_AD_GABAergic,   na.rm = TRUE)
pval_ctrl_GABAergic <- mean(null_control_GABAergic >= obs_ctrl_GABAergic, na.rm = TRUE)

pval_AD_GABAergic_2t   <- 2 * min(pval_AD_GABAergic,   1 - pval_AD_GABAergic)
pval_ctrl_GABAergic_2t <- 2 * min(pval_ctrl_GABAergic, 1 - pval_ctrl_GABAergic)

ci_AD_GABAergic   <- quantile(null_AD_GABAergic,      probs = c(0.025, 0.975), na.rm = TRUE)
ci_ctrl_GABAergic <- quantile(null_control_GABAergic, probs = c(0.025, 0.975), na.rm = TRUE)

z_AD_GABAergic   <- (obs_AD_GABAergic   - mean(null_AD_GABAergic,      na.rm = TRUE)) / sd(null_AD_GABAergic,      na.rm = TRUE)
z_ctrl_GABAergic <- (obs_ctrl_GABAergic - mean(null_control_GABAergic, na.rm = TRUE)) / sd(null_control_GABAergic, na.rm = TRUE)

summary_permutation_GABAergic <- data.frame(
  Group              = c("AD", "Control"),
  Observed_mean_dist = c(obs_AD_GABAergic,        obs_ctrl_GABAergic),
  Null_mean          = c(mean(null_AD_GABAergic,      na.rm = TRUE), mean(null_control_GABAergic, na.rm = TRUE)),
  Null_SD            = c(sd(null_AD_GABAergic,        na.rm = TRUE), sd(null_control_GABAergic,   na.rm = TRUE)),
  CI_lower_95        = c(ci_AD_GABAergic[1],   ci_ctrl_GABAergic[1]),
  CI_upper_95        = c(ci_AD_GABAergic[2],   ci_ctrl_GABAergic[2]),
  Z_score            = c(z_AD_GABAergic,       z_ctrl_GABAergic),
  P_value_2tailed    = c(pval_AD_GABAergic_2t, pval_ctrl_GABAergic_2t),
  N_permutations     = n_perm
)

print(summary_permutation_GABAergic)

write.csv(
  summary_permutation_GABAergic,
  file = file.path(ruta_GABAergic, "permutation_test_GABAergic.csv"),
  row.names = FALSE
)
