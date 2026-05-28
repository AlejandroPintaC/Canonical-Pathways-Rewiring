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
         color = Heatmap_colors,
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
         color = Heatmap_colors,
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
         color = Heatmap_colors,
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
         color = Heatmap_colors,
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
Heatmap_colors <- colorRampPalette(c("#f7fbff", "#6baed6", "#08306b"))(100)


#Establecer funciones similares
rewire_graph_Synap <- function(g, niter = NULL) {
  if (is.null(niter)) niter <- ecount(g) * 10
  rewire(g, with = keeping_degseq(niter = niter))
}

mean_dist_subgraph_Synap <- function(full_graph, gene_set) {
  genes_in_graph <- intersect(V(full_graph)$name, gene_set)
  if (length(genes_in_graph) < 2) return(NA)
  sub_g <- induced_subgraph(full_graph, vids = genes_in_graph)
  if (ecount(sub_g) == 0) return(NA)
  dm <- distances(sub_g)
  valores_validos <- dm[is.finite(dm) & dm > 0]
  if (length(valores_validos) == 0) return(NA)
  mean(valores_validos)
}

#Nuestros valores observados 
dm_AD_Synap   <- distances(grafo_AD_sub_Synap)
dm_ctrl_Synap <- distances(grafo_ctrl_sub_Synap)

obs_AD_Synap   <- mean(dm_AD_Synap[is.finite(dm_AD_Synap)     & dm_AD_Synap   > 0])
obs_ctrl_Synap <- mean(dm_ctrl_Synap[is.finite(dm_ctrl_Synap) & dm_ctrl_Synap > 0])

#Definir nuestros parametros
n_perm     <- 1000
alpha      <- 0.05
gene_set_Synap <- Synap_pathways_AD
set.seed(42)

#Label permutation

all_genes_AD   <- V(grafo_AD)$name
all_genes_ctrl <- V(grafo_ctrl)$name

n_AD_Synap   <- length(Genes_in_AD_Synap)
n_ctrl_Synap <- length(Genes_in_ctr_Synap)

null_AD_Synap      <- numeric(n_perm)
null_control_Synap <- numeric(n_perm)

for (i in seq_len(n_perm)) {
  if (i %% 100 == 0) cat(sprintf("Permutación %d / %d\n", i, n_perm))
  genes_rand         <- sample(all_genes_AD, n_AD_Synap)
  null_AD_Synap[i]   <- mean_dist_subgraph_Synap(grafo_AD, genes_rand)
}

for (i in seq_len(n_perm)) {
  if (i %% 100 == 0) cat(sprintf("Permutación %d / %d\n", i, n_perm))
  genes_rand              <- sample(all_genes_ctrl, n_ctrl_Synap)
  null_control_Synap[i]   <- mean_dist_subgraph_Synap(grafo_ctrl, genes_rand)
}

#p-VALUE, Z-SCORE
pval_AD_Synap   <- mean(null_AD_Synap      >= obs_AD_Synap,   na.rm = TRUE)
pval_ctrl_Synap <- mean(null_control_Synap >= obs_ctrl_Synap, na.rm = TRUE)

pval_AD_Synap_2t   <- 2 * min(pval_AD_Synap,   1 - pval_AD_Synap)
pval_ctrl_Synap_2t <- 2 * min(pval_ctrl_Synap, 1 - pval_ctrl_Synap)

ci_AD_Synap   <- quantile(null_AD_Synap,      probs = c(0.025, 0.975), na.rm = TRUE)
ci_ctrl_Synap <- quantile(null_control_Synap, probs = c(0.025, 0.975), na.rm = TRUE)

z_AD_Synap   <- (obs_AD_Synap   - mean(null_AD_Synap,      na.rm = TRUE)) / sd(null_AD_Synap,      na.rm = TRUE)
z_ctrl_Synap <- (obs_ctrl_Synap - mean(null_control_Synap, na.rm = TRUE)) / sd(null_control_Synap, na.rm = TRUE)

summary_permutation_Synap <- data.frame(
  Group              = c("AD", "Control"),
  Observed_mean_dist = c(obs_AD_Synap,        obs_ctrl_Synap),
  Null_mean          = c(mean(null_AD_Synap,      na.rm = TRUE), mean(null_control_Synap, na.rm = TRUE)),
  Null_SD            = c(sd(null_AD_Synap,        na.rm = TRUE), sd(null_control_Synap,   na.rm = TRUE)),
  CI_lower_95        = c(ci_AD_Synap[1],   ci_ctrl_Synap[1]),
  CI_upper_95        = c(ci_AD_Synap[2],   ci_ctrl_Synap[2]),
  Z_score            = c(z_AD_Synap,       z_ctrl_Synap),
  P_value_2tailed    = c(pval_AD_Synap_2t, pval_ctrl_Synap_2t),
  N_permutations     = n_perm
)

print(summary_permutation_Synap)

write.csv(
  summary_permutation_Synap,
  file = file.path(ruta_Synap, "permutation_test_Synap.csv"),
  row.names = FALSE
)


# Ver si ya tienes algún objeto con la conversión
head(V(grafo_AD)$name)        # ¿son ENSEMBL IDs?
head(V(grafo_AD)$name_trad)   # ¿aquí están los gene symbols?

