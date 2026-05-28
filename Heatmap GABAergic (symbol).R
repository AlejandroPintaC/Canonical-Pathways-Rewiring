pacman::p_load(
  igraph,
  dplyr,
  ggplot2,
  pheatmap
)
# Tabla de conversión para esta vía GABAergica
conversion_AD <- data.frame(
  ensembl = V(grafo_AD)$name,
  symbol  = V(grafo_AD)$name_trad
) %>%
  mutate(symbol = ifelse(symbol == ensembl | is.na(symbol), ensembl, symbol))

conversion_ctrl <- data.frame(
  ensembl = V(grafo_ctrl)$name,
  symbol  = V(grafo_ctrl)$name_trad
) %>%
  mutate(symbol = ifelse(symbol == ensembl | is.na(symbol), ensembl, symbol))

# Función para reemplazar ENSEMBL por symbols en una matriz
reemplazar_labels <- function(mat, conversion) {
  idx_row <- match(rownames(mat), conversion$ensembl)
  idx_col <- match(colnames(mat), conversion$ensembl)
  rownames(mat) <- ifelse(!is.na(idx_row), conversion$symbol[idx_row], rownames(mat))
  colnames(mat) <- ifelse(!is.na(idx_col), conversion$symbol[idx_col], colnames(mat))
  mat
}

# Aplicar conversión a las matrices limpias
dist_matrix_AD_clean_GABAergic   <- reemplazar_labels(dist_matrix_AD_clean_GABAergic,   conversion_AD)
dist_matrix_ctrl_clean_GABAergic <- reemplazar_labels(dist_matrix_ctrl_clean_GABAergic, conversion_ctrl)

# Heatmap AD con gene symbols
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

# Heatmap Control con gene symbols
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
