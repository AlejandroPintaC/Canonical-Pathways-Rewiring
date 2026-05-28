pacman::p_load(
  igraph,
  dplyr,
  ggplot2,
  pheatmap
)

# Tabla de conversión para SNARE
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

# Función para reemplazar ENSEMBL por symbols
reemplazar_labels <- function(mat, conversion) {
  idx_row <- match(rownames(mat), conversion$ensembl)
  idx_col <- match(colnames(mat), conversion$ensembl)
  rownames(mat) <- ifelse(!is.na(idx_row), conversion$symbol[idx_row], rownames(mat))
  colnames(mat) <- ifelse(!is.na(idx_col), conversion$symbol[idx_col], colnames(mat))
  mat
}

# Ordenar matrices - corregido con corchete cerrado
dist_matrix_AD_clean_SNARE <- dist_matrix_AD_clean_SNARE[
  order(rownames(dist_matrix_AD_clean_SNARE)),
  order(colnames(dist_matrix_AD_clean_SNARE))
]

dist_matrix_ctrl_clean_SNARE <- dist_matrix_ctrl_clean_SNARE[
  order(rownames(dist_matrix_ctrl_clean_SNARE)),
  order(colnames(dist_matrix_ctrl_clean_SNARE))
]

# Aplicar conversión
dist_matrix_AD_clean_SNARE   <- reemplazar_labels(dist_matrix_AD_clean_SNARE,   conversion_AD)
dist_matrix_ctrl_clean_SNARE <- reemplazar_labels(dist_matrix_ctrl_clean_SNARE, conversion_ctrl)

# Guardar heatmap AD
png(file.path(ruta_SNARE, "heatmap_AD_SNARE.png"),
    width = 2000, height = 2000, res = 300)

pheatmap(dist_matrix_AD_clean_SNARE,
         color = Heatmap_colors,
         show_rownames = TRUE,
         show_colnames = TRUE,
         clustering_method = "complete",
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         main = "SNARE network distances (AD)")

dev.off()

# Guardar heatmap Control
png(file.path(ruta_SNARE, "heatmap_Control_SNARE.png"),
    width = 2000, height = 2000, res = 300)

pheatmap(dist_matrix_ctrl_clean_SNARE,
         color = Heatmap_colors,
         show_rownames = TRUE,
         show_colnames = TRUE,
         clustering_method = "complete",
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         main = "SNARE network distances (control)")

dev.off()
