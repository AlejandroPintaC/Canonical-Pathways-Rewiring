pacman::p_load(pheatmap)

file_path <- "/Users/alejandropintacastro/Downloads/INMEGEN/ROSMAP/Resultados Jaccard AD vs Control.csv"

# Leer archivo
raw_full <- read.csv(file_path, check.names = FALSE, stringsAsFactors = FALSE)

# Usar columna "network" como rownames y eliminarla
if (tolower(names(raw_full)[1]) == "network") {
  rownames(raw_full) <- raw_full[[1]]
  raw_full <- raw_full[-1]
}

# Convertir a matriz numérica
jaccard_mat <- apply(raw_full, 2, function(x) as.numeric(as.character(x)))
rownames(jaccard_mat) <- rownames(raw_full)

# Eliminar filas/columnas vacías
jaccard_mat <- jaccard_mat[rowSums(!is.na(jaccard_mat)) > 0, , drop = FALSE]
jaccard_mat <- jaccard_mat[, colSums(!is.na(jaccard_mat)) > 0, drop = FALSE]

# Ocultar TODAS las etiquetas individuales
col_labels <- rep("", ncol(jaccard_mat))
row_labels <- rep("", nrow(jaccard_mat))

png("heatmap_Jaccard_ADvsCtrl.png", width = 1800, height = 1600, res = 300)

pheatmap(
  jaccard_mat,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  scale = "none",
  border_color = NA,
  main = "Jaccard Index",
  subtitle = "Control",          
  fontsize = 6,
  fontsize_row = 6,
  fontsize_col = 6,
  labels_row = row_labels,       
  labels_col = col_labels,      
  annotation_row = data.frame(Group = rep("AD", nrow(jaccard_mat))),
  annotation_names_row = TRUE,   
  color = colorRampPalette(c("white", "yellow", "orange", "red"))(100)
)


library(pheatmap)
library(grid)

labels_col <- colnames(jaccard_mat)
labels_row <- rownames(jaccard_mat)

# Mostrar cada 5
labels_col[!(seq_along(labels_col) %% 5 == 1)] <- ""
labels_row[!(seq_along(labels_row) %% 5 == 1)] <- ""

png("heatmap_final.png", width = 2200, height = 2200, res = 300)

# Márgenes más equilibrados
par(mar = c(6, 6, 4, 4))

pheatmap(
  jaccard_mat,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  border_color = NA,
  main = "Jaccard Index (AD vs Control)",
  breaks = seq(0, 1, length.out = 100),
  color = colorRampPalette(c("white", "#fdd49e", "#fc8d59", "#d7301f"))(99),
  labels_col = labels_col,
  labels_row = labels_row,
  fontsize_col = 8,   # ↓ números eje X más pequeños
  fontsize_row = 8    # ↓ números eje Y más pequeños
)

# Etiquetas principales más discretas
grid.text("Control",
          x = 0.5,
          y = 0.01,
          gp = gpar(fontsize = 8))

grid.text("AD",
          x = 0.01,
          y = 0.5,
          rot = 90,
          gp = gpar(fontsize = 8))

dev.off()
