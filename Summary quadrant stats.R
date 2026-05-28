pacman::p_load(dplyr)

# Función para calcular estadísticas de cuadrantes
calcular_estadisticas <- function(df_wide, pathway_name) {
  
  total_genes   <- nrow(df_wide)
  genes_cambian <- sum(df_wide$cambio_cuadrante, na.rm = TRUE)
  pct_cambian   <- round(genes_cambian / total_genes * 100, 1)
  
  df_wide <- df_wide %>%
    mutate(
      cambio_grado = case_when(
        Grado_Alzheimer > Grado_Control ~ "Increased",
        Grado_Alzheimer < Grado_Control ~ "Decreased",
        TRUE ~ "No change"
      ),
      cambio_expr = case_when(
        Expr_Alzheimer > Expr_Control ~ "Increased",
        Expr_Alzheimer < Expr_Control ~ "Decreased",
        TRUE ~ "No change"
      )
    )
  
  data.frame(
    Pathway                 = pathway_name,
    Total_genes             = total_genes,
    Genes_changing_quadrant = genes_cambian,
    Pct_changing_quadrant   = pct_cambian,
    Pct_degree_increased    = round(sum(df_wide$cambio_grado == "Increased", na.rm = TRUE) / total_genes * 100, 1),
    Pct_degree_decreased    = round(sum(df_wide$cambio_grado == "Decreased", na.rm = TRUE) / total_genes * 100, 1),
    Pct_expr_increased      = round(sum(df_wide$cambio_expr  == "Increased", na.rm = TRUE) / total_genes * 100, 1),
    Pct_expr_decreased      = round(sum(df_wide$cambio_expr  == "Decreased", na.rm = TRUE) / total_genes * 100, 1)
  )
}

# Aplicar a cada vía
stats_GABA  <- calcular_estadisticas(gaba_wide,             "GABAergic")
stats_NN    <- calcular_estadisticas(neurexins_wide,         "Neurexins & Neuroligins")
stats_SNARE <- calcular_estadisticas(snare_wide,             "SNARE")
stats_Synap <- calcular_estadisticas(synaptogenesis_wide,    "Synaptogenesis")

# Tabla resumen
summary_quadrant_stats <- bind_rows(
  stats_GABA, stats_NN, stats_SNARE, stats_Synap
)

print(summary_quadrant_stats)

write.csv(
  summary_quadrant_stats,
  file = file.path(ruta_final, "summary_quadrant_statistics.csv"),
  row.names = FALSE
)


