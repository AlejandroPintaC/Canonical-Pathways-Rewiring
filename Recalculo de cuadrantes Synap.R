# Recalcular cuadrantes con umbrales correctos - Synaptogenesis
umbral_grado_ctrl_Synap <- median(synaptogenesis_wide$Grado_Control,   na.rm = TRUE)
umbral_expr_ctrl_Synap  <- median(synaptogenesis_wide$Expr_Control,    na.rm = TRUE)
umbral_grado_AD_Synap   <- median(synaptogenesis_wide$Grado_Alzheimer, na.rm = TRUE)
umbral_expr_AD_Synap    <- median(synaptogenesis_wide$Expr_Alzheimer,  na.rm = TRUE)

# Recalcular cuadrantes
synaptogenesis_wide <- synaptogenesis_wide %>%
  mutate(
    cuadrante_Control = case_when(
      Grado_Control >  umbral_grado_ctrl_Synap & Expr_Control >  umbral_expr_ctrl_Synap ~ "I",
      Grado_Control <= umbral_grado_ctrl_Synap & Expr_Control >  umbral_expr_ctrl_Synap ~ "II",
      Grado_Control >  umbral_grado_ctrl_Synap & Expr_Control <= umbral_expr_ctrl_Synap ~ "III",
      Grado_Control <= umbral_grado_ctrl_Synap & Expr_Control <= umbral_expr_ctrl_Synap ~ "IV"
    ),
    cuadrante_Alzheimer = case_when(
      Grado_Alzheimer >  umbral_grado_AD_Synap & Expr_Alzheimer >  umbral_expr_AD_Synap ~ "I",
      Grado_Alzheimer <= umbral_grado_AD_Synap & Expr_Alzheimer >  umbral_expr_AD_Synap ~ "II",
      Grado_Alzheimer >  umbral_grado_AD_Synap & Expr_Alzheimer <= umbral_expr_AD_Synap ~ "III",
      Grado_Alzheimer <= umbral_grado_AD_Synap & Expr_Alzheimer <= umbral_expr_AD_Synap ~ "IV"
    ),
    cambio_cuadrante = cuadrante_Control != cuadrante_Alzheimer
  )

# Ver genes que cambian
synaptogenesis_wide %>%
  filter(cambio_cuadrante) %>%
  select(Symbol, Grado_Control, Expr_Control, cuadrante_Control,
         Grado_Alzheimer, Expr_Alzheimer, cuadrante_Alzheimer) %>%
  print(width = Inf)

# Actualizar genes_cambio_detalles con cuadrantes corregidos
genes_cambio_detalles <- synaptogenesis_wide %>%
  filter(cambio_cuadrante) %>%
  select(Symbol, Grado_Control, Expr_Control,
         Grado_Alzheimer, Expr_Alzheimer,
         cuadrante_Control, cuadrante_Alzheimer)

# Actualizar genes_Synap
genes_Synap <- genes_cambio_detalles %>%
  mutate(Pathway = "Synaptogenesis")

# Reconstruir CSV general
todos_los_cambios <- bind_rows(
  genes_GABA,
  genes_NN,
  genes_SNARE,
  genes_Synap
)

print(todos_los_cambios)

write.csv(
  todos_los_cambios,
  file = file.path(ruta_final, "todos_genes_cambio_cuadrante.csv"),
  row.names = FALSE
)
