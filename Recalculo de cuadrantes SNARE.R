# Recalcular cuadrantes con umbrales correctos - SNARE
umbral_grado_ctrl_SNARE <- median(snare_wide$Grado_Control,   na.rm = TRUE)
umbral_expr_ctrl_SNARE  <- median(snare_wide$Expr_Control,    na.rm = TRUE)
umbral_grado_AD_SNARE   <- median(snare_wide$Grado_Alzheimer, na.rm = TRUE)
umbral_expr_AD_SNARE    <- median(snare_wide$Expr_Alzheimer,  na.rm = TRUE)

# Recalcular cuadrantes
snare_wide <- snare_wide %>%
  mutate(
    cuadrante_Control = case_when(
      Grado_Control >  umbral_grado_ctrl_SNARE & Expr_Control >  umbral_expr_ctrl_SNARE ~ "I",
      Grado_Control <= umbral_grado_ctrl_SNARE & Expr_Control >  umbral_expr_ctrl_SNARE ~ "II",
      Grado_Control >  umbral_grado_ctrl_SNARE & Expr_Control <= umbral_expr_ctrl_SNARE ~ "III",
      Grado_Control <= umbral_grado_ctrl_SNARE & Expr_Control <= umbral_expr_ctrl_SNARE ~ "IV"
    ),
    cuadrante_Alzheimer = case_when(
      Grado_Alzheimer >  umbral_grado_AD_SNARE & Expr_Alzheimer >  umbral_expr_AD_SNARE ~ "I",
      Grado_Alzheimer <= umbral_grado_AD_SNARE & Expr_Alzheimer >  umbral_expr_AD_SNARE ~ "II",
      Grado_Alzheimer >  umbral_grado_AD_SNARE & Expr_Alzheimer <= umbral_expr_AD_SNARE ~ "III",
      Grado_Alzheimer <= umbral_grado_AD_SNARE & Expr_Alzheimer <= umbral_expr_AD_SNARE ~ "IV"
    ),
    cambio_cuadrante = cuadrante_Control != cuadrante_Alzheimer
  )

# Ver genes que cambian
snare_wide %>%
  filter(cambio_cuadrante) %>%
  select(Symbol, Grado_Control, Expr_Control, cuadrante_Control,
         Grado_Alzheimer, Expr_Alzheimer, cuadrante_Alzheimer) %>%
  print(width = Inf)

# Actualizar snare_genes_detalles con cuadrantes corregidos
snare_genes_detalles <- snare_wide %>%
  filter(cambio_cuadrante) %>%
  select(Symbol, Grado_Control, Expr_Control,
         Grado_Alzheimer, Expr_Alzheimer,
         cuadrante_Control, cuadrante_Alzheimer)

# Actualizar genes_SNARE
genes_SNARE <- snare_genes_detalles %>%
  mutate(Pathway = "SNARE")

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



