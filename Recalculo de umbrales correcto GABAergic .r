pacman::p_load(dplyr)

# Recalcular cuadrantes con umbrales correctos - GABAergic
umbral_grado_ctrl_GABA <- median(gaba_wide$Grado_Control,   na.rm = TRUE)
umbral_expr_ctrl_GABA  <- median(gaba_wide$Expr_Control,    na.rm = TRUE)
umbral_grado_AD_GABA   <- median(gaba_wide$Grado_Alzheimer, na.rm = TRUE)
umbral_expr_AD_GABA    <- median(gaba_wide$Expr_Alzheimer,  na.rm = TRUE)

# Recalcular cuadrantes
gaba_wide <- gaba_wide %>%
  mutate(
    cuadrante_Control = case_when(
      Grado_Control >  umbral_grado_ctrl_GABA & Expr_Control >  umbral_expr_ctrl_GABA ~ "I",
      Grado_Control <= umbral_grado_ctrl_GABA & Expr_Control >  umbral_expr_ctrl_GABA ~ "II",
      Grado_Control >  umbral_grado_ctrl_GABA & Expr_Control <= umbral_expr_ctrl_GABA ~ "III",
      Grado_Control <= umbral_grado_ctrl_GABA & Expr_Control <= umbral_expr_ctrl_GABA ~ "IV"
    ),
    cuadrante_Alzheimer = case_when(
      Grado_Alzheimer >  umbral_grado_AD_GABA & Expr_Alzheimer >  umbral_expr_AD_GABA ~ "I",
      Grado_Alzheimer <= umbral_grado_AD_GABA & Expr_Alzheimer >  umbral_expr_AD_GABA ~ "II",
      Grado_Alzheimer >  umbral_grado_AD_GABA & Expr_Alzheimer <= umbral_expr_AD_GABA ~ "III",
      Grado_Alzheimer <= umbral_grado_AD_GABA & Expr_Alzheimer <= umbral_expr_AD_GABA ~ "IV"
    ),
    cambio_cuadrante = cuadrante_Control != cuadrante_Alzheimer
  )

# Ver genes que cambian
gaba_wide %>%
  filter(cambio_cuadrante) %>%
  select(Symbol, Grado_Control, Expr_Control, cuadrante_Control,
         Grado_Alzheimer, Expr_Alzheimer, cuadrante_Alzheimer) %>%
  print(width = Inf)

# Actualizar gaba_genes_cambio_detalles
gaba_genes_cambio_detalles <- gaba_wide %>%
  filter(cambio_cuadrante) %>%
  select(Symbol, Grado_Control, Expr_Control,
         Grado_Alzheimer, Expr_Alzheimer,
         cuadrante_Control, cuadrante_Alzheimer)

# Guardar archivo individual
write.csv(
  gaba_genes_cambio_detalles,
  file = file.path(ruta_GABAergic, "GABAergic_genes_cambio_cuadricula.csv"),
  row.names = FALSE
)
