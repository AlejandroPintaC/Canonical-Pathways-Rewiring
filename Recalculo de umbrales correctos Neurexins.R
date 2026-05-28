# Recalcular cuadrantes con umbrales correctos
umbral_grado_ctrl_NN <- median(neurexins_wide$Grado_Control,   na.rm = TRUE)
umbral_expr_ctrl_NN  <- median(neurexins_wide$Expr_Control,    na.rm = TRUE)
umbral_grado_AD_NN   <- median(neurexins_wide$Grado_Alzheimer, na.rm = TRUE)
umbral_expr_AD_NN    <- median(neurexins_wide$Expr_Alzheimer,  na.rm = TRUE)

# Recalcular cuadrantes
neurexins_wide <- neurexins_wide %>%
  mutate(
    cuadrante_Control = case_when(
      Grado_Control >  umbral_grado_ctrl_NN & Expr_Control >  umbral_expr_ctrl_NN ~ "I",
      Grado_Control <= umbral_grado_ctrl_NN & Expr_Control >  umbral_expr_ctrl_NN ~ "II",
      Grado_Control >  umbral_grado_ctrl_NN & Expr_Control <= umbral_expr_ctrl_NN ~ "III",
      Grado_Control <= umbral_grado_ctrl_NN & Expr_Control <= umbral_expr_ctrl_NN ~ "IV"
    ),
    cuadrante_Alzheimer = case_when(
      Grado_Alzheimer >  umbral_grado_AD_NN & Expr_Alzheimer >  umbral_expr_AD_NN ~ "I",
      Grado_Alzheimer <= umbral_grado_AD_NN & Expr_Alzheimer >  umbral_expr_AD_NN ~ "II",
      Grado_Alzheimer >  umbral_grado_AD_NN & Expr_Alzheimer <= umbral_expr_AD_NN ~ "III",
      Grado_Alzheimer <= umbral_grado_AD_NN & Expr_Alzheimer <= umbral_expr_AD_NN ~ "IV"
    ),
    cambio_cuadrante = cuadrante_Control != cuadrante_Alzheimer
  )

# Ver genes que cambian
neurexins_wide %>%
  filter(cambio_cuadrante) %>%
  select(Symbol, Grado_Control, Expr_Control, cuadrante_Control,
         Grado_Alzheimer, Expr_Alzheimer, cuadrante_Alzheimer) %>%
  print(width = Inf)

# Actualizar neurexins_genes_detalles con cuadrantes corregidos
neurexins_genes_detalles <- neurexins_wide %>%
  filter(cambio_cuadrante) %>%
  select(Symbol, Grado_Control, Expr_Control,
         Grado_Alzheimer, Expr_Alzheimer,
         cuadrante_Control, cuadrante_Alzheimer)

# Actualizar genes_NN
genes_NN <- neurexins_genes_detalles %>%
  mutate(Pathway = "Neurexins & Neuroligins")

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


