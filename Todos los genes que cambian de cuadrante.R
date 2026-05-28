pacman::p_load(dplyr)

# Agregar columna de vía a cada dataframe
genes_GABA  <- gaba_genes_cambio_detalles %>% mutate(Pathway = "GABAergic")
genes_NN    <- neurexins_genes_detalles   %>% mutate(Pathway = "Neurexins & Neuroligins")
genes_SNARE <- snare_genes_detalles       %>% mutate(Pathway = "SNARE")
genes_Synap <- genes_cambio_detalles      %>% mutate(Pathway = "Synaptogenesis")

# Unir todos en un solo dataframe
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

