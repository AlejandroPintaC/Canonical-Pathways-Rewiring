ruta_distancias <- "/Users/alejandropintacastro/Downloads/INMEGEN/distancias"

summary_results_GABAergic$Pathway <- "GABAergic"
summary_results_Neurexins$Pathway <- "Neurexins and Neuroligins"
summary_results_SNARE$Pathway <- "SNARE"
summary_results_Synap$Pathway <- "Synaptogenesis"

#Unir en una sola tabla
summary_all_distancias <- bind_rows(
  summary_results_GABAergic,
  summary_results_Neurexins,
  summary_results_SNARE,
  summary_results_Synap
)

write.csv(
  summary_all_distancias,
  file = file.path(ruta_distancias, "Summary distances.csv"),
  row.names = FALSE
)
