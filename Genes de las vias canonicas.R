# Aqui vamos a guardar los genes de las vias 
dir.create("/Users/alejandropintacastro/Desktop/Vias_canonicas")
VC <- "/Users/alejandropintacastro/Desktop/Vias_canonicas"

write.csv(GABAergic_plot_expr, 
          file = file.path(VC, "GABAergic_plot_expr.csv"), 
          row.names = FALSE)

write.csv(Neurexins_plot_expr, 
          file = file.path(VC, "Neurexins_plot_expr.csv"), 
          row.names = FALSE)

write.csv(SNARE_plot_expr, 
          file = file.path(VC, "SNARE_plot_expr.csv"), 
          row.names = FALSE)

write.csv(synaptogensis_plot_expr, 
          file = file.path(VC, "Synaptogenesis_plot_expr.csv"), 
          row.names = FALSE)
