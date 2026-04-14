# Instalar gprofiler2 (si no lo tienes)
 install.packages("gprofiler2")

# Cargar la librería
library(gprofiler2)
 
 # Supongamos que quieres analizar la COMUNIDAD 3 (477 genes)
 genes_comunidad <- comm_list[[3]]  # Ajusta el número según la comunidad que quieras
 
 # Verificar los genes
 cat("Analizando", length(genes_comunidad), "genes de la comunidad\n")
 print(head(genes_comunidad, 10))  # Primeros 10 genes
 
 # Correr el análisis completo
 resultados <- gost(
   query = genes_comunidad,     # Tus genes Ensembl
   organism = "hsapiens",       # Organismo (humano)
   sources = c("GO:BP", "GO:MF", "GO:CC", "KEGG", "REAC", "WP"),  # Bases de datos
   significant = TRUE,          # Solo resultados significativos
   correction_method = "g_SCS"  # Método de corrección
 )
 
 # Ver resultados en la consola
 print(resultados)
 
 # Ver tabla de resultados
 tabla_resultados <- resultados$result
 head(tabla_resultados[, c("term_name", "p_value", "term_size", "intersection_size")])
 
 # Crear gráfica de los top términos
 gostplot(resultados, capped = TRUE, interactive = TRUE)
 
 # Top 10 términos más significativos
 top_terminos <- head(tabla_resultados[order(tabla_resultados$p_value), ], 10)
 print(top_terminos[, c("term_name", "p_value", "source")])
 
 # Analizar las 3 comunidades principales automáticamente
 for (i in 1:3) {
   cat("\n=== ANALIZANDO COMUNIDAD", i, "===\n")
   
   resultados_com <- gost(
     query = comm_list[[i]],
     organism = "hsapiens",
     sources = c("GO:BP", "KEGG", "REAC"),
     significant = TRUE
   )
   
   # Exportar resultados
   if (!is.null(resultados_com$result)) {
     archivo_nombre <- paste0("enriquecimiento_comunidad_", i, ".csv")
     write.csv(resultados_com$result, archivo_nombre, row.names = FALSE)
     cat("✓", archivo_nombre, "exportado\n")
   }
 }