# Creamos nuestras tabla de conversión AD
conversion_AD <- data.frame(
  ensembl = V(grafo_AD)$name,
  symbol  = V(grafo_AD)$name_trad
) %>%
  mutate(symbol = ifelse(symbol == ensembl | is.na(symbol), ensembl, symbol))

# Ahora creamos nuestra tabla de conversion para Control
conversion_ctrl <- data.frame(
  ensembl = V(grafo_ctrl)$name,
  symbol  = V(grafo_ctrl)$name_trad
) %>%
  mutate(symbol = ifelse(symbol == ensembl | is.na(symbol), ensembl, symbol))

# Verificar cuántos no tienen traducción
sum(conversion_AD$symbol == conversion_AD$ensembl)
sum(conversion_ctrl$symbol == conversion_ctrl$ensembl)
