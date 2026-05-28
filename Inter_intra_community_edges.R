pacman::p_load(dplyr)

# Calcular número de aristas entre comunidades (inter-community edges)
calcular_edges_inter <- function(grafo) {
  comunidades <- V(grafo)$community
  names(comunidades) <- V(grafo)$name
  
  aristas <- as_edgelist(grafo, names = TRUE)
  
  com_origen  <- comunidades[aristas[, 1]]
  com_destino <- comunidades[aristas[, 2]]
  
  inter <- sum(com_origen != com_destino, na.rm = TRUE)
  intra <- sum(com_origen == com_destino, na.rm = TRUE)
  
  data.frame(
    total_edges           = ecount(grafo),
    intra_community_edges = intra,
    inter_community_edges = inter,
    prop_inter            = round(inter / ecount(grafo), 3),
    prop_intra            = round(intra / ecount(grafo), 3)
  )
}

edges_AD   <- calcular_edges_inter(grafo_AD)
edges_ctrl <- calcular_edges_inter(grafo_ctrl)

resultado <- bind_rows(
  data.frame(Group = "AD",      edges_AD),
  data.frame(Group = "Control", edges_ctrl)
)

print(resultado)

write.csv(
  resultado,
  file = file.path(ruta_final, "inter_intra_community_edges.csv"),
  row.names = FALSE
)
