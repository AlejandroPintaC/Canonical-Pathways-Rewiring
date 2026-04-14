#### GABAergic ####
# Conexion a Ensembl (humano)
ensembl <- useMart("ensembl", dataset = "hsapiens_gene_ensembl")

# Obtener GO para Neurexinas
go_GABAergic_c <- getBM(
  attributes = c("ensembl_gene_id", "go_id", "name_1006"),
  filters = "ensembl_gene_id",
  values = unique(GABAergic_plot$Ensembl),
  mart = ensembl
)
colnames(go_GABAergic_c) <- c("Ensembl", "GO_ID", "GO_Term")

# Unir
GABAergic_complete <- left_join(GABAergic_plot, go_GABAergic_c, by = "Ensembl")

write.csv(GABAergic_complete, "Tabla con TODA la info de GABAergic.csv")

#### GABAreceptor ####
# Obtener GO para Neurexinas
go_GABAreceptor_c <- getBM(
  attributes = c("ensembl_gene_id", "go_id", "name_1006"),
  filters = "ensembl_gene_id",
  values = unique(gaba_receptor_con_top$Ensembl),
  mart = ensembl
)
colnames(go_GABAreceptor_c) <- c("Ensembl", "GO_ID", "GO_Term")

# Unir
GABAreceptor_complete <- left_join(gaba_receptor_con_top, go_GABAreceptor_c, by = "Ensembl")

write.csv(GABAreceptor_complete, "Tabla con TODA la info de GABAreceptor.csv")

#### Glutamatergic #####
# Obtener GO para Neurexinas
go_Glut_c <- getBM(
  attributes = c("ensembl_gene_id", "go_id", "name_1006"),
  filters = "ensembl_gene_id",
  values = unique(glutaminergic_con_top$Ensembl),
  mart = ensembl
)
colnames(go_Glut_c) <- c("Ensembl", "GO_ID", "GO_Term")

# Unir
Glut_complete <- left_join(glutaminergic_con_top, go_Glut_c, by = "Ensembl")

write.csv(Glut_complete, "Tabla con TODA la info de Glut.csv")

###### Neurexins and neuroligins ######
# Conexion a Ensembl (humano)
ensembl <- useMart("ensembl", dataset = "hsapiens_gene_ensembl")

# Obtener GO para Neurexinas
go_neurexins_c <- getBM(
  attributes = c("ensembl_gene_id", "go_id", "name_1006"),
  filters = "ensembl_gene_id",
  values = unique(neurexins_con_top$Ensembl),
  mart = ensembl
)
colnames(go_neurexins_c) <- c("Ensembl", "GO_ID", "GO_Term")

# Unir
Neurexins_complete <- left_join(neurexins_con_top, go_neurexins_c, by = "Ensembl")
 
write.csv(Neurexins_complete, "Tabla con TODA la info de Neurexins.csv")

#### SNARE ####
# Obtener GO para Neurexinas
go_SNARE_c <- getBM(
  attributes = c("ensembl_gene_id", "go_id", "name_1006"),
  filters = "ensembl_gene_id",
  values = unique(snare_con_top$Ensembl),
  mart = ensembl
)
colnames(go_SNARE_c) <- c("Ensembl", "GO_ID", "GO_Term")

# Unir
SNARE_complete <- left_join(snare_con_top, go_SNARE_c, by = "Ensembl")

write.csv(SNARE_complete, "Tabla con TODA la info de SNARE.csv")

#### Synaptogenesis ####
# Obtener GO para Neurexinas
go_Synap_c <- getBM(
  attributes = c("ensembl_gene_id", "go_id", "name_1006"),
  filters = "ensembl_gene_id",
  values = unique(synaptogenesis_con_top$Ensembl),
  mart = ensembl
)
colnames(go_Synap_c) <- c("Ensembl", "GO_ID", "GO_Term")

# Unir
Synap_complete <- left_join(synaptogenesis_con_top, go_Synap_c, by = "Ensembl")

write.csv(Synap_complete, "Tabla con TODA la info de Synap.csv")
