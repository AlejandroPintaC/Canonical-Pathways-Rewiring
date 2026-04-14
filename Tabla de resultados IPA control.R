install.packages("DT")

library(readr)
library(dplyr)
library(DT)
library(tidyverse)



datos_control <- read_delim("/Users/alejandropintacastro/Downloads/INMEGEN/tabla que especifica todo lo de los genes control.csv", 
                    delim = "\t", 
                    escape_double = FALSE, 
                    trim_ws = TRUE,
                    skip = 1 
                    
                    )


head(datos_control)




#### Columna "Communities"####
datos_control %>% 
  group_by(`Expr Intensity/RPKM/FPKM/Counts...4`) %>% 
  tally(sort = T)

datos_control <- 
  datos_control %>% 
  rename(comm = `Expr Intensity/RPKM/FPKM/Counts...4`)

datos_control <- datos_control %>%
  rename(expr = `Expr Intensity/RPKM/FPKM/Counts...1`)

#### Comunidad 1 ####
datos_control %>% 
  filter(comm == 1) %>% 
  select(`Type(s)`, Symbol, everything()) 

datos_control %>% #### Para solo sacar la average expression ####
  filter(comm == 1, !is.na(expr)) %>%
  arrange(desc(expr)) %>%
  select(`Type(s)`, Symbol, expr, comm, everything()) %>%
  head(1)  


#### Comunidad 4 ####
datos_control %>% 
  filter(comm == 4) %>% 
  select(`Type(s)`, Symbol, everything())

#### Comunidad 3 ####
datos_control %>% 
  filter(comm == 3) %>% 
  select(`Type(s)`, Symbol, everything())

#### Comunidad 6 ####
datos_control %>% 
  filter(comm == 6) %>% 
  select(`Type(s)`, Symbol, everything())

#### Comunidad 2 ####
datos_control %>% 
  filter(comm == 2) %>% 
  select(`Type(s)`, Symbol, everything())

#### Comunidad 70 ####
datos_control %>% 
  filter(comm == 70) %>% 
  select(`Type(s)`, Symbol, everything())

#### Comunidad 24 #### 
datos_control %>% 
  filter(comm == 24) %>% 
  select(`Type(s)`, Symbol, everything())

#### Comunidad 30 ####
datos_control %>% 
  filter(comm == 30) %>% 
  select(`Type(s)`, Symbol, everything())

#### Comunidad 68 ####
datos_control %>% 
  filter(comm == 68) %>% 
  select(`Type(s)`, Symbol, everything())


datos_control %>% 
  filter(comm == 5) %>% 
  select(`Type(s)`, Symbol) %>% 
  group_by(`Type(s)`) %>% 
  tally(sort = T) %>% 
  mutate(total = sum(n)) %>% 
  mutate(pc = 100*n/total)



lapply(1:72, function(i){
  datos_control %>% 
    filter(comm == i) %>% 
    group_by(`Type(s)`) %>% 
    tally(sort = T) %>% 
    mutate(total = sum(n)) %>% 
    mutate(pc = 100*n/total)
  
}
)

#### Top communities ####
top_communities <- datos_control %>%
  count(comm, sort = TRUE) %>%
  head(10)

print (top_communities)

top10_comms <- datos_control %>%
  count(comm, sort = TRUE) %>%
  head(10) %>%
  pull(comm)

my_communities <- c(1, 2, 3, 4, 6, 24, 30, 68, 70)

#### Solo datos de la tabla control que tengan una comunidad ####
combined_selected <- datos_control %>%
  filter(comm %in% my_communities, !is.na(comm)) %>%  #### linea para descartar "NA" en la parte de comunidades
  select(`Type(s)`, Symbol, comm, everything()) %>%
  arrange(comm, Symbol)

combined_selected



# Obtener las top 10 comunidades por número de genes
top10_list <- datos_control %>%
  count(comm, sort = TRUE) %>%
  head(10) %>%
  pull(comm)

#### Para solo tener el gen con mayor expr de las comunidades ####
top_gene_per_comm <- datos_control %>%
  filter(comm %in% top10_list, !is.na(expr)) %>%
  group_by(comm) %>%
  slice_max(expr, n = 1) %>%   # 
  ungroup() %>%
  arrange(desc(expr)) %>%      
  select(`Type(s)`, Symbol, comm, expr, everything())

# Ver resultado
top_gene_per_comm

#### Tabla filtrada por medicamentos ####
combined_drugs <- combined_selected %>%
  filter(!is.na(`Drug(s)`)) %>%          ##### Aquí excluyes las filas con "NA" en la columna Drug(s) #### 
  select(`Type(s)`, Symbol, `Drug(s)`, everything()) %>%
  arrange(`Drug(s)`, Symbol) %>%
  head(30)

combined_drugs


#### Descartar other dentro de la columna type(s) ####
datos_sin_other <- datos_control %>%
  filter(!is.na(`Type(s)`)) %>%       #### En caso de que aparezca "NA", tambien quitarlo ####
  filter(!str_detect(`Type(s)`, regex("other", ignore_case = TRUE)))

##### Obteniendo los resultados, ya quitando los que dicen "other" `datos_sin_other` ####
tipos_unicos <- datos_sin_other %>%
  distinct(`Type(s)`) %>%
  arrange(`Type(s)`)

# Mostrar la lista
print(tipos_unicos)

#### Para que me diga cuantas veces se repiten, al menos en la tabla de quita "others" ####
conteo_tipos <- datos_sin_other %>%
  count(`Type(s)`, sort = TRUE)  

print(conteo_tipos)

#### Tabla que solo contenga entrez gene name, types y drugs ####

tabla_resumen <- datos_sin_other %>%
  select(`Entrez Gene Name`, `Type(s)`, Symbol, `Drug(s)`)

# Ver el resultado
print(tabla_resumen)


tabla_tipos_con_genes <- datos_control %>%
  # Eliminar filas donde 'Entrez Gene Name' sea NA
  filter(!is.na(`Entrez Gene Name`)) %>%
  group_by(`Type(s)`) %>%
  summarise(
    n = n(),
    entrez_genes = str_c(`Entrez Gene Name`, collapse = ", ")
  ) %>%
  arrange(desc(n))  # ordenar por frecuencia

# Ver resultado
print(tabla_tipos_con_genes)

tabla_filtrada_control <- datos_control %>%
  filter(!is.na(`Expr Intensity/RPKM/FPKM/Counts...3`)) %>%
  select(comm, expr, `Type(s)`, `Drug(s)`)

# Ver resultado
print(tabla_filtrada_control)

#### Tabla para filtrar la columna Drug(s) ####
datos_control_con_drugs <- datos_control %>%
  filter(!is.na (`Drug(s)`)) %>%
  select(Symbol, `Type(s)`, `Drug(s)`, expr, comm) 

#### Guardar documento ####
write_csv(combined_drugs, "genes_con_drugs_30.csv)