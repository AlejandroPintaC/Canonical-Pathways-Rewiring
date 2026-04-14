install.packages("DT")

library(readr)
library(dplyr)
library(DT)
library(tidyverse)

datos_AD <- read_delim("/Users/alejandropintacastro/Downloads/INMEGEN/Tabla que especifica todos los genes copy.csv", 
                    delim = "\t", 
                    escape_double = FALSE, 
                    trim_ws = TRUE,
                    skip = 1 
                    
                    )


head(datos_AD)



datos_AD %>% 
  group_by(`Expr Intensity/RPKM/FPKM/Counts...4`) %>% 
  tally(sort = T)

datos_AD <- 
  datos_AD %>% 
  rename(comm = `Expr Intensity/RPKM/FPKM/Counts...4`)

datos_AD <-
  datos_AD %>%
  rename(expr = `Expr Intensity/RPKM/FPKM/Counts...1`)

datos_AD %>% 
  filter(comm == 4) %>% 
  select(`Type(s)`, Symbol, everything())

datos_AD %>% 
  filter(comm == 5) %>% 
  select(`Type(s)`, Symbol) %>% 
  group_by(`Type(s)`) %>% 
  tally(sort = T) %>% 
  mutate(total = sum(n)) %>% 
  mutate(pc = 100*n/total)

print(datos, n = 209)

lapply(1:65, function(i){
  datos_AD %>% 
    filter(comm == i) %>% 
    group_by(`Type(s)`) %>% 
    tally(sort = T) %>% 
    mutate(total = sum(n)) %>% 
    mutate(pc = 100*n/total)
  
}
       )

top_communities_AD <- datos_AD %>%
  count(comm, sort = TRUE) %>%
  head(10)

print (top_communities_AD)

#### Solo datos de la tabla control que tengan una comunidad ####
combined_selected_AD <- datos_AD %>%
  filter(comm %in% my_communities, !is.na(comm)) %>%  #### linea para descartar "NA" en la parte de comunidades
  select(`Type(s)`, Symbol, comm, everything()) %>%
  arrange(comm, Symbol)

combined_selected

#### Para solo tener el gen con mayor expr de las comunidades ####
top_gene_per_comm_AD <- datos_AD %>%
  filter(comm %in% top10_list, !is.na(expr)) %>%
  group_by(comm) %>%
  slice_max(expr, n = 1) %>%   # 
  ungroup() %>%
  arrange(desc(expr)) %>%      
  select(`Type(s)`, Symbol, comm, expr, everything())

#### Tabla filtrada por medicamentos ####
combined_drugs_AD <- combined_selected_AD %>%
  filter(!is.na(`Drug(s)`)) %>%          ##### Aquí excluyes las filas con "NA" en la columna Drug(s) #### 
select(`Type(s)`, Symbol, `Drug(s)`, everything()) %>%
  arrange(`Drug(s)`, Symbol) %>%
  head(30)

combined_drugs_AD

#### Descartar other dentro de la columna type(s) ####
datos_sin_other_AD <- datos_AD %>%
  filter(!is.na(`Type(s)`)) %>%       #### En caso de que aparezca "NA", tambien quitarlo ####
filter(!str_detect(`Type(s)`, regex("other", ignore_case = TRUE)))

##### Obteniendo los resultados, ya quitando los que dicen "other" `datos_sin_other` ####
tipos_unicos_AD <- datos_sin_other_AD %>%
  distinct(`Type(s)`) %>%
  arrange(`Type(s)`)

#### Para que me diga cuantas veces se repiten, al menos en la tabla de quita "others" ####
conteo_tipos_AD <- datos_sin_other_AD %>%
  count(`Type(s)`, sort = TRUE) 

#### Tabla que solo contenga entrez gene name, types y drugs ####
tabla_resumen_AD <- datos_sin_other_AD %>%
  select(`Entrez Gene Name`, `Type(s)`, Symbol, `Drug(s)`)

# Ver el resultado
print(tabla_resumen_AD)


#### Para ver columna type(s) y que un otra columna ponga todos los entrez gene name pertenecientes a cada type(s) ####
tabla_tipos_con_genes_AD <- datos_AD %>%
  # eliminar filas donde 'Entrez Gene Name' sea NA
  filter(!is.na(`Entrez Gene Name`)) %>%
  group_by(`Type(s)`) %>%
  summarise(
    n = n(),
    entrez_genes = str_c(`Entrez Gene Name`, collapse = ", ")
  ) %>%
  arrange(desc(n))  # ordenar por frecuencia

# Ver resultado
print(tabla_tipos_con_genes_AD)


tabla_filtrada_AD <- datos_AD%>%
  filter(!is.na(`Expr Intensity/RPKM/FPKM/Counts...3`)) %>%
  select(comm, expr, `Type(s)`, `Drug(s)`)

# Ver resultado
print(tabla_filtrada_AD)

#### Unir tablas top gene per comm ###
tabla_combinada <- bind_rows(
  top_gene_per_comm      %>% mutate(grupo = "Control"),
  top_gene_per_comm_AD   %>% mutate(grupo = "Alzheimer")
) %>%
  arrange(comm, grupo)  # ordenar por comunidad y luego por grupo

# Ver resultado
print(tabla_combinada)


#### La misma tabla comparativa pero agregando los resultados de drug(s) ####
library(dplyr)
library(tidyr)

tabla_comparativa <- bind_rows(
  top_gene_per_comm      %>% mutate(grupo = "Control"),
  top_gene_per_comm_AD   %>% mutate(grupo = "Alzheimer")
) %>%
  #
  select(comm, Symbol, expr, `Drug(s)`, grupo) %>% ### Cambio para agregar columna "Drugs(s)" ####
  pivot_wider(
    names_from = grupo,
    values_from = c(Symbol, expr, `Drug(s)`),
    names_sep = "_"
  ) %>%
  arrange(comm)

# Ver resultado
print(tabla_comparativa)


#### Tabla para filtrar la columna Drug(s) ####
datos_AD_con_drugs <- datos_AD %>%
  filter(!is.na (`Drug(s)`)) %>%
           select(Symbol, `Type(s)`, `Drug(s)`, expr, comm) 
         