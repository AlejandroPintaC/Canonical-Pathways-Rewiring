#### Cargar librerias ####
pacman::p_load("readr",
               "tidyverse",
               "DT",
               "dplyr",
               "vroom")
 #### Lectura de archivos ####
Pathways_GABA_AD <- vroom("/Users/alejandropintacastro/Downloads/INMEGEN/GABA AD.csv")
                           

Pathways_Glutaminergic_AD <- read_delim("/Users/alejandropintacastro/Downloads/INMEGEN/Glutaminergic AD.csv",
                                        delim = "\t",
                                        escape_double = FALSE,
                                        trim_ws = TRUE,
                                        skip = 1)

Pathways_Neuroxins_and_neuroligins_AD <- read_delim("/Users/alejandropintacastro/Downloads/INMEGEN/Neuroxins and neuroligins AD.csv",
                                                    delim = "\t",
                                                    escape_double = FALSE,
                                                    trim_ws = TRUE,
                                                    skip = 1)

Pathways_SNARE_AD <- read_delim("/Users/alejandropintacastro/Downloads/INMEGEN/SNARE AD.csv",
                                delim = "\t",
                                escape_double = FALSE,
                                trim_ws = TRUE,
                                skip = 1)

Pathways_Synaptogenesis_AD <- read_delim("/Users/alejandropintacastro/Downloads/INMEGEN/Synaptogenesis AD.csv",
                                         delim = "\t",
                                         escape_double = FALSE,
                                         trim_ws = TRUE,
                                         skip = 1)

#### Tabla combinada de los 5 pathways ####
tabla_pathways_AD <- bind_rows (
  Pathways_GABA_AD                      %>% mutate(grupo = "GABA"),
  Pathways_Glutaminergic_AD             %>% mutate(grupo = "Glutaminergic"),
  Pathways_Neuroxins_and_neuroligins_AD %>% mutate(grupo = "Neuroxins and neuroligins"),
  Pathways_SNARE_AD                     %>% mutate(grupo = "SNARE"),
  Pathways_Synaptogenesis_AD            %>% mutate(grupo = "Synaptogenesis")
)

#### Guardar archivo ####
write_csv(tabla_pathways_AD, "Tabla_pathways_AD.csv")
                                  
