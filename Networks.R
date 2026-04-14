#### Cargar librerias ####
pacman::p_load("readr",
               "tidyverse",
               "DT",
               "dplyr",
               "vroom",
               "ggplot2",
               "ggrepel",
               "scales",
               "ComplexUpset")

Tox_functions_AD <- read_delim("/Users/alejandropintacastro/Downloads/INMEGEN/Alejandr@s-EHL/observacion1-AD/diseases and functions tox functions.txt",
                         delim = "\t",
                         escape_double = FALSE,
                         trim_ws = TRUE,
                         skip = 1)

Upstream_regulators_AD <- read_delim("/Users/alejandropintacastro/Downloads/INMEGEN/Alejandr@s-EHL/observacion1-AD/diseases and functions upstream regulators.txt",
                            delim = "\t",
                            escape_double = FALSE,
                            trim_ws = TRUE,
                            skip = 1)
Diseaseas_functions_AD <- read_delim("/Users/alejandropintacastro/Downloads/INMEGEN/Alejandr@s-EHL/observacion1-AD/diseases and functions.txt",
                            delim = "\t",
                            escape_double = FALSE,
                            trim_ws = TRUE,
                            skip = 1)

### Networks AD #####
Network_1_AD <- read_delim("/Users/alejandropintacastro/Downloads/INMEGEN/IPA/Alejandr@s-EHL/observacion1-AD/Network 1 AD.txt",
                           delim = "\t",
                           escape_double = FALSE,
                           trim_ws = TRUE,
                           skip = 1)
Network_2_AD <- read_delim("/Users/alejandropintacastro/Downloads/INMEGEN/IPA/Alejandr@s-EHL/observacion1-AD/Network 2 AD.txt",
                           delim = "\t",
                           escape_double = FALSE,
                           trim_ws = TRUE,
                           skip = 1)
Network_3_AD <- read_delim("/Users/alejandropintacastro/Downloads/INMEGEN/IPA/Alejandr@s-EHL/observacion1-AD/Network 3 AD.txt",
                           delim = "\t",
                           escape_double = FALSE,
                           trim_ws = TRUE,
                           skip = 1)
Network_4_AD <- read_delim("/Users/alejandropintacastro/Downloads/INMEGEN/IPA/Alejandr@s-EHL/observacion1-AD/Network 4 AD.txt",
                           delim = "\t",
                           escape_double = FALSE,
                           trim_ws = TRUE,
                           skip = 1)

Network_5_AD <- read_delim("/Users/alejandropintacastro/Downloads/INMEGEN/IPA/Alejandr@s-EHL/observacion1-AD/Network 5 AD.txt",
                           delim = "\t",
                           escape_double = FALSE,
                           trim_ws = TRUE,
                           skip = 1)

####Networks control ####

Network_1_control <- read_delim("/Users/alejandropintacastro/Downloads/INMEGEN/IPA/Alejandr@s-EHL/observacion2/Network 1 control.txt",
                           delim = "\t",
                           escape_double = FALSE,
                           trim_ws = TRUE,
                           skip = 1)
Network_2_control <- read_delim("/Users/alejandropintacastro/Downloads/INMEGEN/IPA/Alejandr@s-EHL/observacion2/Network 2 control.txt",
                           delim = "\t",
                           escape_double = FALSE,
                           trim_ws = TRUE,
                           skip = 1)
Network_3_control <- read_delim("/Users/alejandropintacastro/Downloads/INMEGEN/IPA/Alejandr@s-EHL/observacion2/Network 3 control.txt",
                           delim = "\t",
                           escape_double = FALSE,
                           trim_ws = TRUE,
                           skip = 1)
Network_4_control <- read_delim("/Users/alejandropintacastro/Downloads/INMEGEN/IPA/Alejandr@s-EHL/observacion2/Network 4 control.txt",
                           delim = "\t",
                           escape_double = FALSE,
                           trim_ws = TRUE,
                           skip = 1)

Network_5_control <- read_delim("/Users/alejandropintacastro/Downloads/INMEGEN/IPA/Alejandr@s-EHL/observacion2/Network 5 control.txt",
                           delim = "\t",
                           escape_double = FALSE,
                           trim_ws = TRUE,
                           skip = 1)

#### Unión de network 1 ####
Networks_1_combined <- bind_rows(
  Network_1_AD %>% mutate(grupo = "ALzheimer"),
  Network_1_control %>% mutate(grupo = "Control")
)

ad <- Network_1_AD %>%
  mutate(row_id = row_number(), grupo = "Alzheimer")

ctrl <- Network_1_control %>%
  mutate(row_id = row_number(), grupo = "Control")

# Combinar e intercalar
Networks_1_combined <- bind_rows(ad, ctrl) %>%
  arrange(row_id, desc(grupo)) %>%   
  select(-row_id)                    

#### Unión de network 2 ####
Networks_2_combined <- bind_rows(
  Network_2_AD %>% mutate(grupo = "ALzheimer"),
  Network_2_control %>% mutate(grupo = "Control")
)

ad <- Network_2_AD %>%
  mutate(row_id = row_number(), grupo = "Alzheimer")

ctrl <- Network_2_control %>%
  mutate(row_id = row_number(), grupo = "Control")

# Combinar e intercalar
Networks_3_combined <- bind_rows(ad, ctrl) %>%
  arrange(row_id, desc(grupo)) %>%   
  select(-row_id)                    

#### Unión de network 3 ####
Networks_3_combined <- bind_rows(
  Network_3_AD %>% mutate(grupo = "ALzheimer"),
  Network_3_control %>% mutate(grupo = "Control")
)

ad <- Network_3_AD %>%
  mutate(row_id = row_number(), grupo = "Alzheimer")

ctrl <- Network_3_control %>%
  mutate(row_id = row_number(), grupo = "Control")

# Combinar e intercalar
Networks_3_combined <- bind_rows(ad, ctrl) %>%
  arrange(row_id, desc(grupo)) %>%   
  select(-row_id)                    

#### Unión de network 4 ####
Networks_4_combined <- bind_rows(
  Network_4_AD %>% mutate(grupo = "ALzheimer"),
  Network_4_control %>% mutate(grupo = "Control")
)

ad <- Network_4_AD %>%
  mutate(row_id = row_number(), grupo = "Alzheimer")

ctrl <- Network_4_control %>%
  mutate(row_id = row_number(), grupo = "Control")

# Combinar e intercalar
Networks_4_combined <- bind_rows(ad, ctrl) %>%
  arrange(row_id, desc(grupo)) %>%   
  select(-row_id)                    




#### Unión de network 5 ####
Networks_5_combined <- bind_rows(
  Network_5_AD %>% mutate(grupo = "ALzheimer"),
  Network_5_control %>% mutate(grupo = "Control")
)

ad <- Network_5_AD %>%
  mutate(row_id = row_number(), grupo = "Alzheimer")

ctrl <- Network_5_control %>%
  mutate(row_id = row_number(), grupo = "Control")

# Combinar e intercalar
Networks_5_combined <- bind_rows(ad, ctrl) %>%
  arrange(row_id, desc(grupo)) %>%   
  select(-row_id)

####################################################################   Tabla de Pau ####################################################################  



datos_rosmap <- vroom("//Users/alejandropintacastro/Downloads/INMEGEN/IPA/Alejandr@s-EHL/ROSMAP_DLFPC_DE_full_gene_list_dichoNIAReagan.txt",
  delim = "\t",          
  na = c("", "NA", "N/A", "null"),  
  guess_max = 10000      
)

####################################################################   Verificando correlación de genes de pathways con networks ################################################################  
#### GABAergic combined vs network 1####

gaba_con_top <- gaba_con_top %>%
  mutate(
    en_Network_1 = Symbol %in% Networks_1_combined$Symbol
  )
#### Todos los demas ####
gaba_con_top <- gaba_con_top %>%
  mutate(
    en_Network_2        = Symbol %in% Networks_2_combined$Symbol,
    en_Network_3         = Symbol %in% Networks_3_combined$Symbol,
    en_Network_4_AD      = Symbol %in% Network_4_AD$Symbol,
    en_Network_4_control = Symbol %in% Network_4_control$Symbol,
    en_Network_5         = Symbol %in% Networks_5_combined$Symbol
  )

####  GABA receptor vs networks ####
gaba_receptor_con_top <- gaba_receptor_con_top %>%
  mutate(
    en_Network_1 = Symbol %in% Networks_1_combined$Symbol,
    en_Network_2 = Symbol %in% Networks_2_combined$Symbol,
    en_Network_3 = Symbol %in% Networks_3_combined$Symbol,
    en_Network_4_AD = Symbol %in% Network_4_AD$Symbol,
    en_Network_4_control = Symbol %in% Network_4_control$Symbol,
    en_network_5_ = Symbol %in% Networks_5_combined$Symbol
  )

#### Glutaminergic vs nnetworks ####
glutaminergic_con_top <- glutaminergic_con_top %>%
  mutate(
    en_Network_1 = Symbol %in% Networks_1_combined$Symbol,
    en_Network_2 = Symbol %in% Networks_2_combined$Symbol,
    en_Network_3 = Symbol %in% Networks_3_combined$Symbol,
    en_Network_4_AD = Symbol %in% Network_4_AD$Symbol,
    en_Network_4_control = Symbol %in% Network_4_control$Symbol,
    en_network_5_ = Symbol %in% Networks_5_combined$Symbol
  )

#### Glutaminergic vs nnetworks ####
glutaminergic_con_top <- glutaminergic_con_top %>%
  mutate(
    en_Network_1 = Symbol %in% Networks_1_combined$Symbol,
    en_Network_2 = Symbol %in% Networks_2_combined$Symbol,
    en_Network_3 = Symbol %in% Networks_3_combined$Symbol,
    en_Network_4_AD = Symbol %in% Network_4_AD$Symbol,
    en_Network_4_control = Symbol %in% Network_4_control$Symbol,
    en_network_5_ = Symbol %in% Networks_5_combined$Symbol
  )

#### Neurexins and Neurolgins vs networks ####
neurexins_con_top <- neurexins_con_top %>%
  mutate(
    en_Network_1 = Symbol %in% Networks_1_combined$Symbol,
    en_Network_2 = Symbol %in% Networks_2_combined$Symbol,
    en_Network_3 = Symbol %in% Networks_3_combined$Symbol,
    en_Network_4_AD = Symbol %in% Network_4_AD$Symbol,
    en_Network_4_control = Symbol %in% Network_4_control$Symbol,
    en_network_5_ = Symbol %in% Networks_5_combined$Symbol
  )
    
#### SNARE vs networks ####
snare_con_top <- snare_con_top %>%
  mutate(
    en_Network_1 = Symbol %in% Networks_1_combined$Symbol,
    en_Network_2 = Symbol %in% Networks_2_combined$Symbol,
    en_Network_3 = Symbol %in% Networks_3_combined$Symbol,
    en_Network_4_AD = Symbol %in% Network_4_AD$Symbol,
    en_Network_4_control = Symbol %in% Network_4_control$Symbol,
    en_network_5_ = Symbol %in% Networks_5_combined$Symbol
  )

#### Synaptogenesis vs networks ####
synaptogenesis_con_top <- synaptogenesis_con_top %>%
  mutate(
    en_Network_1 = Symbol %in% Networks_1_combined$Symbol,
    en_Network_2 = Symbol %in% Networks_2_combined$Symbol,
    en_Network_3 = Symbol %in% Networks_3_combined$Symbol,
    en_Network_4_AD = Symbol %in% Network_4_AD$Symbol,
    en_Network_4_control = Symbol %in% Network_4_control$Symbol,
    en_network_5_ = Symbol %in% Networks_5_combined$Symbol
  )

################################################################  Ver que mis genes en pathways esten en la de ROSMAP de Pau ################################################################  
#### GABAergic vs ROSMAP ####
gaba_con_top <- gaba_con_top %>%
  mutate(
    ROSMAP = Ensembl %in% datos_rosmap$ensembl_gene_id
  )

#### GABA receptor vs ROSMAP ####
gaba_receptor_con_top <- gaba_receptor_con_top %>%
  mutate(
    ROSMAP = Ensembl %in% datos_rosmap$ensembl_gene_id
  )

#### Glutaminergic vs ROSMAP ####
glutaminergic_con_top <- glutaminergic_con_top %>%
  mutate(
    ROSMAP = Ensembl %in% datos_rosmap$ensembl_gene_id
  )

#### N&N vs ROSMAP ####
neurexins_con_top <- neurexins_con_top %>%
  mutate(
    ROSMAP = Ensembl %in% datos_rosmap$ensembl_gene_id
  )

#### SNARE vs ROSMAP ####
snare_con_top <- snare_con_top %>%
  mutate(
    ROSMAP = Ensembl %in% datos_rosmap$ensembl_gene_id
  )

#### Synaptogenesis vs ROSMAP ####
synaptogenesis_con_top <- synaptogenesis_con_top %>%
  mutate(
    ROSMAP = Ensembl %in% datos_rosmap$ensembl_gene_id
  )

################################################################  Agregar datos de ROSMAP a tablas de pathways ################################################################  
#### GABAergic con datos ROSMAP ####
gaba_con_top <- gaba_con_top %>%
  left_join(
    datos_rosmap %>%
      select(
        ensembl_gene_id,
        baseMean,
        log2FoldChange,
        lfcSE,         
        stat,
        pvalue,
        padj,
        diffexpressed
      ),
    by = c("Ensembl" = "ensembl_gene_id")
  )

#### GABA receptor con datos ROSMAP ####
gaba_receptor_con_top <- gaba_receptor_con_top %>%
  left_join(
    datos_rosmap %>%
      select(
        ensembl_gene_id,
        baseMean,
        log2FoldChange,
        lfcSE,         
        stat,
        pvalue,
        padj,
        diffexpressed
      ),
    by = c("Ensembl" = "ensembl_gene_id")
)

#### Glutaminergic con datos ROSMAP ####
glutaminergic_con_top <- glutaminergic_con_top %>%
  left_join(
    datos_rosmap %>%
      select(
        ensembl_gene_id,
        baseMean,
        log2FoldChange,
        lfcSE,         
        stat,
        pvalue,
        padj,
        diffexpressed
      ),
    by = c("Ensembl" = "ensembl_gene_id")
  ) 

#### N&N con datos ROSMAP ####
neurexins_con_top <- neurexins_con_top %>%
  left_join(
    datos_rosmap %>%
      select(
        ensembl_gene_id,
        baseMean,
        log2FoldChange,
        lfcSE,         
        stat,
        pvalue,
        padj,
        diffexpressed
      ),
    by = c("Ensembl" = "ensembl_gene_id")
  ) 

#### SNARE con datos ROSMAP ####
snare_con_top <- snare_con_top %>%
  left_join(
    datos_rosmap %>%
      select(
        ensembl_gene_id,
        baseMean,
        log2FoldChange,
        lfcSE,         
        stat,
        pvalue,
        padj,
        diffexpressed
      ),
    by = c("Ensembl" = "ensembl_gene_id")
  ) 

#### Synaptogenesis con datos ROSMAP ####
synaptogenesis_con_top <- synaptogenesis_con_top %>%
  left_join(
    datos_rosmap %>%
      select(
        ensembl_gene_id,
        baseMean,
        log2FoldChange,
        lfcSE,         
        stat,
        pvalue,
        padj,
        diffexpressed
      ),
    by = c("Ensembl" = "ensembl_gene_id")
  ) 

################################################################  Gráfica de resultados ################################################################  
#### Scatterplot GABAergic ####
GABAergic_plot <- gaba_con_top %>%
  filter(!is.na(baseMean), !is.na(`Expr Intensity/RPKM/FPKM/Counts...6`)) %>%
  mutate(
    Grado = as.numeric(`Expr Intensity/RPKM/FPKM/Counts...6`),
    Comunidad = as.factor(as.character(`Expr Intensity/RPKM/FPKM/Counts...7`))
  )
p_gaba <- ggplot(GABAergic_plot, 
                 aes(x = Grado, y = baseMean,
                     color = Comunidad, label = Symbol)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_text_repel(
    size = 3, 
    max.overlaps = 300,
    segment.color = "gray70",
    show.legend = FALSE
  ) + 
  scale_x_log10(name = "Grado (log10)") +
  scale_y_continuous(name = "baseMean (ROSMAP)") + 
  scale_color_viridis_d(option = "plasma", name = "Comunidad") + 
  labs(
    title = "Expresión basal vs Grado en la red - Vía GABAérgica",
    x = "Grado",
    y = "baseMean (ROSMAP)"
  ) + 
  facet_wrap(~ grupo, ncol = 2) +  
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.spacing = unit(2.5, "cm"),  
    legend.position = "right",
    strip.text = element_text(size = 12, face = "bold")  
  )


p_gaba

ggsave("/Users/alejandropintacastro/Downloads/INMEGEN/IPA/Alejandr@s-EHL/plots/GABA_scatter_base_Mean.png",  #### para guardarlo
       plot = p_gaba,
       width = 12,
       height = 7,
       dpi = 300)
  
#### Scatterplot GABA receptor ####

GABA_receptor_plot <- gaba_ambos_grupos %>%
  filter(!is.na(baseMean.x), !is.na(`Expr Intensity/RPKM/FPKM/Counts...6`)) %>%
  mutate(
    Grado = as.numeric(`Expr Intensity/RPKM/FPKM/Counts...6`),
    Comunidad = as.factor(as.character(`Expr Intensity/RPKM/FPKM/Counts...7`)),
    grupo = as.factor(grupo)
  )

p_GABArec <- ggplot(GABA_receptor_plot, 
                    aes(x = Grado, y = baseMean.x,
                        color = Comunidad, label = Symbol)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_text_repel(
    size = 2.5,
    max.overlaps = 100,
    segment.color = "gray70",
    show.legend = FALSE
  ) + 
  scale_x_log10(
    name = "Grado (log10)",
    breaks = c(1, 10, 100, 1000)
    ) +
  scale_y_continuous(name = "baseMean (ROSMAP)") + 
  scale_color_viridis_d(option = "plasma", name = "Comunidad") + 
  labs(title = "Expresión basal vs Grado en la red - Vía GABA receptor") + 
  facet_wrap(~ grupo, ncol = 2) +
  theme_minimal() + 
  theme(
    legend.position = "right", 
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.border = element_blank(),
    strip.text = element_text(face = "bold")
  )

p_GABArec

### Eliminar ciertas columnas####
gaba_receptor_con_top <- gaba_receptor_con_top %>%
  select(-c(
    baseMean.x, log2FoldChange.x, lfcSE.y, stat.y, pvalue.y, padj.y, diffexpressed.y,
    baseMean.y, log2FoldChange.y, lfcSE.x, stat.x, pvalue.x, padj.x, diffexpressed.x
  ))

ggsave("/Users/alejandropintacastro/Downloads/INMEGEN/IPA/Alejandr@s-EHL/plots/GABAreceptor_scatter_base_Mean.png",  #### para guardarlo
       plot = p_GABArec,
       width = 12,
       height = 7,
       dpi = 300)

#### Scatterplot glutaminergica ####
Glutaminergic_plot <- glutaminergic_con_top %>%
  filter(!is.na(baseMean), !is.na(`Expr Intensity/RPKM/FPKM/Counts...6`)) %>%
  mutate(
    Grado = as.numeric(`Expr Intensity/RPKM/FPKM/Counts...6`),
    Comunidad = as.factor(as.character(`Expr Intensity/RPKM/FPKM/Counts...7`))
  )


p_glut <- ggplot(Glutaminergic_plot, 
                 aes(x = Grado, y = baseMean,
                     color = Comunidad, label = Symbol)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_text_repel(
    size = 2.5,
    max.overlaps = 300,
    segment.color = "gray70",
    show.legend = FALSE
  ) + 
  scale_x_log10(name = "Grado (log10)") +
  scale_y_continuous(name = "baseMean (ROSMAP)") + 
  scale_color_viridis_d(option = "plasma", name = "Comunidad") + 
  labs(
    title = "Expresión basal vs Grado en la red - Vía Glutaminérgica",
    x = "Grado",
    y = "baseMean (ROSMAP)"
  ) + 
  theme_minimal() + 
  theme(
    legend.position = "right", 
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill ="white", color = NA),
    panel.border = element_blank()
  )

p_glut

ggsave("/Users/alejandropintacastro/Downloads/INMEGEN/IPA/Alejandr@s-EHL/plots/Glutaminergic_scatter_base_Mean.png",  #### para guardarlo
       plot = p_glut,
       width = 12,
       height = 7,
       dpi = 300)

#### Scatterplot N&N ####
Neurexins_plot <- neurexins_con_top %>%
  filter(!is.na(baseMean), !is.na(`Expr Intensity/RPKM/FPKM/Counts...6`)) %>%
  mutate(
    Grado = as.numeric(`Expr Intensity/RPKM/FPKM/Counts...6`),
    Comunidad = as.factor(as.character(`Expr Intensity/RPKM/FPKM/Counts...7`))
  )

g_nn <- ggplot(Neurexins_plot,
               aes(x = Grado, y = baseMean,
                   color = Comunidad, label = Symbol)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_text_repel(
    size = 3, 
    max.overlaps = 300,
    segment.color = "gray70",
    show.legend = FALSE
  ) + 
  scale_x_log10(
    name = "Grado (log10)",
    breaks = c(1, 10, 100, 1000)
  ) + 
  scale_y_continuous(name = "baseMean (ROSMAP)") + 
  scale_color_viridis_d(option = "plasma", name = "Comunidad") + 
  labs(
    title = "Expresión basal vs Grado en la red - Vía Neurexins and neuroligins",
    x = "Grado",
    y = "baseMean (ROSMAP)"
  ) + 
  facet_wrap(~ grupo, ncol = 2) +  
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.spacing = unit(2.5, "cm"),  
    legend.position = "right",
    strip.text = element_text(size = 12, face = "bold")  
  )

g_nn### para mostrar

ggsave("/Users/alejandropintacastro/Downloads/INMEGEN/IPA/Alejandr@s-EHL/plots/Neurexins_scatter_base_Mean.png",  #### para guardarlo
       plot = g_nn,
       width = 12,
       height = 7,
       dpi = 300)

#### Scatterplot SNARE ####
SNARE_plot <- snare_con_top %>%
  filter(!is.na(baseMean), !is.na(`Expr Intensity/RPKM/FPKM/Counts...6`)) %>%
  mutate(
    Grado = as.numeric(`Expr Intensity/RPKM/FPKM/Counts...6`),
    Comunidad = as.factor(as.character(`Expr Intensity/RPKM/FPKM/Counts...7`))
  )

g_snare <- ggplot(SNARE_plot, 
                  aes(x = Grado, y = baseMean,
                      color = Comunidad, label = Symbol)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_text_repel(
    size = 3, 
    max.overlaps = 300,
    segment.color = "gray70",
    show.legend = FALSE
  ) + 
  scale_x_log10(name = "Grado (log10)") +
  scale_y_continuous(name = "baseMean (ROSMAP)") + 
  scale_color_viridis_d(option = "plasma", name = "Comunidad") + 
  labs(
    title = "Expresión basal vs Grado en la red - Vía SNARE",
    x = "Grado",
    y = "baseMean (ROSMAP)"
  ) + 
  facet_wrap(~ grupo, ncol = 2) +  
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.spacing = unit(2.5, "cm"),  
    legend.position = "right",
    strip.text = element_text(size = 12, face = "bold")  
  )

g_snare ### para mostrar

ggsave("/Users/alejandropintacastro/Downloads/INMEGEN/IPA/Alejandr@s-EHL/plots/SNARE_scatter_base_Mean.png",  #### para guardarlo
       plot = g_snare,
       width = 12,
       height = 7,
       dpi = 300)

#### Scatterplot synaptogenesis ####
Synaptogenesis_plot <- synaptogenesis_con_top %>%
  filter(!is.na(baseMean), !is.na(`Expr Intensity/RPKM/FPKM/Counts...6`)) %>%
  mutate(
    Grado = as.numeric(`Expr Intensity/RPKM/FPKM/Counts...6`),
    Comunidad = as.factor(as.character(`Expr Intensity/RPKM/FPKM/Counts...7`))
  )

g_synap <- ggplot(Synaptogenesis_plot, 
                  aes(x = Grado, y = baseMean,
                      color = Comunidad, label = Symbol)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_text_repel(
    size = 3, 
    max.overlaps = 300,
    segment.color = "gray70",
    show.legend = FALSE
  ) + 
  scale_x_log10(name = "Grado (log10)") +
  scale_y_continuous(name = "baseMean (ROSMAP)") + 
  scale_color_viridis_d(option = "plasma", name = "Comunidad") + 
  labs(
    title = "Expresión basal vs Grado en la red - Vía Synaptogenesis",
    x = "Grado",
    y = "baseMean (ROSMAP)"
  ) + 
  facet_wrap(~ grupo, ncol = 2) +  
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.spacing = unit(2.5, "cm"),  
    legend.position = "right",
    strip.text = element_text(size = 12, face = "bold")  
  )

g_synap ### para mostrar

ggsave("/Users/alejandropintacastro/Downloads/INMEGEN/IPA/Alejandr@s-EHL/plots/Synaptogenesis_scatter_base_Mean.png",  #### para guardarlo
       plot = g_synap,
       width = 12,
       height = 7,
       dpi = 300)

############################## Guardar tablas ##################################################
write_csv(gaba_con_top, "GABAergic.csv")
write_csv(gaba_receptor_con_top, "GABAreceptor.csv")
write_csv(glutaminergic_con_top, "Glutaminergic.csv")
write_csv(neurexins_con_top, "Neurexins.csv")
write_csv(snare_con_top, "SNARE.csv")
write_csv(synaptogenesis_con_top, "Synaptogenesis.csv")
################################################################  Cleaning my environment ####################################################################  
rm(Network_1_AD, Network_1_control, Network_2_AD,  Network_2_control, Network_3_AD, Network_3_control, Network_5_AD, Network_5_control, conteo_tipos, conteo_tipos_AD, datos_AD_con_drugs, datos_control_con_drugs, Synaptogenesis_pathway_AD, Synaptogenesis_pathway_control, SNARE_pathways_AD, SNARE_pathways_control, Neurexins_and_neuroligins_pathway_control, Neurexins_and_neuroligins_pathways_AD, GABA_receptor_pathway_control, GABAergic_pathways_AD)
rm(tipos_unicos, tipos_unicos_AD, tabla_tipos_con_genes, tabla_tipos_con_genes_AD, tabla_syn_interactiva,tabla_resumen, tabla_resumen_AD, tabla_filtrada_control, tabla_filtrada_AD, tabla_comparativa, tabla_combinada, datos_sin_other, datos_sin_other_AD)

df$
  
  
  ######################################################################################################## AHORA VAMOS A GRAFICAR CON AV EXPR ###########################################################
#### Gráfica de GABAergic ######

# Preparar datos para la vía GABAérgica
GABAergic_plot_expr <- gaba_con_top %>%
  filter(
    !is.na(`Expr Intensity/RPKM/FPKM/Counts...4`),  # Expresión
    !is.na(`Expr Intensity/RPKM/FPKM/Counts...6`),  # Grado
    !is.na(grupo)                                   # Asegurar que 'grupo' exista
  ) %>%
  mutate(
    Grado = as.numeric(`Expr Intensity/RPKM/FPKM/Counts...6`),
    Expr = `Expr Intensity/RPKM/FPKM/Counts...4`,
    Comunidad = as.factor(as.character(`Expr Intensity/RPKM/FPKM/Counts...7`))
  ) %>%
  # Asegurar valores positivos para log10
  filter(Grado > 0, Expr > 0) %>%
  # Asegurar orden en el factor 'grupo'
  mutate(grupo = factor(grupo, levels = c("Control", "Alzheimer")))

# Calcular umbrales globales (mismos en ambos paneles)
umbral_grado <- median(GABAergic_plot_expr$Grado, na.rm = TRUE)
umbral_expr  <- median(GABAergic_plot_expr$Expr, na.rm = TRUE)

# Gráfico final con cuadrícula y cuadrantes
p_gaba_expr <- ggplot(GABAergic_plot_expr, 
                      aes(x = Grado, y = Expr,
                          color = Comunidad, label = Symbol)) +
  # Líneas divisorias para los 4 cuadrantes
  geom_vline(xintercept = umbral_grado, linetype = "dashed", color = "darkgray", linewidth = 0.8) +
  geom_hline(yintercept = umbral_expr, linetype = "dashed", color = "darkgray", linewidth = 0.8) +
  
  geom_point(size = 3, alpha = 0.8) +
  geom_text_repel(
    size = 3, 
    max.overlaps = 300,
    segment.color = "gray70",
    show.legend = FALSE
  ) + 
  scale_x_log10(name = "Grado (log10)") +
  scale_y_continuous(name = "Average Expression") + 
  scale_color_viridis_d(option = "plasma", name = "Comunidad") + 
  labs(
    title = "Expresión promedio vs Grado en la red - Vía GABAérgica",
    x = "Grado (log10)",
    y = "Average Expression"
  ) + 
  facet_wrap(~ grupo, ncol = 2) +  
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor = element_line(color = "gray95", linewidth = 0.25),
    panel.spacing = unit(2.5, "cm"),  
    legend.position = "right",
    strip.text = element_text(size = 12, face = "bold")  
  )

p_gaba_expr

ggsave("/Users/alejandropintacastro/Downloads/INMEGEN/IPA/Alejandr@s-EHL/plots mod/GABAergic_expr_scatter_base_Mean.png",  #### para guardarlo
       plot = p_gaba_expr,
       width = 12,
       height = 7,
       dpi = 300)

#### GABA receptor ####

# Añadir expr_Alzheimer a gaba_receptor_con_top
gaba_receptor_con_top <- gaba_receptor_con_top %>%
  left_join(
    datos %>% 
      select(Symbol, expr_Alzheimer = `Expr Intensity/RPKM/FPKM/Counts...1`),
    by = "Symbol"
  )
      
gaba_receptor_con_top <- gaba_receptor_con_top %>%
  left_join(
    datos %>% 
      select(
        Symbol,
        expr_Alzheimer = `Expr Intensity/RPKM/FPKM/Counts...1`,
        grado_Alzheimer = `Expr Intensity/RPKM/FPKM/Counts...3`  # asumiendo que ...3 = grado
      ),
    by = "Symbol"
  )

# Preparar datos: usar la tabla de GABA receptor
GABA_receptor_plot_expr <- gaba_receptor_con_top %>%
  filter(!is.na(expr_Alzheimer.x), !is.na(grado_Alzheimer)) %>%
  mutate(
    Grado = as.numeric(grado_Alzheimer),
    Comunidad = as.factor(as.character(`Expr Intensity/RPKM/FPKM/Counts...7`))
  )

# Gráfico
p_gaba_receptor_expr <- ggplot(GABA_receptor_plot_expr, 
                               aes(x = Grado, y = expr_Alzheimer.x,
                                   color = Comunidad, label = Symbol)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_text_repel(
    size = 3, 
    max.overlaps = 300,
    segment.color = "gray70",
    show.legend = FALSE
  ) + 
  scale_x_log10(name = "Grado (log10) - Alzheimer") +
  scale_y_continuous(name = "Average Expression - Alzheimer") + 
  scale_color_viridis_d(option = "plasma", name = "Comunidad") + 
  labs(
    title = "Expresión promedio vs Grado en la red - Vía GABA receptor (Alzheimer)",
    x = "Grado (log10)",
    y = "Average Expression (Alzheimer)"
  ) + 
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.spacing = unit(2.5, "cm"),  
    legend.position = "right",
    strip.text = element_text(size = 12, face = "bold")  
  )

p_gaba_receptor_expr



# Preparar datos en formato largo para ambas condiciones
gaba_receptor_largo <- gaba_receptor_con_top %>%
  select(
    Symbol,
# Añadir expr_Alzheimer a gaba_receptor_con_top
gaba_receptor_con_top <- gaba_receptor_con_top %>%
  left_join(
    datos %>% 
      select(Symbol, expr_Alzheimer = `Expr Intensity/RPKM/FPKM/Counts...1`),
    by = "Symbol"
  )
      
gaba_receptor_con_top <- gaba_receptor_con_top %>%
  left_join(
    datos %>% 
      select(
        Symbol,
        expr_Alzheimer = `Expr Intensity/RPKM/FPKM/Counts...1`,
        grado_Alzheimer = `Expr Intensity/RPKM/FPKM/Counts...3`  # asumiendo que ...3 = grado
      ),
    by = "Symbol"
  )

# Preparar datos: usar la tabla de GABA receptor
GABA_receptor_plot_expr <- gaba_receptor_con_top %>%
  filter(!is.na(expr_Alzheimer.x), !is.na(grado_Alzheimer)) %>%
  mutate(
    Grado = as.numeric(grado_Alzheimer),
    Comunidad = as.factor(as.character(`Expr Intensity/RPKM/FPKM/Counts...7`))
  )

# Gráfico
p_gaba_receptor_expr <- ggplot(GABA_receptor_plot_expr, 
                               aes(x = Grado, y = expr_Alzheimer.x,
                                   color = Comunidad, label = Symbol)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_text_repel(
    size = 3, 
    max.overlaps = 300,
    segment.color = "gray70",
    show.legend = FALSE
  ) + 
  scale_x_log10(name = "Grado (log10) - Alzheimer") +
  scale_y_continuous(name = "Average Expression - Alzheimer") + 
  scale_color_viridis_d(option = "plasma", name = "Comunidad") + 
  labs(
    title = "Expresión promedio vs Grado en la red - Vía GABA receptor (Alzheimer)",
    x = "Grado (log10)",
    y = "Average Expression (Alzheimer)"
  ) + 
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.spacing = unit(2.5, "cm"),  
    legend.position = "right",
    strip.text = element_text(size = 12, face = "bold")  
  )

p_gaba_receptor_expr



# Preparar datos en formato largo para ambas condiciones
gaba_receptor_largo <- gaba_receptor_con_top %>%
  select(
   # Añadir expr_Alzheimer a gaba_receptor_con_top
gaba_receptor_con_top <- gaba_receptor_con_top %>%
  left_join(
    datos %>% 
      select(Symbol, expr_Alzheimer = `Expr Intensity/RPKM/FPKM/Counts...1`),
    by = "Symbol"
  )
      
gaba_receptor_con_top <- gaba_receptor_con_top %>%
  left_join(
    datos %>% 
      select(
        Symbol,
        expr_Alzheimer = `Expr Intensity/RPKM/FPKM/Counts...1`,
        grado_Alzheimer = `Expr Intensity/RPKM/FPKM/Counts...3`  # asumiendo que ...3 = grado
      ),
    by = "Symbol"
  )

# Preparar datos: usar la tabla de GABA receptor
GABA_receptor_plot_expr <- gaba_receptor_con_top %>%
  filter(!is.na(expr_Alzheimer.x), !is.na(grado_Alzheimer)) %>%
  mutate(
    Grado = as.numeric(grado_Alzheimer),
    Comunidad = as.factor(as.character(`Expr Intensity/RPKM/FPKM/Counts...7`))
  )

# Gráfico
p_gaba_receptor_expr <- ggplot(GABA_receptor_plot_expr, 
                               aes(x = Grado, y = expr_Alzheimer.x,
                                   color = Comunidad, label = Symbol)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_text_repel(
    size = 3, 
    max.overlaps = 300,
    segment.color = "gray70",
    show.legend = FALSE
  ) + 
  scale_x_log10(name = "Grado (log10) - Alzheimer") +
  scale_y_continuous(name = "Average Expression - Alzheimer") + 
  scale_color_viridis_d(option = "plasma", name = "Comunidad") + 
  labs(
    title = "Expresión promedio vs Grado en la red - Vía GABA receptor (Alzheimer)",
    x = "Grado (log10)",
    y = "Average Expression (Alzheimer)"
  ) + 
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.spacing = unit(2.5, "cm"),  
    legend.position = "right",
    strip.text = element_text(size = 12, face = "bold")  
  )

p_gaba_receptor_expr

# Filtrar datos antes de graficar
# Filtrar datos antes de graficar
gaba_receptor_largo_clean <- gaba_receptor_largo %>%
  filter(
    !is.na(grado), 
    !is.na(expr),
    grado > 0,        # necesario para log10
    expr > 0          # opcional, si aplica
  )

# Luego calculas los umbrales con los datos limpios
umbral_grado <- median(gaba_receptor_largo_clean$grado, na.rm = TRUE)
umbral_expr  <- median(gaba_receptor_largo_clean$expr, na.rm = TRUE)

# Y usas gaba_receptor_largo_clean en ggplot en lugar de gaba_receptor_largo

p_gaba_receptor_final <- ggplot(gaba_receptor_largo_clean, 
                                aes(x = grado, y = expr,
                                    color = Comunidad, label = Symbol)) +
  geom_vline(xintercept = umbral_grado, linetype = "dashed", color = "darkgray", linewidth = 0.8) +
  geom_hline(yintercept = umbral_expr, linetype = "dashed", color = "darkgray", linewidth = 0.8) +
  geom_point(size = 3, alpha = 0.8) +
  geom_text_repel(
    size = 3,
    max.overlaps = 100,
    segment.color = "gray70",
    show.legend = FALSE
  ) +
  scale_x_log10(name = "Grado (log10)") +
  scale_y_continuous(name = "Average Expression") +
  scale_color_viridis_d(option = "plasma", name = "Comunidad") +
  labs(
    title = "Expresión promedio vs Grado en la red - Vía GABA receptor",
    x = "Grado (log10)",
    y = "Average Expression"
  ) +
  facet_wrap(~ grupo, ncol = 2) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor = element_line(color = "gray95", linewidth = 0.25),
    panel.spacing = unit(2.5, "cm"),
    legend.position = "right",
    strip.text = element_text(size = 12, face = "bold")
  )

p_gaba_receptor_final

ggsave("/Users/alejandropintacastro/Downloads/INMEGEN/IPA/Alejandr@s-EHL/plots mod/GABA_receptor_expr_scatter_base_Mean.png",  #### para guardarlo
       plot = p_gaba_receptor_final,
       width = 12,
       height = 7,
       dpi = 300)

##############Glutaminergic con av expr #########
glutaminergic_con_top <- glutaminergic_con_top %>%
  left_join(
    datos_control %>% 
      select(Symbol, expr_control = `expr`),
    by = "Symbol"
  )



# Añadir expresión y grado de Control
glutaminergic_con_top <- glutaminergic_con_top %>%
  left_join(
    datos_control %>%
      select(
        Symbol,
        expr_Control = `expr`,
        grado_Control = `Expr Intensity/RPKM/FPKM/Counts...3`,
        Comunidad_Control = `comm`
      ),
    by = "Symbol"
  )


# Convertir a formato largo
glut_largo <- glutaminergic_con_top %>%
  select(
    Symbol,
    expr_Alzheimer = `Expr Intensity/RPKM/FPKM/Counts...4`,
    grado_Alzheimer = `Expr Intensity/RPKM/FPKM/Counts...6`,
    Comunidad = `Expr Intensity/RPKM/FPKM/Counts...7`,
    expr_Control,
    grado_Control
  ) %>%
  pivot_longer(
    cols = c(expr_Alzheimer, expr_Control, grado_Alzheimer, grado_Control),
    names_to = c(".value", "grupo"),
    names_sep = "_",
    values_drop_na = TRUE
  ) %>%
  mutate(
    grupo = factor(grupo, levels = c("Alzheimer", "Control")),
    Comunidad = as.factor(as.character(Comunidad))
  )

# Ver conteo
count(glut_largo, grupo)

# Calcular medianas para usar como umbrales
umbral_grado <- median(glut_largo$grado, na.rm = TRUE)
umbral_expr  <- median(glut_largo$expr, na.rm = TRUE)

# Gráfico con cuadrícula
p_glut_final <- ggplot(glut_largo, 
                       aes(x = grado, y = expr,
                           color = Comunidad, label = Symbol)) +
  # Líneas divisorias para cuadrantes
  geom_vline(xintercept = umbral_grado, linetype = "dashed", color = "darkgray", linewidth = 0.8) +
  geom_hline(yintercept = umbral_expr, linetype = "dashed", color = "darkgray", linewidth = 0.8) +
  
  geom_point(size = 3, alpha = 0.8) +
  geom_text_repel(
    size = 3,
    max.overlaps = 100,
    segment.color = "gray70",
    show.legend = FALSE
  ) +
  scale_x_log10(name = "Grado (log10)") +
  scale_y_continuous(name = "Average Expression") +
  scale_color_viridis_d(option = "plasma", name = "Comunidad") +
  labs(
    title = "Expresión promedio vs Grado en la red - Vía Glutamatérgica",
    x = "Grado (log10)",
    y = "Average Expression"
  ) +
  facet_wrap(~ grupo, ncol = 2) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor = element_line(color = "gray95", linewidth = 0.25),
    panel.spacing = unit(2.5, "cm"),
    legend.position = "right",
    strip.text = element_text(size = 12, face = "bold")
  )

p_glut_final

ggsave("/Users/alejandropintacastro/Downloads/INMEGEN/IPA/Alejandr@s-EHL/plots mod/Glutaminergic_receptor_expr_scatter_base_Mean.png",  #### para guardarlo
       plot = p_glut_final,
       width = 12,
       height = 7,
       dpi = 300)

#### Neurexins and neuroligins ####

# Preparar datos: incluir la columna grupo existente
Neurexins_plot_expr <- neurexins_con_top %>%
  filter(
    !is.na(`Expr Intensity/RPKM/FPKM/Counts...4`),  # Control
    !is.na(`Expr Intensity/RPKM/FPKM/Counts...6`),  # Grado Control
    !is.na(grupo)  # Asegurar que grupo no sea NA
  ) %>%
  mutate(
    Grado = as.numeric(`Expr Intensity/RPKM/FPKM/Counts...6`),
    Expr = `Expr Intensity/RPKM/FPKM/Counts...4`,
    Comunidad = as.factor(as.character(`Expr Intensity/RPKM/FPKM/Counts...7`))
  ) %>%
  # Asegurar que todos los valores sean válidos para log10
  filter(Grado > 0, Expr > 0) %>%
  # Asegurar que grupo sea factor con niveles ordenados
  mutate(grupo = factor(grupo, levels = c("Control", "Alzheimer")))

# Calcular umbrales globales (mismos para ambos paneles)
umbral_grado <- median(Neurexins_plot_expr$Grado, na.rm = TRUE)
umbral_expr  <- median(Neurexins_plot_expr$Expr, na.rm = TRUE)

# Gráfico con comparativa real y cuadrícula
p_neurexins_expr <- ggplot(Neurexins_plot_expr, 
                           aes(x = Grado, y = Expr,
                               color = Comunidad, label = Symbol)) +
  # Líneas divisorias para los 4 cuadrantes (mismas en ambos paneles)
  geom_vline(xintercept = umbral_grado, linetype = "dashed", color = "darkgray", linewidth = 0.8) +
  geom_hline(yintercept = umbral_expr, linetype = "dashed", color = "darkgray", linewidth = 0.8) +
  
  geom_point(size = 3, alpha = 0.8) +
  geom_text_repel(
    size = 3, 
    max.overlaps = 300,
    segment.color = "gray70",
    show.legend = FALSE
  ) + 
  scale_x_log10(name = "Grado (log10)") +
  scale_y_continuous(name = "Average Expression") + 
  scale_color_viridis_d(option = "plasma", name = "Comunidad") + 
  labs(
    title = "Expresión promedio vs Grado en la red - Vía Neurexins",
    x = "Grado (log10)",
    y = "Average Expression"
  ) + 
  facet_wrap(~ grupo, ncol = 2) +  
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor = element_line(color = "gray95", linewidth = 0.25),
    panel.spacing = unit(2.5, "cm"),  
    legend.position = "right",
    strip.text = element_text(size = 12, face = "bold")  
  )

p_neurexins_expr

ggsave("/Users/alejandropintacastro/Downloads/INMEGEN/IPA/Alejandr@s-EHL/plots mod/Neurexins_expr_scatter_base_Mean.png",  #### para guardarlo
       plot = p_neurexins_expr,
       width = 12,
       height = 7,
       dpi = 300)

#### SNARE ########


# Preparar datos para SNARE
SNARE_plot_expr <- snare_con_top %>%
  filter(
    !is.na(`Expr Intensity/RPKM/FPKM/Counts...4`),  # Expr (Control o promedio)
    !is.na(`Expr Intensity/RPKM/FPKM/Counts...6`),  # Grado
    !is.na(grupo)                                   # Asegurar que grupo exista
  ) %>%
  mutate(
    Grado = as.numeric(`Expr Intensity/RPKM/FPKM/Counts...6`),
    Expr = `Expr Intensity/RPKM/FPKM/Counts...4`,
    Comunidad = as.factor(as.character(`Expr Intensity/RPKM/FPKM/Counts...7`))
  ) %>%
  # Asegurar valores válidos para log10 y evitar NAs en la gráfica
  filter(Grado > 0, Expr > 0) %>%
  # Asegurar que grupo sea factor con niveles ordenados
  mutate(grupo = factor(grupo, levels = c("Control", "Alzheimer")))

# Calcular umbrales globales (mismos para ambos paneles)
umbral_grado <- median(SNARE_plot_expr$Grado, na.rm = TRUE)
umbral_expr  <- median(SNARE_plot_expr$Expr, na.rm = TRUE)

# Gráfico con comparativa real y cuadrícula
p_snare_expr <- ggplot(SNARE_plot_expr, 
                       aes(x = Grado, y = Expr,
                           color = Comunidad, label = Symbol)) +
  # Líneas divisorias para los 4 cuadrantes
  geom_vline(xintercept = umbral_grado, linetype = "dashed", color = "darkgray", linewidth = 0.8) +
  geom_hline(yintercept = umbral_expr, linetype = "dashed", color = "darkgray", linewidth = 0.8) +
  
  geom_point(size = 3, alpha = 0.8) +
  geom_text_repel(
    size = 3, 
    max.overlaps = 300,
    segment.color = "gray70",
    show.legend = FALSE
  ) + 
  scale_x_log10(name = "Grado (log10)") +
  scale_y_continuous(name = "Average Expression") + 
  scale_color_viridis_d(option = "plasma", name = "Comunidad") + 
  labs(
    title = "Expresión promedio vs Grado en la red - Vía SNARE",
    x = "Grado (log10)",
    y = "Average Expression"
  ) + 
  facet_wrap(~ grupo, ncol = 2) +  
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor = element_line(color = "gray95", linewidth = 0.25),
    panel.spacing = unit(2.5, "cm"),  
    legend.position = "right",
    strip.text = element_text(size = 12, face = "bold")  
  )

p_snare_expr

ggsave("/Users/alejandropintacastro/Downloads/INMEGEN/IPA/Alejandr@s-EHL/plots mod/SNARE_expr_scatter_base_Mean.png",  #### para guardarlo
       plot = p_snare_expr,
       width = 12,
       height = 7,
       dpi = 300)

#######Synaptogenesis ###########


# Preparar datos para Synaptogenesis
synaptogensis_plot_expr <- synaptogenesis_con_top %>%
  filter(
    !is.na(`Expr Intensity/RPKM/FPKM/Counts...4`),  # Expresión
    !is.na(`Expr Intensity/RPKM/FPKM/Counts...6`),  # Grado
    !is.na(grupo)                                   # Asegurar que 'grupo' exista
  ) %>%
  mutate(
    Grado = as.numeric(`Expr Intensity/RPKM/FPKM/Counts...6`),
    Expr = `Expr Intensity/RPKM/FPKM/Counts...4`,
    Comunidad = as.factor(as.character(`Expr Intensity/RPKM/FPKM/Counts...7`))
  ) %>%
  # Asegurar valores válidos para log10 (evitar 0 o negativos)
  filter(Grado > 0, Expr > 0) %>%
  # Asegurar que 'grupo' sea factor con orden deseado
  mutate(grupo = factor(grupo, levels = c("Control", "Alzheimer")))

# Calcular umbrales globales (mismos para ambos paneles)
umbral_grado <- median(synaptogensis_plot_expr$Grado, na.rm = TRUE)
umbral_expr  <- median(synaptogensis_plot_expr$Expr, na.rm = TRUE)

# Gráfico con cuadrantes y comparativa
p_synaptogensis_expr <- ggplot(synaptogensis_plot_expr, 
                               aes(x = Grado, y = Expr,
                                   color = Comunidad, label = Symbol)) +
  # Líneas divisorias para los 4 cuadrantes
  geom_vline(xintercept = umbral_grado, linetype = "dashed", color = "darkgray", linewidth = 0.8) +
  geom_hline(yintercept = umbral_expr, linetype = "dashed", color = "darkgray", linewidth = 0.8) +
  
  geom_point(size = 3, alpha = 0.8) +
  geom_text_repel(
    size = 3, 
    max.overlaps = 300,
    segment.color = "gray70",
    show.legend = FALSE
  ) + 
  scale_x_log10(name = "Grado (log10)") +
  scale_y_continuous(name = "Average Expression") + 
  scale_color_viridis_d(option = "plasma", name = "Comunidad") + 
  labs(
    title = "Expresión promedio vs Grado en la red - Vía Synaptogenesis",
    x = "Grado (log10)",
    y = "Average Expression"
  ) + 
  facet_wrap(~ grupo, ncol = 2) +  
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor = element_line(color = "gray95", linewidth = 0.25),
    panel.spacing = unit(2.5, "cm"),  
    legend.position = "right",
    strip.text = element_text(size = 12, face = "bold")  
  )

p_synaptogensis_expr

ggsave("/Users/alejandropintacastro/Downloads/INMEGEN/IPA/Alejandr@s-EHL/plots mod/Synaptogenesis_expr_scatter_base_Mean.png",  #### para guardarlo
       plot = p_synaptogensis_expr,
       width = 12,
       height = 7,
       dpi = 300)

####################################################################################

# Verificar que genes del pathway GABAergic se encuentran en mis redes ####
gaba_en_redes <- gaba_con_top %>%
  filter(
    en_Network_1 == TRUE |
      en_Network_2 == TRUE |
      en_Network_3 == TRUE |
      en_Network_4_AD == TRUE |
      en_Network_4_control == TRUE |
      en_Network_5 == TRUE
  ) %>%
  select(
    Symbol,  # o las columnas que quieras ver
    en_Network_1,
    en_Network_2,
    en_Network_3,
    en_Network_4_AD,
    en_Network_4_control,
    en_Network_5,
    everything()  # el resto de columnas
  )

# Ver resultados
print(gaba_en_redes)


#### Verificar que genes de GABA receptor pathway se encuentra en mis redes ####

gaba_receptor_en_redes <- gaba_receptor_con_top %>%
  filter(
    en_Network_1 == TRUE |
      en_Network_2 == TRUE |
      en_Network_3 == TRUE |
      en_Network_4_AD == TRUE |
      en_Network_4_control == TRUE |
      en_network_5_ == TRUE
  ) %>%
  select(
    Symbol,  # o las columnas que quieras ver
    en_Network_1,
    en_Network_2,
    en_Network_3,
    en_Network_4_AD,
    en_Network_4_control,
    en_network_5_,
    everything()  # el resto de columnas
  )

# Ver resultados
print(gaba_receptor_en_redes)

#### Verificar que genes de Glutaminergic pathway se encuentra en mis redes ####

glutaminergic_en_redes <- glutaminergic_con_top %>%
  filter(
    en_Network_1 == TRUE |
      en_Network_2 == TRUE |
      en_Network_3 == TRUE |
      en_Network_4_AD == TRUE |
      en_Network_4_control == TRUE |
      en_network_5_ == TRUE
  ) %>%
  select(
    Symbol,  # o las columnas que quieras ver
    en_Network_1,
    en_Network_2,
    en_Network_3,
    en_Network_4_AD,
    en_Network_4_control,
    en_network_5_,
    everything()  # el resto de columnas
  )

# Ver resultados
print(glutaminergic_en_redes)

#### Verificar que genes de Neurexins and neuroligins  pathway se encuentra en mis redes ####

neurexins_en_redes <- neurexins_con_top %>%
  filter(
    en_Network_1 == TRUE |
      en_Network_2 == TRUE |
      en_Network_3 == TRUE |
      en_Network_4_AD == TRUE |
      en_Network_4_control == TRUE |
      en_network_5_ == TRUE
  ) %>%
  select(
    Symbol,  # o las columnas que quieras ver
    en_Network_1,
    en_Network_2,
    en_Network_3,
    en_Network_4_AD,
    en_Network_4_control,
    en_network_5_,
    everything()  # el resto de columnas
  )

# Ver resultados
print(neurexins_en_redes)

#### Verificar que genes de SNARE pathway se encuentra en mis redes ####

snare_en_redes <- snare_con_top %>%
  filter(
    en_Network_1 == TRUE |
      en_Network_2 == TRUE |
      en_Network_3 == TRUE |
      en_Network_4_AD == TRUE |
      en_Network_4_control == TRUE |
      en_network_5_ == TRUE
  ) %>%
  select(
    Symbol,  # o las columnas que quieras ver
    en_Network_1,
    en_Network_2,
    en_Network_3,
    en_Network_4_AD,
    en_Network_4_control,
    en_network_5_,
    everything()  # el resto de columnas
  )

# Ver resultados
print(snare_en_redes)

#### Verificar que genes de synaptogenesis pathway se encuentra en mis redes ####

synaptogenesis_en_redes <- synaptogenesis_con_top %>%
  filter(
    en_Network_1 == TRUE |
      en_Network_2 == TRUE |
      en_Network_3 == TRUE |
      en_Network_4_AD == TRUE |
      en_Network_4_control == TRUE |
      en_network_5_ == TRUE
  ) %>%
  select(
    Symbol,  # o las columnas que quieras ver
    en_Network_1,
    en_Network_2,
    en_Network_3,
    en_Network_4_AD,
    en_Network_4_control,
    en_network_5_,
    everything()  # el resto de columnas
  )

# Ver resultados
print(synaptogenesis_en_redes)


write_csv(
  Networks_1_combined, 
  "/Users/alejandropintacastro/Downloads/INMEGEN/Network 1 combined.csv")
write_csv(
  GABAergic_plot,
  "/Users/alejandropintacastro/Downloads/INMEGEN/Gabaergic plot.csv")
)


