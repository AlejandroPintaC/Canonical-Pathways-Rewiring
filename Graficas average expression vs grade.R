install.packages("ggthemes")
library(ggthemes)
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

# define colors#
futurama_colors <- c(
  "#f22222",
  "#8a2be2",  
  "#8c8c8c",  
  "#ff69b4",  
  "#ffcc00",  
  "#ff66cc",  
  "#33cc33",
  "#22f2f2",  
  "#ff9900",  
  "#6633ff",  
  "#00cc99",  
  "#ff3366",  
  "#9966ff",  
  "#ff5500",  
  "#00aaff",  
  "#cc0066",  
  "#ffff33",  
  "#33ffff",  
  "#ff99cc",  
  "#66ff66",  
  "#ff4d00",  
  "#b366ff",  
  "#00ffcc",  
  "#ffccff",  
  "#99ff33"   
)
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
  scale_x_log10(name = "Grade (log10)") +
  scale_y_continuous(name = "Average Expression") + 
  scale_color_manual(values = futurama_colors, name = "Community") + 
  labs(
    title = "Average expression vs Grade - GABAergic pathway",
    x = "Grade (log10)",
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

### guaradar en tabla###


# Crear tabla con los datos del gráfico
GABAergic_table <- GABAergic_plot_expr %>%
  select(
    Symbol,                    # Gen
    Grado,                     # Grade (log10)
    Expr,                      # Average Expression
    Comunidad,                 # Community
    grupo,                     # Control/Alzheimer
    `Expr Intensity/RPKM/FPKM/Counts...7`  # Valor original de comunidad
  ) %>%
  arrange(grupo, desc(Expr))  # Ordenar por grupo y expresión

# Ver las primeras filas
head(GABAergic_table)

# Guardar como CSV
write.csv(GABAergic_table, 
          "/Users/alejandropintacastro/Downloads/INMEGEN/IPA/Alejandr@s-EHL/plots mod/GABAergic_expr_data.csv",
          row.names = FALSE)



#### GABA receptor ####
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
          scale_x_log10(name = "Grade (log10)") +
          scale_y_continuous(name = "Average Expression") +
          scale_color_manual(values = futurama_colors, name = "Community") +
          labs(
            title = "Average expression vs Grade - GABA receptor pathway",
            x = "Grade (log10)",
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
        
### Guardar tabla ###
        
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
          scale_x_log10(name = "Grade (log10)") +
          scale_y_continuous(name = "Average Expression") +
          scale_color_manual(values = futurama_colors, name = "Community") +
          labs(
            title = "Average expression vs Grade - Glutamatergic pathway",
            x = "Grade (log10)",
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
          scale_x_log10(name = "Grade (log10)") +
          scale_y_continuous(name = "Average Expression") + 
          scale_color_manual(values = futurama_colors, name = "Community") + 
          labs(
            title = "Average expression vs Grade - Neurexins and neuroligins pathway",
            x = "Grade (log10)",
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
          scale_x_log10(name = "Grade (log10)") +
          scale_y_continuous(name = "Average Expression") + 
          scale_color_manual(values = futurama_colors, name = "Community") +
          labs(
            title = "Average expression vs Grade - SNARE pathway",
            x = "Grade (log10)",
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
          scale_x_log10(name = "Grade (log10)") +
          scale_y_continuous(name = "Average Expression") + 
          scale_color_manual(values = futurama_colors, name = "Community") +
          labs(
            title = "Average expression vs Grade - Synaptogenesis pathway",
            x = "Grade (log10)",
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
        
        
############## Glut corregido ###############
# 1. Reemplazar NA por 0 (solo si es biológicamente válido)
        glut_largo_completo <- glut_largo %>%
          mutate(
            grado = ifelse(is.na(grado), 0, grado),
            expr  = ifelse(is.na(expr),  0, expr)
          )
        
# 2. Ver cuántos eran NA antes (opcional, para registro)
        num_na_grado <- sum(is.na(glut_largo$grado))
        num_na_expr  <- sum(is.na(glut_largo$expr))
        cat("Valores NA reemplazados:\n")
        cat("  grado:", num_na_grado, "\n")
        cat("  expr: ", num_na_expr, "\n")
        
# 3. Calcular medianas (usando los datos originales o los completados, según prefieras)
        #   Aquí uso los datos completados para coherencia visual
        umbral_grado <- median(glut_largo_completo$grado, na.rm = TRUE)
        umbral_expr  <- median(glut_largo_completo$expr,  na.rm = TRUE)
        
# 4. Gráfico en escala lineal (¡no logarítmica!)
        p_glut_final <- ggplot(glut_largo_completo, 
                               aes(x = grado, y = expr,
                                   color = Comunidad, label = Symbol)) +
          # Líneas divisorias
          geom_vline(xintercept = umbral_grado, linetype = "dashed", color = "darkgray", linewidth = 0.8) +
          geom_hline(yintercept = umbral_expr,  linetype = "dashed", color = "darkgray", linewidth = 0.8) +
          
          geom_point(size = 3, alpha = 0.8) +
          geom_text_repel(
            size = 3,
            max.overlaps = 100,
            segment.color = "gray70",
            show.legend = FALSE
          ) +
          
          # ESCALA LINEAL (clave para incluir 0)
          scale_x_continuous(name = "Grade") +
          scale_y_continuous(name = "Average Expression") +
          
          scale_color_manual(values = futurama_colors, name = "Community") +
          labs(
            title = "Average expression vs Grade - Glutamatergic pathway (all genes shown)",
            x = "Grade",
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
        