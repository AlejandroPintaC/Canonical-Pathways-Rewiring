gaba_wide <- GABAergic_plot_expr %>%
  select(Symbol, grupo, Grado, Expr) %>%
  pivot_wider(
    id_cols = Symbol,
    names_from = grupo,
    values_from = c(Grado, Expr),
    names_prefix = ""
  ) %>%
  rename(
    Grado_Control = Grado_Control,
    Grado_Alzheimer = Grado_Alzheimer,
    Expr_Control = Expr_Control,
    Expr_Alzheimer = Expr_Alzheimer
  )

# Paso 3: Asignar cuadrante en cada condición (usando los mismos umbrales globales)
gaba_wide <- gaba_wide %>%
  mutate(
    cuadrante_Control = case_when(
      Grado_Control > umbral_grado & Expr_Control > umbral_expr ~ "I",
      Grado_Control <= umbral_grado & Expr_Control > umbral_expr ~ "II",
      Grado_Control <= umbral_grado & Expr_Control <= umbral_expr ~ "III",
      Grado_Control > umbral_grado & Expr_Control <= umbral_expr ~ "IV"
    ),
    cuadrante_Alzheimer = case_when(
      Grado_Alzheimer > umbral_grado & Expr_Alzheimer > umbral_expr ~ "I",
      Grado_Alzheimer <= umbral_grado & Expr_Alzheimer > umbral_expr ~ "II",
      Grado_Alzheimer <= umbral_grado & Expr_Alzheimer <= umbral_expr ~ "III",
      Grado_Alzheimer > umbral_grado & Expr_Alzheimer <= umbral_expr ~ "IV"
    ),
    cambio_cuadrante = cuadrante_Control != cuadrante_Alzheimer
  )

# Paso 4: Extraer lista de genes que cambian (opcional, para verlos)
genes_cambio <- gaba_wide %>%
  filter(cambio_cuadrante) %>%
  pull(Symbol)

cat("Genes que cambian de cuadrante:\n")
print(genes_cambio)


##### Resaltar genes en los gràficos #### 
# Unir la información de cambio de cuadrante al data frame largo
GABAergic_plot_expr <- GABAergic_plot_expr %>%
  left_join(
    gaba_wide %>% select(Symbol, cambio_cuadrante),
    by = "Symbol"
  ) %>%
  mutate(cambio_cuadrante = replace_na(cambio_cuadrante, FALSE))  # en caso de que falte algún gen

p_gaba_expr_cambios <- ggplot(GABAergic_plot_expr, 
                      aes(x = Grado, y = Expr,
                          color = Comunidad, 
                          label = Symbol)) +
  geom_vline(xintercept = umbral_grado, linetype = "dashed", color = "darkgray", linewidth = 0.8) +
  geom_hline(yintercept = umbral_expr, linetype = "dashed", color = "darkgray", linewidth = 0.8) +
  
  # Puntos normales (gris claro para los que no cambian)
  geom_point(aes(color = Comunidad), size = 3, alpha = 0.6) +
  
  # ¡Resaltar los que cambian de cuadrante!
  geom_point(
    data = ~ filter(.x, cambio_cuadrante),
    color = "black",
    size = 4,
    shape = 1,      # círculo vacío
    stroke = 1.5,
    alpha = 1,
    show.legend = FALSE
  ) +
  
  # Etiquetas solo para los que cambian (opcional, o para todos)
  geom_text_repel(
    data = ~ filter(.x, cambio_cuadrante),
    aes(label = Symbol),
    size = 3.5,
    max.overlaps = 100,
    segment.color = "black",
    fontface = "bold",
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

p_gaba_expr_cambios

ggsave("/Users/alejandropintacastro/Downloads/INMEGEN/IPA/Alejandr@s-EHL/plots mod/GABAergic cambios cuadricula.png",  #### para guardarlo
       plot = p_gaba_expr_cambios,
       width = 12,
       height = 7,
       dpi = 300)

# Tabla con genes que cambian de cuadrante ##
# Extraer genes que cambian de cuadrante con todos sus valores
gaba_genes_cambio_detalles <- gaba_wide %>%
  filter(cambio_cuadrante) %>%
  select(
    Symbol,
    Grado_Control, Expr_Control,
    Grado_Alzheimer, Expr_Alzheimer,
    cuadrante_Control, cuadrante_Alzheimer
  )

print(gaba_genes_cambio_detalles)

# Opcional: guardar en CSV
write.csv(
  gaba_genes_cambio_detalles,
  "/Users/alejandropintacastro/Downloads/INMEGEN/IPA/Alejandr@s-EHL/plots mod/GABAergic_genes_cambio_cuadricula.csv",
  row.names = FALSE
)
################ GABA receptor ##############################
gaba_receptor_largo_clean <- gaba_receptor_largo %>%
  filter(
    !is.na(grado), 
    !is.na(expr),
    grado > 0,        # necesario para log10
    expr > 0          # opcional, si aplica
  )

# Calcular umbrales globales
umbral_grado <- median(gaba_receptor_largo_clean$grado, na.rm = TRUE)
umbral_expr  <- median(gaba_receptor_largo_clean$expr, na.rm = TRUE)


# Paso 1: Convertir a formato ancho (una fila por gen)
gaba_receptor_wide <- gaba_receptor_largo_clean %>%
  select(Symbol, grupo, grado, expr) %>%
  pivot_wider(
    id_cols = Symbol,
    names_from = grupo,
    values_from = c(grado, expr)
  )

# Paso 2: Asignar cuadrantes y detectar cambios
gaba_receptor_wide <- gaba_receptor_wide %>%
  mutate(
    cuadrante_Control = case_when(
      grado_Control > umbral_grado & expr_Control > umbral_expr ~ "I",
      grado_Control <= umbral_grado & expr_Control > umbral_expr ~ "II",
      grado_Control <= umbral_grado & expr_Control <= umbral_expr ~ "III",
      grado_Control > umbral_grado & expr_Control <= umbral_expr ~ "IV"
    ),
    cuadrante_Alzheimer = case_when(
      grado_Alzheimer > umbral_grado & expr_Alzheimer > umbral_expr ~ "I",
      grado_Alzheimer <= umbral_grado & expr_Alzheimer > umbral_expr ~ "II",
      grado_Alzheimer <= umbral_grado & expr_Alzheimer <= umbral_expr ~ "III",
      grado_Alzheimer > umbral_grado & expr_Alzheimer <= umbral_expr ~ "IV"
    ),
    cambio_cuadrante = cuadrante_Control != cuadrante_Alzheimer
  )

# Paso 3: Unir la información de "cambio" al data frame largo
gaba_receptor_largo_clean <- gaba_receptor_largo_clean %>%
  left_join(
    gaba_receptor_wide %>% select(Symbol, cambio_cuadrante),
    by = "Symbol"
  ) %>%
  mutate(cambio_cuadrante = replace_na(cambio_cuadrante, FALSE))

p_gaba_receptor_cambios <- ggplot(gaba_receptor_largo_clean, 
                                aes(x = grado, y = expr,
                                    color = Comunidad, label = Symbol)) +
  geom_vline(xintercept = umbral_grado, linetype = "dashed", color = "darkgray", linewidth = 0.8) +
  geom_hline(yintercept = umbral_expr, linetype = "dashed", color = "darkgray", linewidth = 0.8) +
  
  # Puntos normales (más tenues)
  geom_point(aes(color = Comunidad), size = 3, alpha = 0.6) +
  
  # Resaltar genes que cambian de cuadrante
  geom_point(
    data = ~ filter(.x, cambio_cuadrante),
    color = "black",
    size = 4,
    shape = 1,      # círculo vacío
    stroke = 1.5,
    alpha = 1,
    show.legend = FALSE
  ) +
  
  # Etiquetas solo para los que cambian
  geom_text_repel(
    data = ~ filter(.x, cambio_cuadrante),
    aes(label = Symbol),
    size = 3.5,
    max.overlaps = 100,
    segment.color = "black",
    fontface = "bold",
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

p_gaba_receptor_cambios

# Guardar
ggsave("/Users/alejandropintacastro/Downloads/INMEGEN/IPA/Alejandr@s-EHL/plots mod/GABA_receptor_cambios_cuadricula.png",
       plot = p_gaba_receptor_cambios,
       width = 12,
       height = 7,
       dpi = 300)

############# Glutaminergic ##################
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

# Paso 1: Convertir a formato ancho (una fila por gen)
glut_wide <- glut_largo %>%
  select(Symbol, grupo, grado, expr) %>%
  pivot_wider(
    id_cols = Symbol,
    names_from = grupo,
    values_from = c(grado, expr)
  )

# Paso 2: Asignar cuadrantes y detectar cambios
glut_wide <- glut_wide %>%
  mutate(
    cuadrante_Alzheimer = case_when(
      grado_Alzheimer > umbral_grado & expr_Alzheimer > umbral_expr ~ "I",
      grado_Alzheimer <= umbral_grado & expr_Alzheimer > umbral_expr ~ "II",
      grado_Alzheimer <= umbral_grado & expr_Alzheimer <= umbral_expr ~ "III",
      grado_Alzheimer > umbral_grado & expr_Alzheimer <= umbral_expr ~ "IV"
    ),
    cuadrante_Control = case_when(
      grado_Control > umbral_grado & expr_Control > umbral_expr ~ "I",
      grado_Control <= umbral_grado & expr_Control > umbral_expr ~ "II",
      grado_Control <= umbral_grado & expr_Control <= umbral_expr ~ "III",
      grado_Control > umbral_grado & expr_Control <= umbral_expr ~ "IV"
    ),
    cambio_cuadrante = cuadrante_Alzheimer != cuadrante_Control
  )

# Paso 3: Unir la información de "cambio" al data frame largo
glut_largo <- glut_largo %>%
  left_join(
    glut_wide %>% select(Symbol, cambio_cuadrante),
    by = "Symbol"
  ) %>%
  mutate(cambio_cuadrante = replace_na(cambio_cuadrante, FALSE))

p_glut_cambios <- ggplot(glut_largo, 
                       aes(x = grado, y = expr,
                           color = Comunidad, label = Symbol)) +
  # Líneas divisorias para cuadrantes
  geom_vline(xintercept = umbral_grado, linetype = "dashed", color = "darkgray", linewidth = 0.8) +
  geom_hline(yintercept = umbral_expr, linetype = "dashed", color = "darkgray", linewidth = 0.8) +
  
  # Puntos normales (más tenues)
  geom_point(aes(color = Comunidad), size = 3, alpha = 0.6) +
  
  # Resaltar genes que cambian de cuadrante
  geom_point(
    data = ~ filter(.x, cambio_cuadrante),
    color = "black",
    size = 4,
    shape = 1,      # círculo vacío
    stroke = 1.5,
    alpha = 1,
    show.legend = FALSE
  ) +
  
  # Etiquetas solo para los que cambian
  geom_text_repel(
    data = ~ filter(.x, cambio_cuadrante),
    aes(label = Symbol),
    size = 3.5,
    max.overlaps = 100,
    segment.color = "black",
    fontface = "bold",
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

p_glut_cambios

# Guardar
ggsave("/Users/alejandropintacastro/Downloads/INMEGEN/IPA/Alejandr@s-EHL/plots mod/Glutaminergic_cambios_cuadricula.png",
       plot = p_glut_cambios,
       width = 12,
       height = 7,
       dpi = 300)

# Verificar genes que cambian de cuadrante #
# Extraer genes que cambian de cuadrante con sus valores numéricos
glut_genes_cambio_detalles <- glut_wide %>%
  filter(cambio_cuadrante) %>%
  select(
    Symbol,
    grado_Control, expr_Control,
    grado_Alzheimer, expr_Alzheimer,
    cuadrante_Control, cuadrante_Alzheimer
  )


print(glut_genes_cambio_detalles)

write.csv(
  glut_genes_cambio_detalles,
  "/Users/alejandropintacastro/Downloads/INMEGEN/IPA/Alejandr@s-EHL/plots mod/Glutaminergic_genes_cambio_cuadricula.csv",
  row.names = FALSE
)
####################### Neurexins and Neuroligins ################
Neurexins_plot_expr <- neurexins_con_top %>%
  filter(
    !is.na(`Expr Intensity/RPKM/FPKM/Counts...4`),  # Expr
    !is.na(`Expr Intensity/RPKM/FPKM/Counts...6`),  # Grado
    !is.na(grupo)
  ) %>%
  mutate(
    Grado = as.numeric(`Expr Intensity/RPKM/FPKM/Counts...6`),
    Expr = `Expr Intensity/RPKM/FPKM/Counts...4`,
    Comunidad = as.factor(as.character(`Expr Intensity/RPKM/FPKM/Counts...7`))
  ) %>%
  filter(Grado > 0, Expr > 0) %>%
  mutate(grupo = factor(grupo, levels = c("Control", "Alzheimer")))

# Calcular umbrales globales
umbral_grado <- median(Neurexins_plot_expr$Grado, na.rm = TRUE)
umbral_expr  <- median(Neurexins_plot_expr$Expr, na.rm = TRUE)

# Paso 1: Convertir a formato ancho
neurexins_wide <- Neurexins_plot_expr %>%
  select(Symbol, grupo, Grado, Expr) %>%
  pivot_wider(
    id_cols = Symbol,
    names_from = grupo,
    values_from = c(Grado, Expr)
  )

# Paso 2: Asignar cuadrantes y detectar cambios
neurexins_wide <- neurexins_wide %>%
  mutate(
    cuadrante_Control = case_when(
      Grado_Control > umbral_grado & Expr_Control > umbral_expr ~ "I",
      Grado_Control <= umbral_grado & Expr_Control > umbral_expr ~ "II",
      Grado_Control <= umbral_grado & Expr_Control <= umbral_expr ~ "III",
      Grado_Control > umbral_grado & Expr_Control <= umbral_expr ~ "IV"
    ),
    cuadrante_Alzheimer = case_when(
      Grado_Alzheimer > umbral_grado & Expr_Alzheimer > umbral_expr ~ "I",
      Grado_Alzheimer <= umbral_grado & Expr_Alzheimer > umbral_expr ~ "II",
      Grado_Alzheimer <= umbral_grado & Expr_Alzheimer <= umbral_expr ~ "III",
      Grado_Alzheimer > umbral_grado & Expr_Alzheimer <= umbral_expr ~ "IV"
    ),
    cambio_cuadrante = cuadrante_Control != cuadrante_Alzheimer
  )

# Paso 3: Unir información al data frame largo
Neurexins_plot_expr <- Neurexins_plot_expr %>%
  left_join(
    neurexins_wide %>% select(Symbol, cambio_cuadrante),
    by = "Symbol"
  ) %>%
  mutate(cambio_cuadrante = replace_na(cambio_cuadrante, FALSE))

p_neurexins_expr_cambios <- ggplot(Neurexins_plot_expr, 
                           aes(x = Grado, y = Expr,
                               color = Comunidad, label = Symbol)) +
  geom_vline(xintercept = umbral_grado, linetype = "dashed", color = "darkgray", linewidth = 0.8) +
  geom_hline(yintercept = umbral_expr, linetype = "dashed", color = "darkgray", linewidth = 0.8) +
  
  # Puntos normales (más tenues)
  geom_point(aes(color = Comunidad), size = 3, alpha = 0.6) +
  
  # Resaltar genes que cambian de cuadrante
  geom_point(
    data = ~ filter(.x, cambio_cuadrante),
    color = "black",
    size = 4,
    shape = 1,      # círculo vacío
    stroke = 1.5,
    alpha = 1,
    show.legend = FALSE
  ) +
  
  # Etiquetas solo para los que cambian
  geom_text_repel(
    data = ~ filter(.x, cambio_cuadrante),
    aes(label = Symbol),
    size = 3.5,
    max.overlaps = 100,
    segment.color = "black",
    fontface = "bold",
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

p_neurexins_expr_cambios

# Guardar
ggsave("/Users/alejandropintacastro/Downloads/INMEGEN/IPA/Alejandr@s-EHL/plots mod/Neurexins_cambios_cuadricula.png",
       plot = p_neurexins_expr_cambios,
       width = 12,
       height = 7,
       dpi = 300)

write.csv(
  Neurexins_plot_expr,
  file = "/Users/alejandropintacastro/Downloads/INMEGEN/Indices J/Neurexins J.csv",
  row.names = FALSE
)
# Verificar genes que cambian de cuadricula #
# Extraer genes que cambian de cuadrante con sus valores numéricos
neurexins_genes_detalles <- neurexins_wide %>%
  filter(cambio_cuadrante) %>%
  select(
    Symbol,
    Grado_Control, Expr_Control,
    Grado_Alzheimer, Expr_Alzheimer,
    cuadrante_Control, cuadrante_Alzheimer
  )

print(neurexins_genes_detalles)

# Opcional: guardar en CSV
write.csv(
  neurexins_genes_detalles,
  "/Users/alejandropintacastro/Downloads/INMEGEN/IPA/Alejandr@s-EHL/plots mod/Neurexins_genes_cambio_cuadricula.csv",
  row.names = FALSE
)

###### SNARE ###########
SNARE_plot_expr <- snare_con_top %>%
  filter(
    !is.na(`Expr Intensity/RPKM/FPKM/Counts...4`),  # Expr
    !is.na(`Expr Intensity/RPKM/FPKM/Counts...6`),  # Grado
    !is.na(grupo)
  ) %>%
  mutate(
    Grado = as.numeric(`Expr Intensity/RPKM/FPKM/Counts...6`),
    Expr = `Expr Intensity/RPKM/FPKM/Counts...4`,
    Comunidad = as.factor(as.character(`Expr Intensity/RPKM/FPKM/Counts...7`))
  ) %>%
  filter(Grado > 0, Expr > 0) %>%
  mutate(grupo = factor(grupo, levels = c("Control", "Alzheimer")))

# Calcular umbrales globales
umbral_grado <- median(SNARE_plot_expr$Grado, na.rm = TRUE)
umbral_expr  <- median(SNARE_plot_expr$Expr, na.rm = TRUE)

# Convertir a ancho y asignar cuadrantes
snare_wide <- SNARE_plot_expr %>%
  select(Symbol, grupo, Grado, Expr) %>%
  pivot_wider(
    id_cols = Symbol,
    names_from = grupo,
    values_from = c(Grado, Expr)
  ) %>%
  filter(
    !is.na(Grado_Control), !is.na(Grado_Alzheimer),
    !is.na(Expr_Control), !is.na(Expr_Alzheimer)
  ) %>%
  mutate(
    cuadrante_Control = case_when(
      Grado_Control > umbral_grado & Expr_Control > umbral_expr ~ "I",
      Grado_Control <= umbral_grado & Expr_Control > umbral_expr ~ "II",
      Grado_Control <= umbral_grado & Expr_Control <= umbral_expr ~ "III",
      Grado_Control > umbral_grado & Expr_Control <= umbral_expr ~ "IV"
    ),
    cuadrante_Alzheimer = case_when(
      Grado_Alzheimer > umbral_grado & Expr_Alzheimer > umbral_expr ~ "I",
      Grado_Alzheimer <= umbral_grado & Expr_Alzheimer > umbral_expr ~ "II",
      Grado_Alzheimer <= umbral_grado & Expr_Alzheimer <= umbral_expr ~ "III",
      Grado_Alzheimer > umbral_grado & Expr_Alzheimer <= umbral_expr ~ "IV"
    ),
    cambio_cuadrante = cuadrante_Control != cuadrante_Alzheimer
  )

# Unir al data frame largo
SNARE_plot_expr <- SNARE_plot_expr %>%
  left_join(
    snare_wide %>% select(Symbol, cambio_cuadrante),
    by = "Symbol"
  ) %>%
  mutate(cambio_cuadrante = replace_na(cambio_cuadrante, FALSE))


p_snare_expr <- ggplot(SNARE_plot_expr, 
                       aes(x = Grado, y = Expr,
                           color = Comunidad, label = Symbol)) +
  geom_vline(xintercept = umbral_grado, linetype = "dashed", color = "darkgray", linewidth = 0.8) +
  geom_hline(yintercept = umbral_expr, linetype = "dashed", color = "darkgray", linewidth = 0.8) +
  
  # Puntos base (más tenues)
  geom_point(aes(color = Comunidad), size = 3, alpha = 0.6) +
  
  # Resaltar genes que cambian de cuadrante
  geom_point(
    data = ~ filter(.x, cambio_cuadrante),
    color = "black",
    size = 4,
    shape = 1,      # círculo vacío
    stroke = 1.5,
    alpha = 1,
    show.legend = FALSE
  ) +
  
  # Etiquetas solo para los que cambian
  geom_text_repel(
    data = ~ filter(.x, cambio_cuadrante),
    aes(label = Symbol),
    size = 3.5,
    max.overlaps = 100,
    segment.color = "black",
    fontface = "bold",
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

# Guardar versión con cambios resaltados
ggsave("/Users/alejandropintacastro/Downloads/INMEGEN/IPA/Alejandr@s-EHL/plots mod/SNARE_cambios_cuadricula.png",
       plot = p_snare_expr,
       width = 12,
       height = 7,
       dpi = 300)
#Verificar genes que cambian de cuadricula #
# Extraer genes que cambian de cuadrante con sus valores numéricos
snare_genes_detalles <- snare_wide %>%
  filter(cambio_cuadrante) %>%
  select(
    Symbol,
    Grado_Control, Expr_Control,
    Grado_Alzheimer, Expr_Alzheimer,
    cuadrante_Control, cuadrante_Alzheimer
  )

print(snare_genes_detalles)

write.csv(
  snare_genes_detalles,
  "/Users/alejandropintacastro/Downloads/INMEGEN/IPA/Alejandr@s-EHL/plots mod/SNARE_genes_cambio_cuadricula.csv",
  row.names = FALSE
)

###### Synaptogenesis ###########
# Preparar datos para Synaptogenesis
synaptogensis_plot_expr <- synaptogenesis_con_top %>%
  filter(
    !is.na(`Expr Intensity/RPKM/FPKM/Counts...4`),  # Expresión
    !is.na(`Expr Intensity/RPKM/FPKM/Counts...6`),  # Grado
    !is.na(grupo)
  ) %>%
  mutate(
    Grado = as.numeric(`Expr Intensity/RPKM/FPKM/Counts...6`),
    Expr = `Expr Intensity/RPKM/FPKM/Counts...4`,
    Comunidad = as.factor(as.character(`Expr Intensity/RPKM/FPKM/Counts...7`))
  ) %>%
  filter(Grado > 0, Expr > 0) %>%
  mutate(grupo = factor(grupo, levels = c("Control", "Alzheimer")))

# Calcular umbrales globales
umbral_grado <- median(synaptogensis_plot_expr$Grado, na.rm = TRUE)
umbral_expr  <- median(synaptogensis_plot_expr$Expr, na.rm = TRUE)


# Convertir a formato ancho y asignar cuadrantes
synaptogenesis_wide <- synaptogensis_plot_expr %>%
  select(Symbol, grupo, Grado, Expr) %>%
  pivot_wider(
    id_cols = Symbol,
    names_from = grupo,
    values_from = c(Grado, Expr)
  ) %>%
  filter(
    !is.na(Grado_Control), !is.na(Grado_Alzheimer),
    !is.na(Expr_Control), !is.na(Expr_Alzheimer)
  ) %>%
  mutate(
    cuadrante_Control = case_when(
      Grado_Control > umbral_grado & Expr_Control > umbral_expr ~ "I",
      Grado_Control <= umbral_grado & Expr_Control > umbral_expr ~ "II",
      Grado_Control <= umbral_grado & Expr_Control <= umbral_expr ~ "III",
      Grado_Control > umbral_grado & Expr_Control <= umbral_expr ~ "IV"
    ),
    cuadrante_Alzheimer = case_when(
      Grado_Alzheimer > umbral_grado & Expr_Alzheimer > umbral_expr ~ "I",
      Grado_Alzheimer <= umbral_grado & Expr_Alzheimer > umbral_expr ~ "II",
      Grado_Alzheimer <= umbral_grado & Expr_Alzheimer <= umbral_expr ~ "III",
      Grado_Alzheimer > umbral_grado & Expr_Alzheimer <= umbral_expr ~ "IV"
    ),
    cambio_cuadrante = cuadrante_Control != cuadrante_Alzheimer
  )

# Unir información al data frame largo
synaptogensis_plot_expr <- synaptogensis_plot_expr %>%
  left_join(
    synaptogenesis_wide %>% select(Symbol, cambio_cuadrante),
    by = "Symbol"
  ) %>%
  mutate(cambio_cuadrante = replace_na(cambio_cuadrante, FALSE))

p_synaptogensis_expr <- ggplot(synaptogensis_plot_expr, 
                               aes(x = Grado, y = Expr,
                                   color = Comunidad, label = Symbol)) +
  geom_vline(xintercept = umbral_grado, linetype = "dashed", color = "darkgray", linewidth = 0.8) +
  geom_hline(yintercept = umbral_expr, linetype = "dashed", color = "darkgray", linewidth = 0.8) +
  
  # Puntos base (más tenues para contraste)
  geom_point(aes(color = Comunidad), size = 3, alpha = 0.6) +
  
  # Resaltar genes que cambian de cuadrante
  geom_point(
    data = ~ filter(.x, cambio_cuadrante),
    color = "black",
    size = 4,
    shape = 1,      # círculo vacío
    stroke = 1.5,
    alpha = 1,
    show.legend = FALSE
  ) +
  
  # Etiquetas solo para los que cambian
  geom_text_repel(
    data = ~ filter(.x, cambio_cuadrante),
    aes(label = Symbol),
    size = 3.5,
    max.overlaps = 100,
    segment.color = "black",
    fontface = "bold",
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

# Guardar versión con cambios resaltados
ggsave("/Users/alejandropintacastro/Downloads/INMEGEN/IPA/Alejandr@s-EHL/plots mod/Synaptogenesis_cambios_cuadricula.png",
       plot = p_synaptogensis_expr,
       width = 12,
       height = 7,
       dpi = 300)

# Mostrar genes que cambian de cuadrante con sus valores
genes_cambio_detalles <- synaptogenesis_wide %>%
  filter(cambio_cuadrante) %>%
  select(
    Symbol,
    Grado_Control, Expr_Control,
    Grado_Alzheimer, Expr_Alzheimer,
    cuadrante_Control, cuadrante_Alzheimer
  )

# Imprimir en consola
print("Genes que cambian de cuadrante en Synaptogenesis:")
print(genes_cambio_detalles)

# Opcional: guardar en un archivo CSV
write.csv(genes_cambio_detalles, 
          "/Users/alejandropintacastro/Downloads/INMEGEN/IPA/Alejandr@s-EHL/plots mod/Synaptogenesis_genes_cambio_cuadricula.csv",
          row.names = FALSE)




### Tablas para indice de Jaccard ####
write.csv(
  GABA_receptor_plot_expr,
  file = "/Users/alejandropintacastro/Downloads/INMEGEN/Indices J/GABA receptor J.csv",
  row.names = FALSE)

write.csv(
  GABAergic_plot_expr,
  file = "/Users/alejandropintacastro/Downloads/INMEGEN/Indices J/GABAergic J.csv",
  row.names = FALSE
)  

write.csv(
  Neurexins_plot_expr,
  file = "/Users/alejandropintacastro/Downloads/INMEGEN/Indices J/Neurexins J.csv",
  row.names = FALSE
)

write.csv(
  glutaminergic_con_top,
  file = "/Users/alejandropintacastro/Downloads/INMEGEN/Indices J/Glutaminergic.csv",
  row.names = FALSE
)

write.csv(
  SNARE_plot_expr,
  file = "/Users/alejandropintacastro/Downloads/INMEGEN/Indices J/SNARE J.csv",
  row.names = FALSE
)

write.csv(
  synaptogensis_plot_expr,
  file = "/Users/alejandropintacastro/Downloads/INMEGEN/Indices J/Synaptogenesis J.csv",
  row.names = FALSE
)
