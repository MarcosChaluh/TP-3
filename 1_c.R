# --- 0. Carga de Librerías ---
library(dplyr)
library(readr)
library(ggplot2)
library(ggrepel)
library(RColorBrewer)
library(eph) # ¡Librería clave para este punto!

## -----------------------------------------------
## 1. CÁLCULO DE POBREZA (Punto 1c)
## -----------------------------------------------

# --- Cargar la base de datos procesada ---
eph_base_labeled <- read_rds("eph_data/eph_individual_2017_2025.rds")

# --- Descargar las canastas oficiales ---
canasta <- get_poverty_lines(regional = TRUE)

# --- Ejecutar la función para calcular la pobreza ---
# Aseguramos que las columnas de cruce sean numéricas
base_con_pobreza <- calculate_poverty(
  base = eph_base_numeric,
  basket = canasta, 
  print_summary = TRUE 
)


## -----------------------------------------------
## 2. AGREGACIÓN DE TASAS POR REGIÓN
## -----------------------------------------------

tasas_pobreza <- base_con_pobreza %>%
  
  # Agrupar por tiempo y REGIÓN
  group_by(ANO4, TRIMESTRE, REGION) %>%
  
  # Calcular los agregados poblacionales
  summarise(
    # --- Usamos PONDIH y minúsculas (según tu código) ---
    total_indigentes = sum(PONDIH[situacion == "indigente"], na.rm = TRUE),
    total_pobres_no_ind = sum(PONDIH[situacion == "pobre"], na.rm = TRUE),
    
    # El total de población es la suma de PONDIH
    total_poblacion = sum(PONDIH, na.rm = TRUE),
    
    .groups = 'drop'
  ) %>%
  
  # Calcular las tasas (con control de división por cero)
  mutate(
    # Tasa de Pobreza = (Pobres No Indigentes + Indigentes) / Total
    tasa_pobreza = ifelse(total_poblacion == 0, NA, 
                          (total_indigentes + total_pobres_no_ind) / total_poblacion),
    
    # Tasa de Indigencia = Indigentes / Total
    tasa_indigencia = ifelse(total_poblacion == 0, NA, 
                             total_indigentes / total_poblacion)
  ) %>%
  
  # Ordenar para ver lo más reciente primero
  arrange(desc(ANO4), desc(TRIMESTRE), REGION)

# --- Ver los resultados en la consola ---
print(tasas_pobreza, n = 40)


## -----------------------------------------------
## 3. GRÁFICO DE SERIES DE TIEMPO (Pobreza por REGIÓN)
## -----------------------------------------------
# (Esta sección está corregida para graficar por REGIÓN)

# --- Preparar los datos para el gráfico (ahora por REGIÓN) ---
plot_data_pobreza <- tasas_pobreza %>%
  # Nos aseguramos que REGION sea numérica para el 'case_when'
  mutate(REGION_NUM = as.numeric(REGION)) %>%
  # Filtramos la región 0 (Total Urbano) si existiera
  filter(REGION_NUM != 0) %>%
  mutate(
    fecha_decimal = ANO4 + (TRIMESTRE - 1) / 4,
    tasa_pobreza_pct = tasa_pobreza * 100 # Convertir a porcentaje
  )

# --- Crear un data.frame para las etiquetas de REGIÓN ---
label_data_pobreza <- plot_data_pobreza %>%
  group_by(REGION) %>%
  slice_max(order_by = fecha_decimal, n = 1) %>%
  ungroup() %>%
  mutate(
    # Etiquetas cortas para las 6 regiones
    label_corta = case_when(
      REGION_NUM == 1  ~ "GBA",
      REGION_NUM == 40 ~ "Noroeste (NOA)",
      REGION_NUM == 41 ~ "Noreste (NEA)",
      REGION_NUM == 42 ~ "Cuyo",
      REGION_NUM == 43 ~ "Pampeana",
      REGION_NUM == 44 ~ "Patagonia",
      TRUE ~ as.character(REGION)
    )
  )

# --- Crear el gráfico ---
g_pobreza_region <- ggplot(plot_data_pobreza, 
                           aes(x = fecha_decimal, 
                               y = tasa_pobreza_pct, 
                               color = factor(REGION), 
                               group = factor(REGION))) +
  
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.5) +
  
  # --- Paleta de colores (Dark2 tiene 8 colores, perfecto para 6 regiones) ---
  scale_color_brewer(palette = "Dark2") + 
  
  # --- Añadir labels ---
  geom_label_repel(
    data = label_data_pobreza,
    aes(label = label_corta), 
    nudge_x = 0.25,
    direction = "y",
    hjust = 0,
    segment.color = 'grey50',
    size = 3.5
  ) +
  
  # --- Ajustar los ejes ---
  scale_x_continuous(
    limits = c(min(plot_data_pobreza$fecha_decimal), max(plot_data_pobreza$fecha_decimal) + 0.5), 
    breaks = seq(floor(min(plot_data_pobreza$fecha_decimal)), 
                 ceiling(max(plot_data_pobreza$fecha_decimal)), by = 1)
  ) +
  scale_y_continuous(
    # Formato de Porcentaje
    labels = function(y) paste0(round(y, 0), "%")
  ) +
  
  # --- Títulos y etiquetas (actualizados a REGIÓN) ---
  labs(
    title = "Tasa de Pobreza por Región (Serie de Tiempo)",
    subtitle = "Evolución trimestral por región (2017-2025)",
    y = "Tasa de Pobreza",
    x = "Año"
  ) +
  
  # --- Tema y leyenda ---
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 11, face = "bold")
  )

# --- Guardar el gráfico (con nuevo nombre) ---
ggsave("grafico_1c.png", plot = g_pobreza_region, width = 11, height = 7, dpi = 300)

# --- Mostrar el gráfico (en RStudio) ---
print(g_pobreza_region)
