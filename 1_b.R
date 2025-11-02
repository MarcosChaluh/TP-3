# --- 0. Carga de Librerías ---
library(dplyr)
library(readr)
library(ggplot2)
library(ggrepel)
library(reldist) 
library(RColorBrewer) 

## -----------------------------------------------
## 1. CÁLCULO DE DESIGUALDAD (Punto 1b)
## -----------------------------------------------

# --- Cargar la base de datos procesada ---
eph_base_labeled <- read_rds("eph_data/eph_individual_2017_2025_std.rds")

# --- 1.1. Preparar los datos para el cálculo ---
inequality_data <- eph_base_labeled %>%
  mutate(
    IPCF = as.numeric(IPCF),
    PONDERA = as.numeric(PONDERA)
  ) %>%
  filter(
    !is.na(IPCF) & IPCF > 0 & 
      !is.na(PONDERA) & PONDERA > 0
  )

# --- 1.2. Calcular Gini y Ratio P90/P10 ---
calculos_desigualdad <- inequality_data %>%
  group_by(ANO4, TRIMESTRE, AGLOMERADO) %>%
  summarise(
    gini = reldist::gini(IPCF, weights = PONDERA),
    p90 = reldist::wtd.quantile(IPCF, q = 0.9, weight = PONDERA),
    p10 = reldist::wtd.quantile(IPCF, q = 0.1, weight = PONDERA),
    ratio_p90_p10 = p90 / p10,
    .groups = 'drop'
  ) %>%
  mutate(ratio_p90_p10 = ifelse(is.infinite(ratio_p90_p10), NA, ratio_p90_p10)) %>%
  arrange(desc(ANO4), desc(TRIMESTRE), AGLOMERADO)

# --- 1.3. Ver los resultados en la consola ---
print(calculos_desigualdad, n = 40)


## -----------------------------------------------
## 2. GRÁFICO DE SERIES DE TIEMPO (Gini)
## -----------------------------------------------

# --- Definir los códigos de aglomerados a graficar ---
aglomerados_grandes_cods <- c(32, 33, 13, 4, 10)

# --- Preparar los datos para el gráfico ---
plot_data_gini <- calculos_desigualdad %>%
  mutate(AGLOMERADO_NUM = as.numeric(AGLOMERADO)) %>%
  filter(AGLOMERADO_NUM %in% aglomerados_grandes_cods) %>%
  mutate(
    fecha_decimal = ANO4 + (TRIMESTRE - 1) / 4
  )

# --- Crear un data.frame para las etiquetas ---
label_data_gini <- plot_data_gini %>%
  group_by(AGLOMERADO) %>%
  slice_max(order_by = fecha_decimal, n = 1) %>%
  ungroup() %>%
  mutate(
    label_corta = case_when(
      AGLOMERADO_NUM == 33 ~ "GBA",
      AGLOMERADO_NUM == 32 ~ "CABA",
      AGLOMERADO_NUM == 13 ~ "Gran Córdoba",
      AGLOMERADO_NUM == 4  ~ "Gran Rosario",
      AGLOMERADO_NUM == 10 ~ "Gran Mendoza",
      TRUE ~ as.character(AGLOMERADO)
    )
  )

# --- Crear el gráfico ---
g_gini <- ggplot(plot_data_gini, 
                 # --- ¡AQUÍ ESTÁ LA CORRECCIÓN! ---
                 # Envolvemos AGLOMERADO con factor()
                 aes(x = fecha_decimal, 
                     y = gini, 
                     color = factor(AGLOMERADO), 
                     group = factor(AGLOMERADO))) +
  
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.5) +
  
  # --- Paleta de colores (ahora funciona) ---
  scale_color_brewer(palette = "Dark2") + 
  
  # --- Añadir labels ---
  geom_label_repel(
    data = label_data_gini,
    aes(label = label_corta), 
    nudge_x = 0.25,
    direction = "y",
    hjust = 0,
    segment.color = 'grey50',
    size = 3.5
  ) +
  
  # --- Ajustar los ejes ---
  scale_x_continuous(
    limits = c(min(plot_data_gini$fecha_decimal), max(plot_data_gini$fecha_decimal) + 0.5), 
    breaks = seq(floor(min(plot_data_gini$fecha_decimal)), 
                 ceiling(max(plot_data_gini$fecha_decimal)), by = 1)
  ) +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01)
  ) +
  
  # --- Títulos y etiquetas ---
  labs(
    title = "Coeficiente de Gini (Serie de Tiempo)",
    subtitle = "Evolución de la desigualdad de ingresos (IPCF) en 5 grandes aglomerados",
    y = "Coeficiente de Gini",
    x = "Año"
  ) +
  
  # --- Tema y leyenda ---
  theme_minimal() +
  theme(
    legend.position = "none", # Sacamos la leyenda, ya tenemos labels
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 11, face = "bold")
  )

# --- Guardar el gráfico ---
ggsave("grafico_1b.png", plot = g_gini, width = 11, height = 7, dpi = 300)

# --- Mostrar el gráfico (en RStudio) ---
print(g_gini)
