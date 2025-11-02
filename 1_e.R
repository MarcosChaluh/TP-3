# --- 0. Carga de Librerías y Datos ---
library(dplyr)
library(readr)
library(ggplot2)
library(eph)
library(expss)   # Para unlab()
library(tidyr)   # Para pivot_wider()

# Cargar la base de datos procesada
eph_base_labeled <- read_rds("eph_data/eph_individual_2017_2025_std.rds")

# Definir el período más reciente para analizar
ANO_RECIENTE <- 2025
TRIM_RECIENTE <- 1

# Función para crear etiquetas cortas
crear_label_corta <- function(df) {
  df %>%
    mutate(
      AGLOMERADO_NUM = as.numeric(AGLOMERADO),
      label_corta = case_when(
        AGLOMERADO_NUM == 33 ~ "GBA",
        AGLOMERADO_NUM == 32 ~ "CABA",
        AGLOMERADO_NUM == 13 ~ "Gran Córdoba",
        AGLOMERADO_NUM == 4  ~ "Gran Rosario",
        AGLOMERADO_NUM == 10 ~ "Gran Mendoza",
        TRUE ~ as.character(AGLOMERADO_NUM) # Usar el número para los demás
      )
    )
}

## -----------------------------------------------
## 1. INDICADOR 1: Tasa de Asalariados No Registrados (Informalidad)
## -----------------------------------------------

# Universo: Asalariados (CAT_OCUP == 3)
# Tasa: (Quienes NO tienen descuento jubilatorio [PP07H == 2]) / (Total de Asalariados)

plot_data_informalidad <- eph_base_labeled %>%
  
  # Quitar etiquetas
  mutate(
    ESTADO = expss::unlab(ESTADO),
    CAT_OCUP = expss::unlab(CAT_OCUP),
    PP07H = expss::unlab(PP07H)
  ) %>%
  
  # Filtrar período y universo
  filter(
    ANO4 == ANO_RECIENTE,
    TRIMESTRE == TRIM_RECIENTE,
    ESTADO == 1,       # Ocupados
    CAT_OCUP == 3      # Asalariados
  ) %>%
  
  group_by(AGLOMERADO) %>%
  summarise(
    # Numerador: Asalariados sin descuento jubilatorio
    no_registrados = sum(PONDERA[PP07H == 2], na.rm = TRUE),
    # Denominador: Total de asalariados
    total_asalariados = sum(PONDERA, na.rm = TRUE),
    # Tasa
    tasa_no_registro_pct = 100 * (no_registrados / total_asalariados),
    .groups = 'drop'
  ) %>%
  crear_label_corta() # Añadir etiquetas cortas

# --- Gráfico 1: Informalidad ---
g_informalidad <- ggplot(plot_data_informalidad, 
                         aes(x = reorder(label_corta, tasa_no_registro_pct), 
                             y = tasa_no_registro_pct)) +
  geom_col(fill = "#D55E00") + # Color Naranja
  coord_flip() +
  geom_text(aes(label = paste0(round(tasa_no_registro_pct, 1), "%")), 
            hjust = -0.2, size = 3.5, color = "black") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Tasa de Asalariados No Registrados (Informalidad)",
    subtitle = paste("Porcentaje de asalariados sin descuento jubilatorio", ANO_RECIENTE, "T", TRIM_RECIENTE),
    y = "Tasa de No Registro (%)",
    x = "Aglomerado"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    axis.title = element_text(face = "bold")
  )

ggsave("grafico_1e_informalidad.png", plot = g_informalidad, width = 11, height = 8, dpi = 300)
print(g_informalidad)

## -----------------------------------------------
## 2. INDICADOR 2: Brecha de Tasa de Actividad por Género
## -----------------------------------------------

# Universo: Población de 10 años o más (ESTADO 1, 2 o 3)
# Brecha: (Tasa Actividad Hombres [CH04==1]) - (Tasa Actividad Mujeres [CH04==2])

plot_data_brecha <- eph_base_labeled %>%
  
  mutate(
    ESTADO = expss::unlab(ESTADO),
    CH04 = expss::unlab(CH04)
  ) %>%
  
  # Filtrar período y universo
  filter(
    ANO4 == ANO_RECIENTE,
    TRIMESTRE == TRIM_RECIENTE,
    ESTADO %in% c(1, 2, 3) # Población 10+
  ) %>%
  
  group_by(AGLOMERADO, CH04) %>%
  summarise(
    pea = sum(PONDERA[ESTADO %in% c(1, 2)], na.rm = TRUE),
    pob_10_mas = sum(PONDERA, na.rm = TRUE),
    tasa_actividad = 100 * (pea / pob_10_mas),
    .groups = 'drop'
  ) %>%
  
  # Pivotear para tener columnas separadas por sexo
  select(AGLOMERADO, CH04, tasa_actividad) %>%
  pivot_wider(
    names_from = CH04,
    values_from = tasa_actividad,
    names_prefix = "sexo_" # sexo_1 (Varon), sexo_2 (Mujer)
  ) %>%
  
  # Calcular la brecha
  mutate(
    brecha_actividad_pp = sexo_1 - sexo_2 # Brecha en puntos porcentuales
  ) %>%
  crear_label_corta() # Añadir etiquetas

# --- Gráfico 2: Brecha de Género ---
g_brecha_genero <- ggplot(plot_data_brecha, 
                          aes(x = reorder(label_corta, brecha_actividad_pp), 
                              y = brecha_actividad_pp)) +
  geom_col(fill = "#009E73") + # Color Verde
  coord_flip() +
  geom_text(aes(label = paste0(round(brecha_actividad_pp, 1), " p.p.")), 
            hjust = -0.2, size = 3.5, color = "black") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Brecha de Género en Tasa de Actividad",
    subtitle = paste("Diferencia en puntos porcentuales (Hombres - Mujeres)", ANO_RECIENTE, "T", TRIM_RECIENTE),
    y = "Brecha de Actividad (p.p.)",
    x = "Aglomerado"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    axis.title = element_text(face = "bold")
  )

ggsave("grafico_1e_brecha_genero.png", plot = g_brecha_genero, width = 11, height = 8, dpi = 300)
print(g_brecha_genero)

## -----------------------------------------------
## 3. INDICADOR 3: Tasa de Cuentapropismo
## -----------------------------------------------

# Universo: Ocupados (ESTADO == 1)
# Tasa: (Cuentapropistas [CAT_OCUP == 2]) / (Total de Ocupados)

plot_data_cuentaprop <- eph_base_labeled %>%
  
  mutate(
    ESTADO = expss::unlab(ESTADO),
    CAT_OCUP = expss::unlab(CAT_OCUP)
  ) %>%
  
  # Filtrar período y universo
  filter(
    ANO4 == ANO_RECIENTE,
    TRIMESTRE == TRIM_RECIENTE,
    ESTADO == 1 # Ocupados
  ) %>%
  
  group_by(AGLOMERADO) %>%
  summarise(
    cuentapropistas = sum(PONDERA[CAT_OCUP == 2], na.rm = TRUE),
    total_ocupados = sum(PONDERA, na.rm = TRUE),
    tasa_cuentaprop_pct = 100 * (cuentapropistas / total_ocupados),
    .groups = 'drop'
  ) %>%
  crear_label_corta() # Añadir etiquetas

# --- Gráfico 3: Cuentapropismo ---
g_cuentaprop <- ggplot(plot_data_cuentaprop, 
                       aes(x = reorder(label_corta, tasa_cuentaprop_pct), 
                           y = tasa_cuentaprop_pct)) +
  geom_col(fill = "#56B4E9") + # Color Celeste
  coord_flip() +
  geom_text(aes(label = paste0(round(tasa_cuentaprop_pct, 1), "%")), 
            hjust = -0.2, size = 3.5, color = "black") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Tasa de Cuentapropismo",
    subtitle = paste("Porcentaje de ocupados que trabajan por su cuenta", ANO_RECIENTE, "T", TRIM_RECIENTE),
    y = "Tasa de Cuentapropismo (%)",
    x = "Aglomerado"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    axis.title = element_text(face = "bold")
  )

ggsave("grafico_1e_cuentapropismo.png", plot = g_cuentaprop, width = 11, height = 8, dpi = 300)
print(g_cuentaprop)
