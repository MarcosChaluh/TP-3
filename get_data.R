library(eph)
library(dplyr)
library(purrr)
library(readr)
library(tidyr)

# ==== 1) Definición de años y trimestres ====
years <- 2017:2024
trims <- 1:4

# ==== 2) Función segura de descarga ====
safe_get_micro <- possibly(function(y, t) {
  eph::get_microdata(year = y, trimester = t, type = "individual")
}, otherwise = NULL)

# ==== 3) Descargar todo 2017–2024 ====
grid <- crossing(years, trims)
eph_list <- map2(grid$years, grid$trims, safe_get_micro)
eph_valid <- compact(eph_list)  # quita NULLs

# ==== 4) Estandarizar nombres y tipos ====
std_names_types <- function(df) {
  names(df) <- toupper(names(df))
  df <- df %>%
    mutate(
      CH03 = suppressWarnings(as.integer(CH03)),
      CH04 = suppressWarnings(as.integer(CH04)),
      CH05 = suppressWarnings(as.integer(CH05)),
      CH06 = suppressWarnings(as.integer(CH06)),
      NIVEL_ED = as.character(NIVEL_ED)
    )
  df
}

eph_2017_2024 <- bind_rows(lapply(eph_valid, std_names_types))

# ==== 5) Descargar 2025T1 ====
eph_2025_t1 <- eph::get_microdata(year = 2025, trimester = 1, type = "individual") %>%
  std_names_types()

# ==== 6) Unir todo ====
eph_all <- bind_rows(eph_2017_2024, eph_2025_t1)

# ==== 7) Crear CH05 si no existe ====
if (!"CH05" %in% names(eph_all)) eph_all$CH05 <- NA_integer_


# ==== 8) Selección de variables y derivadas ====

# Definimos todas las variables que necesitamos para el análisis
vars_to_keep <- c(
  # --- IDs y Contexto ---
  "ANO4", "TRIMESTRE", 
  "CODUSU", "NRO_HOGAR", "COMPONENTE", # IDs de hogar y persona
  "PONDERA", "AGLOMERADO", "REGION",   # Ponderador y Geografía
  
  # --- Punto 1a: Tasas de Mercado Laboral ---
  "ESTADO",
  
  # --- Puntos 1b y 1c: Desigualdad y Pobreza ---
  "IPCF",   # Ingreso Per Cápita Familiar (para Gini)
  "ITF",    # Ingreso Total Familiar (para Pobreza)
  "IX_TOT", # Miembros del hogar (para Pobreza)
  "P47T",   # Ingreso Total Individual (para Gini alternativo)
  
  # --- Punto 1d: Ecuación de Mincer ---
  "P21",         # Ingreso de la ocupación principal
  "PP3E_TOT",    # Horas trabajadas por semana
  "CH06",        # Edad
  "CH04",        # Sexo
  "CH12",        # Nivel educativo (raw)
  "CH13",        # Terminó nivel (raw)
  "CAT_OCUP",    # Categoría ocupacional
  "PP07H",       # Descuento jubilatorio (proxy formalidad)
  
  # --- Puntos 2a y 2b: O*NET y Trabajo Remoto ---
  "PP04D_COD",   # Código de ocupación (para cruzar con O*NET)
  "PP04G"        # Lugar de trabajo (para proxy remoto)
)

# AHORA SÍ: Usamos eph_all (la base numérica)
eph_base_numeric <- eph_all %>%
  # Usamos any_of() para seleccionar variables que podrían no estar en todas las olas
  select(any_of(vars_to_keep)) %>%
  
  # --- Creación de variables derivadas ---
  mutate(
    # Variable dependiente para Mincer: Log del Ingreso por Hora
    LN_WAGE_HORA = case_when(
      !is.na(P21) & P21 > 0 & !is.na(PP3E_TOT) & PP3E_TOT > 0 ~ log((P21 / 4.3333) / PP3E_TOT),
      TRUE ~ NA_real_ 
    ),
    
    # Edad al cuadrado (para Mincer)
    EDAD2 = ifelse(!is.na(CH06), CH06^2, NA_real_)
  )

# ==== 9) Aplicar etiquetas (Labels) AL FINAL ====
# Ahora que terminamos los cálculos, etiquetamos el resultado
eph_base_labeled <- organize_labels(eph_base_numeric, type = "individual")


# ==== 10) Guardar ====
dir.create("eph_data", showWarnings = FALSE)
# Guardamos la base final con etiquetas
write_rds(eph_base_labeled, "eph_data/eph_individual_2017_2025_std.rds")
write_csv(eph_base_labeled, "eph_data/eph_individual_2017_2025_std.csv")


# ==== 11) Chequeo rápido ====
eph_base_labeled %>%
  count(ANO4, TRIMESTRE, name = "n_obs") %>%
  arrange(ANO4, TRIMESTRE) %>%
  print(n = 50)

# Chequeamos la nueva variable (ya debería ser numérica, no "labelled")
print(summary(eph_base_labeled$LN_WAGE_HORA))

# Chequeamos una variable etiquetada
str(eph_base_labeled$ESTADO)
