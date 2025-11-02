library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(janitor)

## --- 0. Definir Rutas ---
BASE <- "eph_data"
OUTPUT_DIR <- file.path(BASE, "output")
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

## --- 1. Cargar y Limpiar EPH ---
# Cargar datos EPH, filtrar por período y universo (ocupados)
# y seleccionar SOLO las variables necesarias.
eph <- read_rds(file.path(BASE, "eph_individual_2017_2025_std.rds")) %>%
  filter(ANO4 == 2025, TRIMESTRE == 1, expss::unlab(ESTADO) == 1) %>%
  
  # Seleccionar solo las columnas mínimas para este análisis
  select(AGLOMERADO, PONDERA, PP04D_COD) %>%
  
  # Limpiar el código de ocupación (CNO)
  mutate(
    cno2001 = str_replace_all(as.character(PP04D_COD), "[^0-9]", "")
  ) %>%
  select(-PP04D_COD) # Eliminar la columna original

## --- 2. Cargar Crosswalk 1: CNO-2001 -> ISCO-08 ---
# Cargar y limpiar el crosswalk de INDEC
cno_to_isco <- read_xls(file.path(BASE, "CONVERSION_CNO-01_CIUO-08.xls")) %>%
  clean_names() %>%
  
  # Renombrar columnas (asumiendo que 'clean_names' las convierte a esto)
  # Esto reemplaza el bloque 'if' por un renombrado explícito.
  rename(
    cno2001 = x2,  # Asume que 'CNO-01' se convierte en 'cno_01'
    isco08  = x3  # Asume que 'CIUO-08' se convierte en 'ciuo_08'
  ) %>%
  
  # Limpiar códigos y extraer los 2 dígitos de ISCO
  mutate(
    cno2001 = str_replace_all(as.character(cno2001), "[^0-9]", ""),
    isco08  = str_replace_all(as.character(isco08), "[^0-9]", ""),
    isco2   = substr(isco08, 1, 2) # 2 dígitos
  ) %>%
  
  # Quedarnos solo con las columnas necesarias para el join
  select(cno2001, isco2) %>%
  distinct()

## --- 3. Unir EPH + ISCO ---
eph_con_isco <- eph %>%
  left_join(cno_to_isco, by = "cno2001") %>%
  # Descartar personas cuya ocupación (CNO) no matcheó con un ISCO
  filter(!is.na(isco2)) %>%
  # Estandarizar ISCO a 2 dígitos con cero ("9" -> "09")
  mutate(isco2 = sprintf("%02d", as.integer(isco2))) %>%
  select(-cno2001) # Ya no necesitamos la llave CNO

## --- 4. Cargar Crosswalk 2: ISCO -> O*NET (AI/WFH) ---
# Cargar tu archivo "Conversor" que ya tiene los scores de AI y Teletrabajo
conv <- read_xlsx(file.path(BASE, "Conversor a CNO.xlsx")) %>% 
  clean_names()

# Este bloque robusto encuentra la columna ISCO de 2 dígitos
# (ej. "2_isco", "x2_isco") en tu archivo 'conv'
col_isco2 <- c("2_isco", "x2_isco", "isco_2", "two_isco")
col_isco2 <- col_isco2[col_isco2 %in% names(conv)][1]
stopifnot(!is.na(col_isco2)) # Frena si no encuentra la columna


## --- 5. Unir EPH + O*NET y Generar Tablas ---
# Unir la base EPH (con ISCO2) al conversor O*NET
eph_final <- eph_con_isco %>%
  left_join(conv, by = c("isco2" = col_isco2)) %>%
  # Descartar personas cuyo ISCO2 no matcheó con un score
  filter(!is.na(aioe), !is.na(teleworkable))

# --- TABLA 1: AI/WFH por Ocupación (O*NET Title) ---
# Agrega los scores promedio por ocupación, ponderados por el empleo

# --- Creación del Data Frame de Etiquetas ISCO-08 (2 dígitos) ---

labels_isco2 <- tibble::tribble(
  ~isco2, ~label_isco2,
  "01", "Oficiales de las fuerzas armadas",
  "02", "Suboficiales de las fuerzas armadas",
  "03", "Otros miembros de las fuerzas armadas",
  "11", "Directores ejecutivos, admin. pública y legisladores",
  "12", "Directores administrativos y comerciales",
  "13", "Directores de producción y operaciones",
  "14", "Gerentes de hotelería, comercio y otros servicios",
  "21", "Profesionales de las ciencias y de la ingeniería",
  "22", "Profesionales de la salud",
  "23", "Profesionales de la enseñanza",
  "24", "Especialistas en organización de admin. y empresas",
  "25", "Profesionales de TIC",
  "26", "Profesionales en derecho, ciencias sociales y culturales",
  "31", "Profesionales de nivel medio de ciencias e ingeniería",
  "32", "Profesionales de nivel medio de la salud",
  "33", "Profesionales de nivel medio en finanzas y administración",
  "34", "Profesionales de nivel medio de servicios jurídicos, sociales, etc.",
  "35", "Técnicos de TIC",
  "41", "Oficinistas",
  "42", "Empleados en trato directo con el público",
  "43", "Empleados contables y de registro de materiales",
  "44", "Otro personal de apoyo administrativo",
  "51", "Trabajadores de los servicios personales",
  "52", "Vendedores",
  "53", "Trabajadores de los cuidados personales",
  "54", "Personal de los servicios de protección",
  "61", "Agricultores y trabajadores calificados (mercado)",
  "62", "Trabajadores forestales, pesqueros y cazadores calificados",
  "63", "Trabajadores de subsistencia (agro, pesca, etc.)",
  "71", "Oficiales y operarios de la construcción (no electricistas)",
  "72", "Oficiales y operarios de la metalurgia y mecánica",
  "73", "Artesanos y operarios de las artes gráficas",
  "74", "Trabajadores de la electricidad y la electrotecnología",
  "75", "Operarios de la industria (alimentación, madera, textil, etc.)",
  "81", "Operadores de instalaciones fijas y máquinas",
  "82", "Ensambladores",
  "83", "Conductores de vehículos y operadores de equipos móviles",
  "91", "Limpiadores y asistentes",
  "92", "Peones agropecuarios, pesqueros y forestales",
  "93", "Peones (minería, construcción, industria, transporte)",
  "94", "Ayudantes de preparación de alimentos",
  "95", "Vendedores ambulantes (no alimentos)",
  "96", "Recolectores de desechos y otras ocupaciones elementales"
)

tabla_ocupacion <- eph_final %>%
  group_by(isco2) %>% # Agregamos isco2 al grupo
  summarise(
    empleo_total  = sum(PONDERA, na.rm = TRUE),
    ai_exp_prom   = weighted.mean(aioe, w = PONDERA, na.rm = TRUE),
    wfh_prom      = weighted.mean(teleworkable, w = PONDERA, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  # --- Unir las etiquetas ---
  left_join(labels_isco2, by = "isco2") %>%
  
  # Ordenar y mover columnas
  arrange(desc(empleo_total)) %>%
  select(isco2, label_isco2, empleo_total, everything())

readr::write_csv(tabla_ocupacion, file.path(OUTPUT_DIR, "tabla_1_AI_WFH_por_Ocupacion.csv"))

# --- TABLA 2: AI/WFH por Aglomerado ---

# --- Creación del Data Frame de Etiquetas AGLOMERADO ---

labels_aglomerado <- tibble::tribble(
  ~AGLOMERADO, ~label_aglomerado,
  2, "Gran La Plata",
  3, "Bahia Blanca - Cerri",
  4, "Gran Rosario",
  5, "Gran Santa Fe",
  6, "Gran Parana",
  7, "Posadas",
  8, "Gran Resistencia",
  9, "Cdro. Rivadavia - R.Tilly",
  10, "Gran Mendoza",
  12, "Corrientes",
  13, "Gran Cordoba",
  14, "Concordia",
  15, "Formosa",
  17, "Neuquen - Plottier",
  18, "S. del Estero - La Banda",
  19, "Jujuy - Palpala",
  20, "Rio Gallegos",
  22, "Gran Catamarca",
  23, "Salta",
  25, "La Rioja",
  26, "San Luis - El Chorrillo",
  27, "Gran San Juan",
  29, "Gran Tucuman - T. Viejo",
  30, "Santa Rosa - Toay",
  31, "Ushuaia - Rio Grande",
  32, "Ciudad de Buenos Aires",
  33, "Partidos del GBA",
  34, "Mar del Plata - Batan",
  36, "Rio Cuarto",
  38, "San Nicolas - Villa Constitucion",
  91, "Rawson - Trelew",
  93, "Viedma - Carmen de Patagones"
)

# Agrega los scores promedio por aglomerado, ponderados por el empleo
tabla_aglomerado <- eph_final %>%
  
  # ¡Importante! Convertir AGLOMERADO a numérico antes de agrupar
  mutate(AGLOMERADO = expss::unlab(AGLOMERADO)) %>% 
  
  group_by(AGLOMERADO) %>%
  summarise(
    empleo_total  = sum(PONDERA, na.rm = TRUE),
    ai_exp_prom   = weighted.mean(aioe, w = PONDERA, na.rm = TRUE),
    wfh_prom      = weighted.mean(teleworkable, w = PONDERA, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  # --- Unir las etiquetas ---
  left_join(labels_aglomerado, by = "AGLOMERADO") %>%
  
  # Ordenar y mover columnas
  arrange(desc(empleo_total)) %>%
  select(AGLOMERADO, label_aglomerado, empleo_total, everything())

readr::write_csv(tabla_aglomerado, file.path(OUTPUT_DIR, "tabla_2_AI_WFH_por_Aglomerado.csv"))

# --- Opcional: Guardar la base individual completa ---
# Guarda la base de datos a nivel individuo ya matcheada
write_csv(eph_final, file.path(OUTPUT_DIR, "EPH2025T1_con_scores_AI_WFH.csv"))

print("Proceso completado. Archivos guardados en la carpeta 'eph_data/output'")
