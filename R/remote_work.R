# Helpers used in the remote work analysis (script 2).

load_eph_for_remote_work <- function(paths, ano = 2025, trimestre = 1) {
  readr::read_rds(processed_data_path("eph_individual_2017_2025_std.rds", paths)) %>%
    dplyr::filter(ANO4 == ano, TRIMESTRE == trimestre, expss::unlab(ESTADO) == 1) %>%
    dplyr::select(AGLOMERADO, PONDERA, PP04D_COD)
}

load_cno_to_isco <- function(paths) {
  readxl::read_xls(raw_data_path("CONVERSION_CNO-01_CIUO-08.xls", paths)) %>%
    janitor::clean_names() %>%
    dplyr::rename(
      cno2001 = x2,
      isco08 = x3
    ) %>%
    dplyr::mutate(
      cno2001 = stringr::str_replace_all(as.character(cno2001), "[^0-9]", ""),
      isco08 = stringr::str_replace_all(as.character(isco08), "[^0-9]", ""),
      isco2 = substr(isco08, 1, 2)
    ) %>%
    dplyr::select(cno2001, isco2) %>%
    dplyr::distinct()
}

load_onet_crosswalk <- function(paths) {
  conv <- readxl::read_xlsx(raw_data_path("Conversor a CNO.xlsx", paths)) %>%
    janitor::clean_names()
  col_isco2 <- c("2_isco", "x2_isco", "isco_2", "two_isco")
  col_isco2 <- col_isco2[col_isco2 %in% names(conv)][1]
  stopifnot(!is.na(col_isco2))
  list(data = conv, col = col_isco2)
}

agglomerado_labels_remote <- function() {
  tibble::tribble(
    ~AGLOMERADO, ~label_aglomerado,
    2, "Gran La Plata",
    3, "Bahía Blanca - Cerri",
    4, "Gran Rosario",
    5, "Gran Santa Fe",
    6, "Gran Paraná",
    7, "Posadas",
    8, "Gran Resistencia",
    9, "Comodoro Rivadavia - R. Tilly",
    10, "Gran Mendoza",
    12, "Corrientes",
    13, "Gran Córdoba",
    14, "Concordia",
    15, "Formosa",
    17, "Neuquén - Plottier",
    18, "S. del Estero - La Banda",
    19, "Jujuy - Palpalá",
    20, "Río Gallegos",
    22, "Gran Catamarca",
    23, "Salta",
    25, "La Rioja",
    26, "San Luis - El Chorrillo",
    27, "Gran San Juan",
    29, "Gran Tucumán - T. Viejo",
    30, "Santa Rosa - Toay",
    31, "Ushuaia - Río Grande",
    32, "Ciudad de Buenos Aires",
    33, "Partidos del GBA",
    34, "Mar del Plata - Batán",
    36, "Río Cuarto",
    38, "San Nicolás - Villa Constitución",
    91, "Rawson - Trelew",
    93, "Viedma - Carmen de Patagones"
  )
}

isco_labels_remote <- function() {
  tibble::tribble(
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
}

create_remote_work_tables <- function(eph, cno_to_isco, onet_crosswalk) {
  eph %>%
    dplyr::mutate(cno2001 = stringr::str_replace_all(as.character(PP04D_COD), "[^0-9]", "")) %>%
    dplyr::select(-PP04D_COD) %>%
    dplyr::left_join(cno_to_isco, by = "cno2001") %>%
    dplyr::filter(!is.na(isco2)) %>%
    dplyr::mutate(isco2 = sprintf("%02d", as.integer(isco2))) %>%
    dplyr::left_join(onet_crosswalk$data, by = stats::setNames("isco2", onet_crosswalk$col)) %>%
    dplyr::filter(!is.na(aioe), !is.na(teleworkable))
}

summarise_by_occupation <- function(eph_final) {
  eph_final %>%
    dplyr::group_by(isco2) %>%
    dplyr::summarise(
      empleo_total = sum(PONDERA, na.rm = TRUE),
      ai_exp_prom = stats::weighted.mean(aioe, w = PONDERA, na.rm = TRUE),
      wfh_prom = stats::weighted.mean(teleworkable, w = PONDERA, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::left_join(isco_labels_remote(), by = "isco2") %>%
    dplyr::arrange(dplyr::desc(empleo_total)) %>%
    dplyr::select(isco2, label_isco2, dplyr::everything())
}

summarise_by_agglomerado <- function(eph_final) {
  eph_final %>%
    dplyr::mutate(AGLOMERADO = expss::unlab(AGLOMERADO)) %>%
    dplyr::group_by(AGLOMERADO) %>%
    dplyr::summarise(
      empleo_total = sum(PONDERA, na.rm = TRUE),
      ai_exp_prom = stats::weighted.mean(aioe, w = PONDERA, na.rm = TRUE),
      wfh_prom = stats::weighted.mean(teleworkable, w = PONDERA, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::left_join(agglomerado_labels_remote(), by = "AGLOMERADO")
}
