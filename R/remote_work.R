# Helpers used in the remote work analysis (script 2).

source(file.path("R", "labels.R"))

load_eph_for_remote_work <- function(paths, ano = 2025, trimestre = 1) {
  readr::read_rds(processed_data_path("eph_individual_2017_2025_std.rds", paths)) %>%
    dplyr::filter(ANO4 == ano, TRIMESTRE == trimestre, expss::unlab(ESTADO) == 1) %>%
    dplyr::select(AGLOMERADO, PONDERA, PP04D_COD, PP04B_COD) %>%
    add_province_from_agglomerado()
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

industry_section_labels_remote <- function() {
  tibble::tribble(
    ~industry_section, ~label_industry,
    "A", "Agricultura, ganadería, caza, silvicultura y pesca",
    "B", "Explotación de minas y canteras",
    "C", "Industria manufacturera",
    "D", "Suministro de electricidad, gas, vapor y aire acondicionado",
    "E", "Suministro de agua; alcantarillado, gestión de desechos y saneamiento",
    "F", "Construcción",
    "G", "Comercio al por mayor y al por menor; reparación de vehículos",
    "H", "Transporte y almacenamiento",
    "I", "Alojamiento y servicios de comidas",
    "J", "Información y comunicación",
    "K", "Actividades financieras y de seguros",
    "L", "Actividades inmobiliarias",
    "M", "Actividades profesionales, científicas y técnicas",
    "N", "Actividades administrativas y servicios de apoyo",
    "O", "Administración pública y defensa; planes de seguro social obligatorio",
    "P", "Enseñanza",
    "Q", "Salud humana y servicios sociales",
    "R", "Artes, entretenimiento y recreación",
    "S", "Otras actividades de servicios",
    "T", "Actividades de los hogares como empleadores y productores para uso propio",
    "U", "Actividades de organizaciones y organismos extraterritoriales",
    "V", "Descripción de actividad vacía",
    "W", "Falsos ocupados",
    "Z", "Actividad no especificada claramente"
  )
}

create_remote_work_tables <- function(eph, cno_to_isco, onet_crosswalk) {
  eph %>%
    dplyr::mutate(cno2001 = stringr::str_replace_all(as.character(PP04D_COD), "[^0-9]", "")) %>%
    dplyr::select(-PP04D_COD) %>%
    dplyr::left_join(cno_to_isco, by = "cno2001") %>%
    dplyr::filter(!is.na(isco2)) %>%
    dplyr::mutate(isco2 = sprintf("%02d", as.integer(isco2))) %>%
    dplyr::left_join(onet_crosswalk$data, by = stats::setNames(onet_crosswalk$col, "isco2")) %>%
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

summarise_by_province <- function(eph_final) {
  eph_final %>%
    dplyr::filter(!is.na(PROVINCIA)) %>%
    dplyr::group_by(PROVINCIA, label_provincia) %>%
    dplyr::summarise(
      empleo_total = sum(PONDERA, na.rm = TRUE),
      ai_exp_prom = stats::weighted.mean(aioe, w = PONDERA, na.rm = TRUE),
      wfh_prom = stats::weighted.mean(teleworkable, w = PONDERA, na.rm = TRUE),
      .groups = "drop"
    )
}

prepare_remote_work_heatmap <- function(resumen_provincia) {
  resumen_provincia %>%
    dplyr::mutate(
      ai_exp_pct = ai_exp_prom * 100,
      wfh_pct = wfh_prom * 100
    ) %>%
    dplyr::select(PROVINCIA, label_provincia, ai_exp_pct, wfh_pct) %>%
    tidyr::pivot_longer(
      cols = c(ai_exp_pct, wfh_pct),
      names_to = "indicador",
      values_to = "valor"
    ) %>%
    dplyr::mutate(
      indicador = dplyr::recode(indicador,
                                ai_exp_pct = "Exposición a IA",
                                wfh_pct = "Teletrabajo")
    )
}

plot_remote_work_heatmap <- function(heatmap_data) {
  ggplot(heatmap_data,
         aes(x = indicador, y = reorder(label_provincia, valor), fill = valor)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%0.1f%%", valor)), size = 3) +
    scale_fill_distiller(palette = "Spectral", direction = -1) +
    labs(
      title = "Trabajo Remoto y Exposición a IA por Provincia",
      subtitle = "Promedios ponderados trimestre seleccionado",
      x = "Indicador",
      y = "Provincia",
      fill = "%"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title = element_text(face = "bold")
    )
}

summarise_by_industry <- function(eph_final) {
  eph_final %>%
    dplyr::mutate(
      industry_code = expss::unlab(PP04B_COD),
      industry_code = stringr::str_to_upper(stringr::str_trim(as.character(industry_code))),
      industry_code = stringr::str_remove_all(industry_code, "[^A-Z0-9]"),
      section_numeric = suppressWarnings(as.integer(stringr::str_sub(industry_code, 1, 2))),
      industry_section = dplyr::case_when(
        stringr::str_detect(industry_code, "^[A-Z]") ~ stringr::str_sub(industry_code, 1, 1),
        !is.na(section_numeric) & section_numeric >= 1 & section_numeric <= 3 ~ "A",
        !is.na(section_numeric) & section_numeric >= 5 & section_numeric <= 9 ~ "B",
        !is.na(section_numeric) & section_numeric >= 10 & section_numeric <= 33 ~ "C",
        !is.na(section_numeric) & section_numeric == 35 ~ "D",
        !is.na(section_numeric) & section_numeric >= 36 & section_numeric <= 39 ~ "E",
        !is.na(section_numeric) & section_numeric >= 40 & section_numeric <= 43 ~ "F",
        !is.na(section_numeric) & section_numeric >= 45 & section_numeric <= 48 ~ "G",
        !is.na(section_numeric) & section_numeric >= 49 & section_numeric <= 53 ~ "H",
        !is.na(section_numeric) & section_numeric >= 55 & section_numeric <= 56 ~ "I",
        !is.na(section_numeric) & section_numeric >= 58 & section_numeric <= 63 ~ "J",
        !is.na(section_numeric) & section_numeric >= 64 & section_numeric <= 66 ~ "K",
        !is.na(section_numeric) & section_numeric == 68 ~ "L",
        !is.na(section_numeric) & section_numeric >= 69 & section_numeric <= 75 ~ "M",
        !is.na(section_numeric) & section_numeric >= 77 & section_numeric <= 82 ~ "N",
        !is.na(section_numeric) & section_numeric >= 83 & section_numeric <= 84 ~ "O",
        !is.na(section_numeric) & section_numeric == 85 ~ "P",
        !is.na(section_numeric) & section_numeric >= 86 & section_numeric <= 88 ~ "Q",
        !is.na(section_numeric) & section_numeric >= 90 & section_numeric <= 93 ~ "R",
        !is.na(section_numeric) & section_numeric >= 94 & section_numeric <= 96 ~ "S",
        !is.na(section_numeric) & section_numeric >= 97 & section_numeric <= 98 ~ "T",
        !is.na(section_numeric) & section_numeric == 99 ~ "U",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::mutate(industry_section = dplyr::na_if(industry_section, "")) %>%
    dplyr::filter(!is.na(industry_section)) %>%
    dplyr::group_by(industry_section) %>%
    dplyr::summarise(
      empleo_total = sum(PONDERA, na.rm = TRUE),
      ai_exp_prom = stats::weighted.mean(aioe, w = PONDERA, na.rm = TRUE),
      wfh_prom = stats::weighted.mean(teleworkable, w = PONDERA, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::left_join(industry_section_labels_remote(), by = "industry_section") %>%
    dplyr::mutate(
      label_industry = dplyr::coalesce(label_industry, "Sin clasificar")
    ) %>%
    dplyr::arrange(dplyr::desc(empleo_total)) %>%
    dplyr::select(industry_section, label_industry, dplyr::everything())
}
