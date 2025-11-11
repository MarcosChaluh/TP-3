source(file.path("R", "labels.R"))

#' Compute informality rate among salaried workers
compute_informality_rate <- function(eph, ano, trimestre) {
  eph %>%
    mutate(
      ESTADO = expss::unlab(ESTADO),
      CAT_OCUP = expss::unlab(CAT_OCUP),
      PP07H = expss::unlab(PP07H)
    ) %>%
    filter(ANO4 == ano, TRIMESTRE == trimestre, ESTADO == 1, CAT_OCUP == 3) %>%
    add_province_from_agglomerado() %>%
    filter(!is.na(PROVINCIA)) %>%
    group_by(PROVINCIA, label_provincia) %>%
    summarise(
      no_registrados = sum(PONDERA[PP07H == 2], na.rm = TRUE),
      total_asalariados = sum(PONDERA, na.rm = TRUE),
      tasa_no_registro_pct = 100 * (no_registrados / total_asalariados),
      .groups = "drop"
    ) %>%
    arrange(desc(tasa_no_registro_pct)) %>%
    dplyr::select(PROVINCIA, label_provincia, dplyr::everything())
}

#' Compute gender gap in activity rate
compute_activity_gender_gap <- function(eph, ano, trimestre) {
  eph %>%
    mutate(
      ESTADO = expss::unlab(ESTADO),
      CH04 = expss::unlab(CH04)
    ) %>%
    filter(ANO4 == ano, TRIMESTRE == trimestre, ESTADO %in% c(1, 2, 3)) %>%
    add_province_from_agglomerado() %>%
    filter(!is.na(PROVINCIA)) %>%
    group_by(PROVINCIA, label_provincia, CH04) %>%
    summarise(
      pea = sum(PONDERA[ESTADO %in% c(1, 2)], na.rm = TRUE),
      pob_10_mas = sum(PONDERA, na.rm = TRUE),
      tasa_actividad = 100 * (pea / pob_10_mas),
      .groups = "drop"
    ) %>%
    select(PROVINCIA, label_provincia, CH04, tasa_actividad) %>%
    tidyr::pivot_wider(
      names_from = CH04,
      values_from = tasa_actividad,
      names_prefix = "sexo_"
    ) %>%
    mutate(brecha_actividad_pp = sexo_1 - sexo_2) %>%
    arrange(desc(brecha_actividad_pp)) %>%
    dplyr::select(PROVINCIA, label_provincia, dplyr::everything())
}

#' Compute self-employment share among occupied workers
compute_self_employment_rate <- function(eph, ano, trimestre) {
  eph %>%
    mutate(
      ESTADO = expss::unlab(ESTADO),
      CAT_OCUP = expss::unlab(CAT_OCUP)
    ) %>%
    filter(ANO4 == ano, TRIMESTRE == trimestre, ESTADO == 1) %>%
    add_province_from_agglomerado() %>%
    filter(!is.na(PROVINCIA)) %>%
    group_by(PROVINCIA, label_provincia) %>%
    summarise(
      cuentapropistas = sum(PONDERA[CAT_OCUP == 2], na.rm = TRUE),
      total_ocupados = sum(PONDERA, na.rm = TRUE),
      tasa_cuentaprop_pct = 100 * (cuentapropistas / total_ocupados),
      .groups = "drop"
    ) %>%
    arrange(desc(tasa_cuentaprop_pct)) %>%
    dplyr::select(PROVINCIA, label_provincia, dplyr::everything())
}

#' Generic helper to plot ranked bar charts with labels on the bars
plot_ranked_bars <- function(data, value_col, title, subtitle, y_label,
                             label_col = "label_corta", x_label = "Aglomerado") {
  data$.value <- data[[value_col]]
  label_suffix <- if (grepl("brecha", value_col)) " p.p." else "%"
  labels <- data[[label_col]]

  ggplot(data, aes(x = reorder(labels, .value), y = .value)) +
    geom_col(fill = "#0072B2") +
    coord_flip() +
    geom_text(aes(label = paste0(round(.value, 1), label_suffix)),
              hjust = -0.2, size = 3.5, color = "black") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    labs(
      title = title,
      subtitle = subtitle,
      y = y_label,
      x = x_label
    ) +
    theme_minimal() +
    theme(
      panel.grid.major.y = element_blank(),
      axis.title = element_text(face = "bold")
    )
}
