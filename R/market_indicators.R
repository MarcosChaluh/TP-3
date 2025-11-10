source(file.path("R", "labels.R"))

#' Compute labour market rates by provincia
compute_market_rates <- function(eph) {
  eph %>%
    mutate(ESTADO = suppressWarnings(as.numeric(ESTADO))) %>%
    add_province_from_agglomerado() %>%
    filter(!is.na(PROVINCIA)) %>%
    group_by(ANO4, TRIMESTRE, PROVINCIA, label_provincia) %>%
    summarise(
      total_ocupados = sum(PONDERA[ESTADO == 1], na.rm = TRUE),
      total_desocupados = sum(PONDERA[ESTADO == 2], na.rm = TRUE),
      pea = total_ocupados + total_desocupados,
      pob_10_mas = sum(PONDERA, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      tasa_actividad = dplyr::if_else(pob_10_mas == 0, NA_real_, pea / pob_10_mas),
      tasa_empleo = dplyr::if_else(pob_10_mas == 0, NA_real_, total_ocupados / pob_10_mas),
      tasa_desempleo = dplyr::if_else(pea == 0, NA_real_, total_desocupados / pea)
    ) %>%
    arrange(desc(ANO4), desc(TRIMESTRE), PROVINCIA, label_provincia)
}

#' Prepare unemployment rate data for plotting
prepare_unemployment_plot_data <- function(rates, provincias = NULL) {
  rates %>%
    filter(is.null(provincias) | PROVINCIA %in% provincias) %>%
    mutate(
      fecha_decimal = ANO4 + (TRIMESTRE - 1) / 4,
      tasa_desempleo_pct = tasa_desempleo * 100
    )
}

#' Plot unemployment series with labels at the end of the lines
plot_unemployment_series <- function(plot_data) {
  label_data <- plot_data %>%
    group_by(PROVINCIA) %>%
    slice_max(order_by = fecha_decimal, n = 1) %>%
    ungroup()

  ggplot(plot_data,
         aes(x = fecha_decimal,
             y = tasa_desempleo_pct,
             color = factor(label_provincia),
             group = factor(PROVINCIA))) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 1.5) +
    ggrepel::geom_label_repel(
      data = label_data,
      aes(label = label_provincia),
      nudge_x = 0.25,
      direction = "y",
      hjust = 0,
      segment.color = "grey50",
      size = 3.5
    ) +
    scale_x_continuous(
      limits = c(min(plot_data$fecha_decimal), max(plot_data$fecha_decimal) + 0.5),
      breaks = seq(floor(min(plot_data$fecha_decimal)),
                   ceiling(max(plot_data$fecha_decimal)), by = 1)
    ) +
    scale_y_continuous(labels = function(y) paste0(round(y, 0), "%")) +
    scale_color_brewer(palette = "Dark2") +
    labs(
      title = "Tasa de Desempleo (Serie de Tiempo)",
      subtitle = "Evolución trimestral por provincia seleccionada",
      y = "Tasa de Desempleo",
      x = "Año"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(size = 11, face = "bold")
    )
}

#' Prepare unemployment heatmap data for a given quarter
prepare_unemployment_heatmap <- function(rates, ano, trimestre) {
  rates %>%
    filter(ANO4 == ano, TRIMESTRE == trimestre) %>%
    mutate(
      tasa_actividad_pct = tasa_actividad * 100,
      tasa_empleo_pct = tasa_empleo * 100,
      tasa_desempleo_pct = tasa_desempleo * 100
    ) %>%
    select(PROVINCIA, label_provincia, tasa_actividad_pct, tasa_empleo_pct, tasa_desempleo_pct) %>%
    tidyr::pivot_longer(
      cols = starts_with("tasa_"),
      names_to = "indicador",
      values_to = "valor"
    ) %>%
    mutate(
      indicador = dplyr::recode(indicador,
                                tasa_actividad_pct = "Actividad",
                                tasa_empleo_pct = "Empleo",
                                tasa_desempleo_pct = "Desempleo")
    )
}

#' Plot heatmap of labour market rates by province
plot_unemployment_heatmap <- function(heatmap_data, ano, trimestre) {
  ggplot(heatmap_data,
         aes(x = indicador, y = reorder(label_provincia, valor), fill = valor)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%0.1f%%", valor)), size = 3) +
    scale_fill_distiller(palette = "Spectral", direction = -1) +
    labs(
      title = "Indicadores del Mercado Laboral por Provincia",
      subtitle = sprintf("Valores porcentuales %dT%d", ano, trimestre),
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
