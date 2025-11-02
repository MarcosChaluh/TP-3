source(file.path("R", "labels.R"))

#' Compute labour market rates by agglomerado
compute_market_rates <- function(eph) {
  eph %>%
    mutate(ESTADO = suppressWarnings(as.numeric(ESTADO))) %>%
    group_by(ANO4, TRIMESTRE, AGLOMERADO) %>%
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
    arrange(desc(ANO4), desc(TRIMESTRE), AGLOMERADO)
}

#' Prepare unemployment rate data for plotting
prepare_unemployment_plot_data <- function(rates, aglomerados) {
  rates %>%
    mutate(AGLOMERADO_NUM = suppressWarnings(as.numeric(AGLOMERADO))) %>%
    filter(AGLOMERADO_NUM %in% aglomerados) %>%
    mutate(
      fecha_decimal = ANO4 + (TRIMESTRE - 1) / 4,
      tasa_desempleo_pct = tasa_desempleo * 100
    ) %>%
    with_agglomerado_labels()
}

#' Plot unemployment series with labels at the end of the lines
plot_unemployment_series <- function(plot_data) {
  label_data <- plot_data %>%
    group_by(AGLOMERADO) %>%
    slice_max(order_by = fecha_decimal, n = 1) %>%
    ungroup()

  ggplot(plot_data,
         aes(x = fecha_decimal,
             y = tasa_desempleo_pct,
             color = factor(AGLOMERADO),
             group = factor(AGLOMERADO))) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 1.5) +
    ggrepel::geom_label_repel(
      data = label_data,
      aes(label = label_corta),
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
      subtitle = "Evolución trimestral en aglomerados seleccionados",
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
