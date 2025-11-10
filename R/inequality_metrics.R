source(file.path("R", "labels.R"))

#' Compute Gini and percentile ratios by provincia
compute_income_inequality <- function(eph) {
  eph %>%
    mutate(
      IPCF = suppressWarnings(as.numeric(IPCF)),
      PONDERA = suppressWarnings(as.numeric(PONDERA))
    ) %>%
    filter(!is.na(IPCF) & IPCF > 0, !is.na(PONDERA) & PONDERA > 0) %>%
    add_province_from_agglomerado() %>%
    filter(!is.na(PROVINCIA)) %>%
    group_by(ANO4, TRIMESTRE, PROVINCIA, label_provincia) %>%
    summarise(
      gini = reldist::gini(IPCF, weights = PONDERA),
      p90 = reldist::wtd.quantile(IPCF, q = 0.9, weight = PONDERA),
      p10 = reldist::wtd.quantile(IPCF, q = 0.1, weight = PONDERA),
      ratio_p90_p10 = p90 / p10,
      .groups = "drop"
    ) %>%
    mutate(ratio_p90_p10 = dplyr::if_else(is.infinite(ratio_p90_p10), NA_real_, ratio_p90_p10)) %>%
    arrange(desc(ANO4), desc(TRIMESTRE), PROVINCIA)
}

#' Prepare Gini series for plotting
prepare_gini_plot_data <- function(inequality_data, provincias = NULL) {
  inequality_data %>%
    filter(is.null(provincias) | PROVINCIA %in% provincias) %>%
    mutate(
      fecha_decimal = ANO4 + (TRIMESTRE - 1) / 4
    )
}

#' Plot Gini series for the selected provincias
plot_gini_series <- function(plot_data) {
  label_data <- plot_data %>%
    group_by(PROVINCIA) %>%
    slice_max(order_by = fecha_decimal, n = 1) %>%
    ungroup()

  ggplot(plot_data,
         aes(x = fecha_decimal,
             y = gini,
             color = factor(label_provincia),
             group = factor(PROVINCIA))) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 1.5) +
    scale_color_brewer(palette = "Dark2") +
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
    scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
    labs(
      title = "Coeficiente de Gini (Serie de Tiempo)",
      subtitle = "Evolución de la desigualdad de ingresos por provincia",
      y = "Coeficiente de Gini",
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

#' Prepare inequality heatmap data for a given quarter
prepare_inequality_heatmap <- function(inequality_data, ano, trimestre) {
  inequality_data %>%
    filter(ANO4 == ano, TRIMESTRE == trimestre) %>%
    mutate(
      ratio_p90_p10 = dplyr::if_else(is.nan(ratio_p90_p10), NA_real_, ratio_p90_p10)
    ) %>%
    select(PROVINCIA, label_provincia, gini, ratio_p90_p10) %>%
    tidyr::pivot_longer(
      cols = c(gini, ratio_p90_p10),
      names_to = "indicador",
      values_to = "valor"
    ) %>%
    mutate(
      indicador = dplyr::recode(indicador,
                                gini = "Gini",
                                ratio_p90_p10 = "P90/P10")
    )
}

#' Plot heatmap of inequality indicators by provincia
plot_inequality_heatmap <- function(heatmap_data, ano, trimestre) {
  ggplot(heatmap_data,
         aes(x = indicador, y = reorder(label_provincia, valor, na.rm = TRUE), fill = valor)) +
    geom_tile(color = "white") +
    geom_text(aes(label = ifelse(indicador == "Gini",
                                 scales::number(valor, accuracy = 0.01),
                                 scales::number(valor, accuracy = 0.1))),
              size = 3, na.rm = TRUE) +
    scale_fill_distiller(palette = "Spectral", direction = -1, na.value = "grey90") +
    labs(
      title = "Indicadores de Desigualdad por Provincia",
      subtitle = sprintf("Valores %dT%d", ano, trimestre),
      x = "Indicador",
      y = "Provincia",
      fill = "Valor"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title = element_text(face = "bold")
    )
}
