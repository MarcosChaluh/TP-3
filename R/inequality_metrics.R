source(file.path("R", "labels.R"))

#' Compute Gini and percentile ratios by agglomerado
compute_income_inequality <- function(eph) {
  eph %>%
    mutate(
      IPCF = suppressWarnings(as.numeric(IPCF)),
      PONDERA = suppressWarnings(as.numeric(PONDERA))
    ) %>%
    filter(!is.na(IPCF) & IPCF > 0, !is.na(PONDERA) & PONDERA > 0) %>%
    group_by(ANO4, TRIMESTRE, AGLOMERADO) %>%
    summarise(
      gini = reldist::gini(IPCF, weights = PONDERA),
      p90 = reldist::wtd.quantile(IPCF, q = 0.9, weight = PONDERA),
      p10 = reldist::wtd.quantile(IPCF, q = 0.1, weight = PONDERA),
      ratio_p90_p10 = p90 / p10,
      .groups = "drop"
    ) %>%
    mutate(ratio_p90_p10 = dplyr::if_else(is.infinite(ratio_p90_p10), NA_real_, ratio_p90_p10)) %>%
    arrange(desc(ANO4), desc(TRIMESTRE), AGLOMERADO)
}

#' Prepare Gini series for plotting
prepare_gini_plot_data <- function(inequality_data, aglomerados) {
  inequality_data %>%
    mutate(AGLOMERADO_NUM = suppressWarnings(as.numeric(AGLOMERADO))) %>%
    filter(AGLOMERADO_NUM %in% aglomerados) %>%
    mutate(
      fecha_decimal = ANO4 + (TRIMESTRE - 1) / 4
    ) %>%
    with_agglomerado_labels()
}

#' Plot Gini series for the selected agglomerados
plot_gini_series <- function(plot_data) {
  label_data <- plot_data %>%
    group_by(AGLOMERADO) %>%
    slice_max(order_by = fecha_decimal, n = 1) %>%
    ungroup()

  ggplot(plot_data,
         aes(x = fecha_decimal,
             y = gini,
             color = factor(AGLOMERADO),
             group = factor(AGLOMERADO))) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 1.5) +
    scale_color_brewer(palette = "Dark2") +
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
    scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
    labs(
      title = "Coeficiente de Gini (Serie de Tiempo)",
      subtitle = "Evolución de la desigualdad de ingresos en aglomerados seleccionados",
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
