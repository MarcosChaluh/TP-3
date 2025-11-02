source(file.path("R", "labels.R"))

#' Run the poverty calculation using the official basket
calculate_poverty_status <- function(eph_numeric, basket) {
  eph::calculate_poverty(base = eph_numeric, basket = basket, print_summary = FALSE)
}

#' Aggregate poverty and indigence rates by region
aggregate_poverty_by_region <- function(base_con_pobreza) {
  base_con_pobreza %>%
    group_by(ANO4, TRIMESTRE, REGION) %>%
    summarise(
      total_indigentes = sum(PONDIH[situacion == "indigente"], na.rm = TRUE),
      total_pobres_no_ind = sum(PONDIH[situacion == "pobre"], na.rm = TRUE),
      total_poblacion = sum(PONDIH, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      tasa_pobreza = dplyr::if_else(total_poblacion == 0, NA_real_,
                                    (total_indigentes + total_pobres_no_ind) / total_poblacion),
      tasa_indigencia = dplyr::if_else(total_poblacion == 0, NA_real_,
                                       total_indigentes / total_poblacion)
    ) %>%
    arrange(desc(ANO4), desc(TRIMESTRE), REGION)
}

#' Prepare poverty data for plotting by region
prepare_poverty_plot_data <- function(tasas_pobreza) {
  tasas_pobreza %>%
    mutate(REGION_NUM = suppressWarnings(as.numeric(REGION))) %>%
    filter(REGION_NUM != 0) %>%
    mutate(
      fecha_decimal = ANO4 + (TRIMESTRE - 1) / 4,
      tasa_pobreza_pct = tasa_pobreza * 100
    ) %>%
    with_region_labels(label_col = "label_corta")
}

#' Plot poverty series by region
plot_poverty_series <- function(plot_data) {
  label_data <- plot_data %>%
    group_by(REGION) %>%
    slice_max(order_by = fecha_decimal, n = 1) %>%
    ungroup()

  ggplot(plot_data,
         aes(x = fecha_decimal,
             y = tasa_pobreza_pct,
             color = factor(REGION),
             group = factor(REGION))) +
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
    scale_y_continuous(labels = function(y) paste0(round(y, 0), "%")) +
    labs(
      title = "Tasa de Pobreza por Región (Serie de Tiempo)",
      subtitle = "Evolución trimestral por región",
      y = "Tasa de Pobreza",
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
