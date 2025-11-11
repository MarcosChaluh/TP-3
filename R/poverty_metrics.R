source(file.path("R", "labels.R"))
source(file.path("R", "maps.R"))

#' Run the poverty calculation using the official basket
calculate_poverty_status <- function(eph_numeric, basket) {
  eph::calculate_poverty(base = eph_numeric, basket = basket, print_summary = FALSE)
}

#' Aggregate poverty and indigence rates by provincia
aggregate_poverty_by_province <- function(base_con_pobreza) {
  base_con_pobreza %>%
    add_province_from_agglomerado() %>%
    filter(!is.na(PROVINCIA)) %>%
    group_by(ANO4, TRIMESTRE, PROVINCIA, label_provincia) %>%
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
    arrange(desc(ANO4), desc(TRIMESTRE), PROVINCIA)
}

#' Prepare poverty data for plotting by provincia
prepare_poverty_plot_data <- function(tasas_pobreza, provincias = NULL) {
  tasas_pobreza %>%
    filter(is.null(provincias) | PROVINCIA %in% provincias) %>%
    mutate(
      fecha_decimal = ANO4 + (TRIMESTRE - 1) / 4,
      tasa_pobreza_pct = tasa_pobreza * 100
    )
}

#' Plot poverty series by provincia
plot_poverty_series <- function(plot_data) {
  label_data <- plot_data %>%
    group_by(PROVINCIA) %>%
    slice_max(order_by = fecha_decimal, n = 1) %>%
    ungroup()

  ggplot(plot_data,
         aes(x = fecha_decimal,
             y = tasa_pobreza_pct,
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
    scale_y_continuous(labels = function(y) paste0(round(y, 0), "%")) +
    labs(
      title = "Tasa de Pobreza por Provincia (Serie de Tiempo)",
      subtitle = "Evolución trimestral por provincia",
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

#' Prepare map data for poverty indicators by provincia
prepare_poverty_map <- function(tasas_pobreza, ano, trimestre) {
  tasas_pobreza %>%
    filter(ANO4 == ano, TRIMESTRE == trimestre) %>%
    mutate(
      tasa_pobreza_pct = tasa_pobreza * 100,
      tasa_indigencia_pct = tasa_indigencia * 100
    ) %>%
    select(PROVINCIA, label_provincia, tasa_pobreza_pct, tasa_indigencia_pct) %>%
    tidyr::pivot_longer(
      cols = starts_with("tasa_"),
      names_to = "indicador",
      values_to = "valor"
    ) %>%
    mutate(
      indicador = dplyr::recode(indicador,
                                tasa_pobreza_pct = "Pobreza",
                                tasa_indigencia_pct = "Indigencia")
    ) %>%
    attach_province_geometry()
}

#' Plot map of poverty indicators by provincia
plot_poverty_map <- function(map_data, ano, trimestre) {
  ggplot(map_data) +
    geom_sf(aes(fill = valor), color = "white", linewidth = 0.2) +
    facet_wrap(~ indicador) +
    scale_fill_viridis_c(
      option = "mako",
      direction = -1,
      na.value = "grey90",
      labels = function(x) sprintf("%0.1f%%", x)
    ) +
    labs(
      title = "Pobreza e Indigencia por Provincia",
      subtitle = sprintf("Valores porcentuales %dT%d", ano, trimestre),
      fill = "%"
    ) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      strip.text = element_text(face = "bold")
    )
}

# Backwards compatibility helpers -------------------------------------------------

prepare_poverty_heatmap <- function(tasas_pobreza, ano, trimestre) {
  prepare_poverty_map(tasas_pobreza, ano, trimestre)
}

plot_poverty_heatmap <- function(heatmap_data, ano, trimestre) {
  plot_poverty_map(heatmap_data, ano, trimestre)
}
