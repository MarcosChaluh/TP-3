source(file.path("R", "labels.R"))
source(file.path("R", "maps.R"))

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

#' Compute national aggregates of labour market stocks and rates
compute_national_market_rates <- function(rates) {
  rates %>%
    group_by(ANO4, TRIMESTRE) %>%
    summarise(
      total_ocupados = sum(total_ocupados, na.rm = TRUE),
      total_desocupados = sum(total_desocupados, na.rm = TRUE),
      pea = sum(pea, na.rm = TRUE),
      pob_10_mas = sum(pob_10_mas, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      tasa_actividad = dplyr::if_else(pob_10_mas == 0, NA_real_, pea / pob_10_mas),
      tasa_empleo = dplyr::if_else(pob_10_mas == 0, NA_real_, total_ocupados / pob_10_mas),
      tasa_desempleo = dplyr::if_else(pea == 0, NA_real_, total_desocupados / pea),
      fecha_decimal = ANO4 + (TRIMESTRE - 1) / 4,
      periodo = sprintf("%dT%d", ANO4, TRIMESTRE),
      tasa_desempleo_pct = tasa_desempleo * 100
    ) %>%
    arrange(ANO4, TRIMESTRE)
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

#' Prepare a province snapshot of labour market rates for a given quarter
prepare_unemployment_snapshot <- function(rates, ano, trimestre) {
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
    ) %>%
    group_by(indicador) %>%
    arrange(valor, .by_group = TRUE) %>%
    mutate(
      etiqueta_provincia = factor(label_provincia, levels = unique(label_provincia))
    ) %>%
    ungroup()
}

#' Plot horizontal bars with labels for the provincial labour market snapshot
plot_unemployment_snapshot <- function(snapshot_data, ano, trimestre) {
  ggplot(snapshot_data, aes(x = valor, y = etiqueta_provincia)) +
    geom_col(fill = "#08519c") +
    geom_text(
      aes(label = sprintf("%0.1f%%", valor)),
      hjust = -0.1,
      size = 3.2,
      color = "black"
    ) +
    facet_wrap(~ indicador, scales = "free_x") +
    scale_x_continuous(labels = scales::label_percent(accuracy = 0.1), expand = expansion(mult = c(0, 0.15))) +
    labs(
      title = "Indicadores del mercado laboral por provincia",
      subtitle = sprintf("Porcentajes %dT%d", ano, trimestre),
      x = "",
      y = "Provincia"
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(face = "bold"),
      panel.grid.major.y = element_blank(),
      axis.text.y = element_text(size = 8),
      axis.text.x = element_text(size = 8)
    )
}

#' Plot national unemployment time series
plot_national_unemployment_series <- function(series_data) {
  label_data <- series_data %>%
    slice_max(order_by = fecha_decimal, n = 1)

  ggplot(series_data, aes(x = fecha_decimal, y = tasa_desempleo_pct)) +
    geom_line(color = "#d7301f", linewidth = 1.2) +
    geom_point(color = "#d7301f", size = 2.2) +
    ggrepel::geom_label_repel(
      data = label_data,
      aes(label = sprintf("%0.1f%%", tasa_desempleo_pct)),
      color = "#d7301f",
      nudge_x = 0.2,
      direction = "y",
      hjust = 0,
      segment.color = "grey50",
      size = 3.5
    ) +
    scale_x_continuous(
      breaks = seq(floor(min(series_data$fecha_decimal)), ceiling(max(series_data$fecha_decimal)), by = 1),
      labels = function(x) {
        ano <- floor(x)
        trimestre <- round((x - ano) * 4 + 1)
        sprintf("%dT%d", ano, trimestre)
      }
    ) +
    scale_y_continuous(labels = function(y) sprintf("%0.1f%%", y)) +
    labs(
      title = "Tasa de desempleo nacional",
      subtitle = "Serie trimestral",
      x = "Periodo",
      y = "Tasa de desempleo (%)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(size = 11, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

#' Prepare unemployment map data for a given quarter
prepare_unemployment_map <- function(rates, ano, trimestre) {
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
    ) %>%
    attach_province_geometry()
}

#' Plot map of labour market rates by province
plot_unemployment_map <- function(map_data, ano, trimestre) {
  ggplot(map_data) +
    geom_sf(aes(fill = valor), color = "white", linewidth = 0.2) +
    facet_wrap(~ indicador) +
    scale_fill_viridis_c(
      option = "magma",
      direction = -1,
      na.value = "grey90",
      labels = function(x) sprintf("%0.1f%%", x)
    ) +
    labs(
      title = "Indicadores del Mercado Laboral por Provincia",
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

prepare_unemployment_heatmap <- function(rates, ano, trimestre) {
  prepare_unemployment_map(rates, ano, trimestre)
}

plot_unemployment_heatmap <- function(heatmap_data, ano, trimestre) {
  plot_unemployment_map(heatmap_data, ano, trimestre)
}
