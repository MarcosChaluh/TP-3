source(file.path("R", "labels.R"))
source(file.path("R", "maps.R"))

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

#' Prepare inequality map data for a given quarter
prepare_inequality_map <- function(inequality_data, ano, trimestre) {
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
    ) %>%
    attach_province_geometry()
}

#' Plot map of inequality indicators by provincia
plot_inequality_map <- function(map_data, ano, trimestre) {
  indicator_specs <- list(
    list(
      name = "Gini",
      title = "Coeficiente de Gini",
      fill_label = "Gini",
      formatter = scales::label_number(accuracy = 0.01)
    ),
    list(
      name = "P90/P10",
      title = "Relación P90/P10",
      fill_label = "P90/P10",
      formatter = scales::label_number(accuracy = 0.1)
    )
  )

  map_theme <- theme_minimal(base_size = 11) +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank()
    )

  indicator_plots <- lapply(indicator_specs, function(spec) {
    indicator_data <- dplyr::filter(map_data, indicador == spec$name)

    if (nrow(indicator_data) == 0) {
      return(NULL)
    }

    label_values <- spec$formatter(indicator_data$valor)
    label_values[is.na(indicator_data$valor)] <- "s/d"

    indicator_labels <- indicator_data %>%
      dplyr::mutate(
        label_text = paste0(label_provincia, ": ", label_values)
      )

    ggplot(indicator_data) +
      geom_sf(aes(fill = valor), color = "white", linewidth = 0.25) +
      scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
      ggrepel::geom_label_repel(
        data = indicator_labels,
        aes(geometry = geometry, label = label_text),
        stat = "sf_coordinates",
        min.segment.length = 0,
        seed = 123,
        size = 3,
        label.size = 0.2,
        fill = scales::alpha("white", 0.85),
        segment.color = "grey40",
        max.overlaps = Inf
      ) +
      labs(
        title = spec$title,
        fill = spec$fill_label
      ) +
      map_theme +
      theme(
        plot.title = element_text(face = "bold")
      )
  })

  indicator_plots <- Filter(Negate(is.null), indicator_plots)

  if (length(indicator_plots) == 1) {
    return(indicator_plots[[1]])
  }

  grobs_matrix <- matrix(lapply(indicator_plots, ggplot2::ggplotGrob), nrow = 1)
  widths <- rep(list(grid::unit(1, "null")), ncol(grobs_matrix))
  heights <- list(grid::unit(1, "null"))
  combined <- gtable::gtable_matrix(
    name = "inequality_maps",
    grobs = grobs_matrix,
    widths = widths,
    heights = heights
  )

  combined <- gtable::gtable_add_rows(combined, grid::unit(1.5, "lines"), 0)
  combined <- gtable::gtable_add_grob(
    combined,
    grid::textGrob(
      "Indicadores de Desigualdad por Provincia",
      gp = grid::gpar(fontface = "bold", fontsize = 16)
    ),
    t = 1,
    l = 1,
    r = ncol(grobs_matrix)
  )

  combined <- gtable::gtable_add_rows(combined, grid::unit(1.1, "lines"), 1)
  combined <- gtable::gtable_add_grob(
    combined,
    grid::textGrob(
      sprintf("Valores %dT%d", ano, trimestre),
      gp = grid::gpar(fontsize = 11)
    ),
    t = 2,
    l = 1,
    r = ncol(grobs_matrix)
  )

  combined
}

# Backwards compatibility helpers -------------------------------------------------

prepare_inequality_heatmap <- function(inequality_data, ano, trimestre) {
  prepare_inequality_map(inequality_data, ano, trimestre)
}

plot_inequality_heatmap <- function(heatmap_data, ano, trimestre) {
  plot_inequality_map(heatmap_data, ano, trimestre)
}
