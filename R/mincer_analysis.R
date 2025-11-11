source(file.path("R", "labels.R"))
source(file.path("R", "maps.R"))

#' Prepare the dataset used for the Mincer regression
prepare_mincer_data <- function(eph) {
  eph %>%
    mutate(
      AGLOMERADO = expss::unlab(AGLOMERADO),
      ESTADO = expss::unlab(ESTADO),
      CAT_OCUP = expss::unlab(CAT_OCUP),
      CH04 = expss::unlab(CH04),
      CH06 = expss::unlab(CH06),
      CH12_NUM = expss::unlab(CH12),
      CH13_NUM = expss::unlab(CH13),
      anios_escolaridad = dplyr::case_when(
        CH12_NUM %in% c(2, 3) & CH13_NUM == 1 ~ 7,
        CH12_NUM %in% c(2, 3) & CH13_NUM == 2 ~ 3,
        CH12_NUM %in% c(4, 5) & CH13_NUM == 1 ~ 12,
        CH12_NUM %in% c(4, 5) & CH13_NUM == 2 ~ 10,
        CH12_NUM %in% c(6, 7) & CH13_NUM == 1 ~ 17,
        CH12_NUM %in% c(6, 7) & CH13_NUM == 2 ~ 14,
        CH12_NUM == 8 ~ 19,
        CH12_NUM %in% c(1, 9, 99) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    add_province_from_agglomerado() %>%
    filter(!is.na(PROVINCIA)) %>%
    filter(
      ESTADO == 1,
      CAT_OCUP == 3,
      !is.na(LN_WAGE_HORA),
      !is.na(anios_escolaridad)
    ) %>%
    mutate(
      experiencia = pmax(CH06 - anios_escolaridad - 6, 0),
      experiencia2 = experiencia^2,
      sexo = factor(CH04)
    )
}

#' Estimate Mincer regressions by provincia
estimate_mincer_by_province <- function(mincer_data) {
  mincer_data %>%
    tidyr::nest(data = -c(ANO4, TRIMESTRE, PROVINCIA, label_provincia)) %>%
    mutate(
      modelo = purrr::map(data, ~ lm(LN_WAGE_HORA ~ anios_escolaridad + experiencia + experiencia2 + sexo, data = .x)),
      coefs = purrr::map(modelo, broom::tidy)
    ) %>%
    select(ANO4, TRIMESTRE, PROVINCIA, label_provincia, coefs) %>%
    tidyr::unnest(coefs) %>%
    ungroup()
}

#' Extract the education coefficient snapshot for a selected quarter
extract_mincer_snapshot <- function(coeficientes, ano, trimestre) {
  coeficientes %>%
    filter(ANO4 == ano, TRIMESTRE == trimestre, term == "anios_escolaridad") %>%
    transmute(
      ANO4,
      TRIMESTRE,
      PROVINCIA,
      label_provincia = dplyr::coalesce(label_provincia, PROVINCIA),
      coeficiente = estimate,
      error_estandar = std.error,
      estadistico_t = statistic,
      p_valor = p.value,
      retorno_pct = estimate * 100
    ) %>%
    arrange(desc(retorno_pct))
}

#' Plot the education returns by provincia
plot_mincer_returns <- function(coeficientes) {
  plot_data <- coeficientes %>%
    filter(term == "anios_escolaridad") %>%
    mutate(
      retorno_pct = estimate * 100,
      label_provincia = dplyr::coalesce(label_provincia, PROVINCIA)
    )

  ggplot(plot_data,
         aes(x = reorder(label_provincia, retorno_pct), y = retorno_pct)) +
    geom_col(fill = "#0072B2") +
    coord_flip() +
    geom_text(aes(label = paste0(round(retorno_pct, 1), "%")),
              hjust = -0.2, size = 3.5, color = "black") +
    scale_y_continuous(
      labels = function(y) paste0(round(y, 0), "%"),
      expand = expansion(mult = c(0, 0.1))
    ) +
    labs(
      title = "Retorno a la Educación por Provincia",
      subtitle = "Coeficiente de 'años de escolaridad' en Mincer",
      y = "Aumento % del ingreso por hora por cada año de educación",
      x = "Provincia"
    ) +
    theme_minimal() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.title = element_text(face = "bold")
    )
}

#' Prepare map-ready data for education returns by provincia
prepare_mincer_map <- function(coeficientes, ano, trimestre) {
  coeficientes %>%
    filter(term == "anios_escolaridad", ANO4 == ano, TRIMESTRE == trimestre) %>%
    mutate(
      retorno_pct = estimate * 100,
      label_provincia = dplyr::coalesce(label_provincia, PROVINCIA),
      indicador = "Retorno educativo",
      valor = retorno_pct
    ) %>%
    select(PROVINCIA, label_provincia, indicador, valor) %>%
    attach_province_geometry()
}

#' Plot map of education returns by provincia for a selected quarter
plot_mincer_map <- function(map_data, ano, trimestre) {
  ggplot(map_data) +
    geom_sf(aes(fill = valor), color = "white", linewidth = 0.2) +
    scale_fill_viridis_c(
      option = "inferno",
      direction = -1,
      na.value = "grey90",
      labels = function(x) sprintf("%0.1f%%", x)
    ) +
    labs(
      title = "Retorno a la Educación por Provincia",
      subtitle = sprintf("Coeficientes %dT%d", ano, trimestre),
      fill = "%"
    ) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank()
    )
}

# Backwards compatibility helper --------------------------------------------------

plot_mincer_heatmap <- function(coeficientes, ano, trimestre) {
  map_data <- prepare_mincer_map(coeficientes, ano, trimestre)
  plot_mincer_map(map_data, ano, trimestre)
}
