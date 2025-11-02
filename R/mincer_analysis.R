source(file.path("R", "labels.R"))

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

#' Estimate Mincer regressions by agglomerado
estimate_mincer_by_aglomerado <- function(mincer_data) {
  mincer_data %>%
    tidyr::nest(data = -AGLOMERADO) %>%
    mutate(
      modelo = purrr::map(data, ~ lm(LN_WAGE_HORA ~ anios_escolaridad + experiencia + experiencia2 + sexo, data = .x)),
      coefs = purrr::map(modelo, broom::tidy)
    ) %>%
    select(AGLOMERADO, coefs) %>%
    tidyr::unnest(coefs) %>%
    ungroup()
}

#' Plot the education returns by agglomerado
plot_mincer_returns <- function(coeficientes) {
  plot_data <- coeficientes %>%
    filter(term == "anios_escolaridad") %>%
    mutate(
      retorno_pct = estimate * 100,
      AGLOMERADO_NUM = suppressWarnings(as.numeric(AGLOMERADO))
    ) %>%
    with_agglomerado_labels(label_col = "label_corta")

  ggplot(plot_data,
         aes(x = reorder(label_corta, retorno_pct), y = retorno_pct)) +
    geom_col(fill = "#0072B2") +
    coord_flip() +
    geom_text(aes(label = paste0(round(retorno_pct, 1), "%")),
              hjust = -0.2, size = 3.5, color = "black") +
    scale_y_continuous(
      labels = function(y) paste0(round(y, 0), "%"),
      expand = expansion(mult = c(0, 0.1))
    ) +
    labs(
      title = "Retorno a la Educación por Aglomerado",
      subtitle = "Coeficiente de 'años de escolaridad' en Mincer",
      y = "Aumento % del ingreso por hora por cada año de educación",
      x = "Aglomerado"
    ) +
    theme_minimal() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.title = element_text(face = "bold")
    )
}
