# --- 0. Carga de Librerías ---
library(dplyr)
library(readr)
library(eph)
library(expss)   # unlab()
library(broom)   # tidy()
library(ggplot2)
library(purrr)   # map()
library(tidyr)   # nest/unnest

## -----------------------------------------------
## 1. PREPARACIÓN DE DATOS PARA MINCER
## -----------------------------------------------

eph_base_labeled <- read_rds("eph_data/eph_individual_2017_2025_std.rds")

mincer_data <- eph_base_labeled %>%
  mutate(
    # dejar numéricos “planos”
    AGLOMERADO   = expss::unlab(AGLOMERADO),
    ESTADO       = expss::unlab(ESTADO),
    CAT_OCUP     = expss::unlab(CAT_OCUP),
    CH04         = expss::unlab(CH04),
    CH06         = expss::unlab(CH06),
    CH12_NUM     = expss::unlab(CH12),
    CH13_NUM     = expss::unlab(CH13),
    
    # años de escolaridad (sin acentos para usar en fórmulas)
    anios_escolaridad = case_when(
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
    experiencia  = CH06 - anios_escolaridad - 6,
    experiencia  = ifelse(experiencia < 0, 0, experiencia),
    experiencia2 = experiencia^2,
    sexo         = factor(CH04)
  )

## -----------------------------------------------
## 2. ESTIMACIÓN DE COEFICIENTES POR AGLOMERADO
## -----------------------------------------------

coeficientes_mincer <- mincer_data %>%
  # opción 1: anidar todo menos AGLOMERADO
  tidyr::nest(data = -AGLOMERADO) %>%
  mutate(
    modelo = map(data, ~ lm(LN_WAGE_HORA ~ anios_escolaridad + experiencia + experiencia2 + sexo, data = .x)),
    coefs  = map(modelo, broom::tidy)
  ) %>%
  select(AGLOMERADO, coefs) %>%
  tidyr::unnest(coefs) %>%
  ungroup()

print(coeficientes_mincer, n = 50)

## -----------------------------------------------
## 3. GRÁFICO DE COEFICIENTES
## -----------------------------------------------

plot_coefs <- coeficientes_mincer %>%
  filter(term == "anios_escolaridad") %>%
  mutate(
    retorno_pct    = estimate * 100,
    AGLOMERADO_NUM = as.numeric(AGLOMERADO),
    label_corta = dplyr::case_when(
      AGLOMERADO_NUM == 33 ~ "GBA",
      AGLOMERADO_NUM == 32 ~ "CABA",
      AGLOMERADO_NUM == 13 ~ "Gran Córdoba",
      AGLOMERADO_NUM == 4  ~ "Gran Rosario",
      AGLOMERADO_NUM == 10 ~ "Gran Mendoza",
      TRUE ~ as.character(AGLOMERADO_NUM)
    )
  )

g_mincer_coefs <- ggplot(plot_coefs,
                         aes(x = reorder(label_corta, retorno_pct),
                             y = retorno_pct)) +
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
    subtitle = "Coeficiente de 'años de escolaridad' en Mincer (2017–2025)",
    y = "Aumento % del ingreso por hora por cada año de educación",
    x = "AGLOMERADO"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(face = "bold")
  )

ggsave("grafico_1d_mincer_retorno_educacion.png", plot = g_mincer_coefs, width = 11, height = 8, dpi = 300)
print(g_mincer_coefs)
