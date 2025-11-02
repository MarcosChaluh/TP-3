source(file.path("R", "project_paths.R"))

library(eph)
library(dplyr)
library(purrr)
library(readr)
library(tidyr)

paths <- ensure_project_structure()

# ==== 1) Definición de años y trimestres ====
years <- 2017:2024
trims <- 1:4

# ==== 2) Función segura de descarga ====
safe_get_micro <- purrr::possibly(function(y, t) {
  eph::get_microdata(year = y, trimester = t, type = "individual")
}, otherwise = NULL)

# ==== 3) Descargar todo 2017–2024 ====
grid <- tidyr::crossing(years, trims)
eph_list <- purrr::map2(grid$years, grid$trims, safe_get_micro)
eph_valid <- purrr::compact(eph_list)

# ==== 4) Estandarizar nombres y tipos ====
std_names_types <- function(df) {
  names(df) <- toupper(names(df))
  df %>%
    mutate(
      CH03 = suppressWarnings(as.integer(CH03)),
      CH04 = suppressWarnings(as.integer(CH04)),
      CH05 = suppressWarnings(as.integer(CH05)),
      CH06 = suppressWarnings(as.integer(CH06)),
      NIVEL_ED = as.character(NIVEL_ED)
    )
}

eph_2017_2024 <- dplyr::bind_rows(lapply(eph_valid, std_names_types))

# ==== 5) Descargar 2025T1 ====
eph_2025_t1 <- eph::get_microdata(year = 2025, trimester = 1, type = "individual") %>%
  std_names_types()

# ==== 6) Unir todo ====
eph_all <- dplyr::bind_rows(eph_2017_2024, eph_2025_t1)

# ==== 7) Crear CH05 si no existe ====
if (!"CH05" %in% names(eph_all)) eph_all$CH05 <- NA_integer_

# ==== 8) Selección de variables y derivadas ====
vars_to_keep <- c(
  "ANO4", "TRIMESTRE",
  "CODUSU", "NRO_HOGAR", "COMPONENTE",
  "PONDERA", "PONDIH", "PONDIIO", "PONDII", "AGLOMERADO", "REGION",
  "ESTADO",
  "IPCF", "ITF", "IX_TOT", "P47T",
  "P21", "PP3E_TOT", "CH06", "CH04", "CH12", "CH13", "CAT_OCUP", "PP07H",
  "PP04D_COD", "PP04B_COD", "PP04G"
)

eph_base_numeric <- eph_all %>%
  dplyr::select(dplyr::any_of(vars_to_keep)) %>%
  dplyr::mutate(
    LN_WAGE_HORA = dplyr::case_when(
      !is.na(P21) & P21 > 0 & !is.na(PP3E_TOT) & PP3E_TOT > 0 ~ log((P21 / 4.3333) / PP3E_TOT),
      TRUE ~ NA_real_
    ),
    EDAD2 = dplyr::if_else(!is.na(CH06), CH06^2, NA_real_)
  )

# ==== 9) Aplicar etiquetas (Labels) ====
eph_base_labeled <- organize_labels(eph_base_numeric, type = "individual")

# ==== 10) Guardar ====
readr::write_rds(eph_base_numeric, processed_data_path("eph_individual_2017_2025.rds", paths))
readr::write_rds(eph_base_labeled, processed_data_path("eph_individual_2017_2025_std.rds", paths))

# ==== 11) Chequeos rápidos ====
eph_base_labeled %>%
  count(ANO4, TRIMESTRE, name = "n_obs") %>%
  arrange(ANO4, TRIMESTRE) %>%
  print(n = 50)

print(summary(eph_base_labeled$LN_WAGE_HORA))
str(eph_base_labeled$ESTADO)
