source(file.path("R", "project_paths.R"))
source(file.path("R", "data_access.R"))
source(file.path("R", "mincer_analysis.R"))
source(file.path("R", "output_utils.R"))

library(dplyr)
library(expss)
library(broom)
library(ggplot2)
library(purrr)
library(tidyr)

ANO_RECIENTE <- 2025
TRIM_RECIENTE <- 1

paths <- ensure_project_structure()
eph_base_labeled <- load_processed_data("eph_individual_2017_2025_std.rds", paths)

mincer_data <- prepare_mincer_data(eph_base_labeled)
coeficientes_mincer <- estimate_mincer_by_province(mincer_data)

save_table(coeficientes_mincer, "mincer_coeficientes_provincias.csv", paths)

coeficientes_recientes <- coeficientes_mincer %>%
  filter(ANO4 == ANO_RECIENTE, TRIMESTRE == TRIM_RECIENTE)

print(coeficientes_recientes, n = 50)

tabla_snapshot <- extract_mincer_snapshot(coeficientes_mincer, ANO_RECIENTE, TRIM_RECIENTE)
print(tabla_snapshot, n = 50)

save_table(tabla_snapshot,
           sprintf("mincer_coeficientes_provincias_%dT%d.csv", ANO_RECIENTE, TRIM_RECIENTE),
           paths)

g_mincer_coefs <- plot_mincer_returns(coeficientes_recientes)

save_plot(g_mincer_coefs, "mincer_retorno_educacion_provincias.png", paths, width = 11, height = 8)
print(g_mincer_coefs)

map_data <- prepare_mincer_map(coeficientes_mincer, ANO_RECIENTE, TRIM_RECIENTE)
g_mincer_map <- plot_mincer_map(map_data, ANO_RECIENTE, TRIM_RECIENTE)

save_plot(g_mincer_map, "mincer_map_provincias.png", paths, width = 8, height = 8)
print(g_mincer_map)
