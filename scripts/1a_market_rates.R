source(file.path("R", "project_paths.R"))
source(file.path("R", "data_access.R"))
source(file.path("R", "market_indicators.R"))
source(file.path("R", "output_utils.R"))

library(dplyr)
library(ggplot2)
library(ggrepel)
library(scales)

paths <- ensure_project_structure()
eph_base_labeled <- load_processed_data("eph_individual_2017_2025_std.rds", paths)

tasas_mercado_laboral <- compute_market_rates(eph_base_labeled)

tasas_recientes <- tasas_mercado_laboral %>%
  filter(ANO4 == 2025, TRIMESTRE == 1)

print(tasas_recientes, n = 10)

save_table(tasas_recientes, "tasas_mercado_laboral_provincias_2025T1.csv", paths)

snapshot_data <- prepare_unemployment_snapshot(tasas_mercado_laboral, 2025, 1)
g_snapshot <- plot_unemployment_snapshot(snapshot_data, 2025, 1)

save_plot(g_snapshot, "mercado_laboral_provincias_2025T1.png", paths, width = 11, height = 8)
print(g_snapshot)

serie_nacional <- compute_national_market_rates(tasas_mercado_laboral)
print(serie_nacional, n = 12)

save_table(serie_nacional, "tasa_desempleo_nacional_trimestral.csv", paths)

g_nacional <- plot_national_unemployment_series(serie_nacional)

save_plot(g_nacional, "desempleo_nacional_serie.png", paths, width = 10, height = 6)
print(g_nacional)
