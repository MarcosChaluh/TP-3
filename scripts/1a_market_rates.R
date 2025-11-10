source(file.path("R", "project_paths.R"))
source(file.path("R", "data_access.R"))
source(file.path("R", "market_indicators.R"))
source(file.path("R", "output_utils.R"))

library(dplyr)
library(ggplot2)
library(ggrepel)

paths <- ensure_project_structure()
eph_base_labeled <- load_processed_data("eph_individual_2017_2025_std.rds", paths)

provincias_destacadas <- c(
  "Ciudad Autónoma de Buenos Aires",
  "Buenos Aires",
  "Córdoba",
  "Santa Fe",
  "Mendoza"
)

tasas_mercado_laboral <- compute_market_rates(eph_base_labeled)
print(tasas_mercado_laboral, n = 5)

save_table(tasas_mercado_laboral, "tasas_mercado_laboral_provincias.csv", paths)

plot_data <- prepare_unemployment_plot_data(tasas_mercado_laboral, provincias_destacadas)
g <- plot_unemployment_series(plot_data)

save_plot(g, "desempleo_provincias.png", paths)
print(g)

heatmap_data <- prepare_unemployment_heatmap(tasas_mercado_laboral, 2025, 1)
g_heatmap <- plot_unemployment_heatmap(heatmap_data, 2025, 1)

save_plot(g_heatmap, "desempleo_heatmap_provincias.png", paths)
print(g_heatmap)
