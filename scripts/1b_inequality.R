source(file.path("R", "project_paths.R"))
source(file.path("R", "data_access.R"))
source(file.path("R", "inequality_metrics.R"))
source(file.path("R", "output_utils.R"))

library(dplyr)
library(ggplot2)
library(ggrepel)
library(reldist)
library(scales)

paths <- ensure_project_structure()
eph_base_labeled <- load_processed_data("eph_individual_2017_2025_std.rds", paths)

provincias_destacadas <- c(
  "Ciudad Autónoma de Buenos Aires",
  "Buenos Aires",
  "Córdoba",
  "Santa Fe",
  "Mendoza"
)

calculos_desigualdad <- compute_income_inequality(eph_base_labeled)
print(calculos_desigualdad, n = 40)

save_table(calculos_desigualdad, "desigualdad_provincias.csv", paths)

plot_data_gini <- prepare_gini_plot_data(calculos_desigualdad, provincias_destacadas)
g_gini <- plot_gini_series(plot_data_gini)

save_plot(g_gini, "desigualdad_gini_series_provincias.png", paths)
print(g_gini)

map_data <- prepare_inequality_map(calculos_desigualdad, 2025, 1)
g_map <- plot_inequality_map(map_data, 2025, 1)

save_plot(g_map, "desigualdad_map_provincias.png", paths, width = 10, height = 6)
print(g_map)
