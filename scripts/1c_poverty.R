source(file.path("R", "project_paths.R"))
source(file.path("R", "data_access.R"))
source(file.path("R", "poverty_metrics.R"))
source(file.path("R", "output_utils.R"))

library(dplyr)
library(ggplot2)
library(ggrepel)
library(eph)

paths <- ensure_project_structure()
eph_base_numeric <- load_processed_data("eph_individual_2017_2025.rds", paths)

canasta <- get_poverty_lines(regional = TRUE)

base_con_pobreza <- calculate_poverty_status(eph_base_numeric, canasta)

tasas_pobreza <- aggregate_poverty_by_province(base_con_pobreza)
print(tasas_pobreza, n = 40)

save_table(tasas_pobreza, "tasas_pobreza_provincias.csv", paths)

provincias_destacadas <- c(
  "Ciudad Autónoma de Buenos Aires",
  "Buenos Aires",
  "Córdoba",
  "Santa Fe",
  "Mendoza"
)

plot_data_pobreza <- prepare_poverty_plot_data(tasas_pobreza, provincias_destacadas)
g_pobreza_region <- plot_poverty_series(plot_data_pobreza)

save_plot(g_pobreza_region, "pobreza_por_provincia.png", paths)
print(g_pobreza_region)

map_data <- prepare_poverty_map(tasas_pobreza, 2025, 1)
g_map <- plot_poverty_map(map_data, 2025, 1)

save_plot(g_map, "pobreza_map_provincias.png", paths, width = 10, height = 6)
print(g_map)
