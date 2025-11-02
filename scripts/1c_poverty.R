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

tasas_pobreza <- aggregate_poverty_by_region(base_con_pobreza)
print(tasas_pobreza, n = 40)

save_table(tasas_pobreza, "tasas_pobreza_region.csv", paths)

plot_data_pobreza <- prepare_poverty_plot_data(tasas_pobreza)
g_pobreza_region <- plot_poverty_series(plot_data_pobreza)

save_plot(g_pobreza_region, "pobreza_por_region.png", paths)
print(g_pobreza_region)
