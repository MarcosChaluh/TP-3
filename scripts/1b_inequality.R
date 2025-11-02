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

aglomerados_grandes_cods <- c(32, 33, 13, 4, 10)

calculos_desigualdad <- compute_income_inequality(eph_base_labeled)
print(calculos_desigualdad, n = 40)

save_table(calculos_desigualdad, "desigualdad_aglomerados.csv", paths)

plot_data_gini <- prepare_gini_plot_data(calculos_desigualdad, aglomerados_grandes_cods)
g_gini <- plot_gini_series(plot_data_gini)

save_plot(g_gini, "desigualdad_gini_series.png", paths)
print(g_gini)
