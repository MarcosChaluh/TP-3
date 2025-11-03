source(file.path("R", "project_paths.R"))
source(file.path("R", "data_access.R"))
source(file.path("R", "market_indicators.R"))
source(file.path("R", "output_utils.R"))

library(dplyr)
library(ggplot2)
library(ggrepel)

paths <- ensure_project_structure()
eph_base_labeled <- load_processed_data("eph_individual_2017_2025_std.rds", paths)

aglomerados_grandes_cods <- c(32, 33, 13, 4, 10)

tasas_mercado_laboral <- compute_market_rates(eph_base_labeled)
print(tasas_mercado_laboral, n = 5)

save_table(tasas_mercado_laboral, "tasas_mercado_laboral.csv", paths)

plot_data <- prepare_unemployment_plot_data(tasas_mercado_laboral, aglomerados_grandes_cods)
g <- plot_unemployment_series(plot_data)

save_plot(g, "desempleo_aglomerados.png", paths)
print(g)
