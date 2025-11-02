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

paths <- ensure_project_structure()
eph_base_labeled <- load_processed_data("eph_individual_2017_2025_std.rds", paths)

mincer_data <- prepare_mincer_data(eph_base_labeled)
coeficientes_mincer <- estimate_mincer_by_aglomerado(mincer_data)
print(coeficientes_mincer, n = 50)

save_table(coeficientes_mincer, "mincer_coeficientes.csv", paths)

g_mincer_coefs <- plot_mincer_returns(coeficientes_mincer)

save_plot(g_mincer_coefs, "mincer_retorno_educacion.png", paths, width = 11, height = 8)
print(g_mincer_coefs)
