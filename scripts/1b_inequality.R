source(file.path("R", "project_paths.R"))
source(file.path("R", "data_access.R"))
source(file.path("R", "inequality_metrics.R"))
source(file.path("R", "output_utils.R"))

library(dplyr)
library(ggplot2)
library(reldist)
library(scales)

paths <- ensure_project_structure()
eph_base_labeled <- load_processed_data("eph_individual_2017_2025_std.rds", paths)

calculos_desigualdad <- compute_income_inequality(eph_base_labeled)

desigualdad_reciente <- calculos_desigualdad %>%
  filter(ANO4 == 2025, TRIMESTRE == 1)

print(desigualdad_reciente, n = 24)

save_table(desigualdad_reciente, "desigualdad_provincias_2025T1.csv", paths)

snapshot_data <- prepare_inequality_snapshot(calculos_desigualdad, 2025, 1)
g_snapshot <- plot_inequality_snapshot(snapshot_data, 2025, 1)

save_plot(g_snapshot, "desigualdad_rankings_2025T1.png", paths, width = 11, height = 8)
print(g_snapshot)
