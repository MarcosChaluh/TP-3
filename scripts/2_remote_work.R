source(file.path("R", "project_paths.R"))
source(file.path("R", "remote_work.R"))
source(file.path("R", "output_utils.R"))

library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(janitor)
library(tibble)
library(expss)
library(ggplot2)

paths <- ensure_project_structure()

eph <- load_eph_for_remote_work(paths)
cno_to_isco <- load_cno_to_isco(paths)
onet_crosswalk <- load_onet_crosswalk(paths)

eph_final <- create_remote_work_tables(eph, cno_to_isco, onet_crosswalk)

tabla_ocupacion <- summarise_by_occupation(eph_final)
print(tabla_ocupacion, n = 20)

save_table(tabla_ocupacion, "tabla_1_AI_WFH_por_Ocupacion.csv", paths)

tabla_provincia <- summarise_by_province(eph_final)
print(tabla_provincia, n = 24)

save_table(tabla_provincia, "tabla_2_AI_WFH_por_Provincia.csv", paths)

heatmap_data <- prepare_remote_work_heatmap(tabla_provincia)
g_heatmap <- plot_remote_work_heatmap(heatmap_data)

save_plot(g_heatmap, "remoto_heatmap_provincias.png", paths, width = 9, height = 8)
print(g_heatmap)

tabla_industria <- summarise_by_industry(eph_final)
print(tabla_industria, n = 20)

save_table(tabla_industria, "tabla_3_AI_WFH_por_Industria.csv", paths)
