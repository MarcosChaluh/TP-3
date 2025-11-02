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

paths <- ensure_project_structure()

eph <- load_eph_for_remote_work(paths)
cno_to_isco <- load_cno_to_isco(paths)
onet_crosswalk <- load_onet_crosswalk(paths)

eph_final <- create_remote_work_tables(eph, cno_to_isco, onet_crosswalk)

tabla_ocupacion <- summarise_by_occupation(eph_final)
print(tabla_ocupacion, n = 20)

save_table(tabla_ocupacion, "tabla_1_AI_WFH_por_Ocupacion.csv", paths)

tabla_aglomerado <- summarise_by_agglomerado(eph_final)
print(tabla_aglomerado, n = 20)

save_table(tabla_aglomerado, "tabla_2_AI_WFH_por_Aglomerado.csv", paths)
