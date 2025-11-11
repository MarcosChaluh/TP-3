source(file.path("R", "project_paths.R"))
source(file.path("R", "data_access.R"))
source(file.path("R", "structural_indicators.R"))
source(file.path("R", "output_utils.R"))

library(dplyr)
library(expss)
library(ggplot2)
library(tidyr)

paths <- ensure_project_structure()
eph_base_labeled <- load_processed_data("eph_individual_2017_2025_std.rds", paths)

ANO_RECIENTE <- 2025
TRIM_RECIENTE <- 1

informalidad <- compute_informality_rate(eph_base_labeled, ANO_RECIENTE, TRIM_RECIENTE)
print(informalidad, n = 10)

save_table(informalidad, sprintf("indicador_informalidad_provincias_%dT%d.csv", ANO_RECIENTE, TRIM_RECIENTE), paths)

g_informalidad <- plot_ranked_bars(
  informalidad,
  "tasa_no_registro_pct",
  "Tasa de Asalariados No Registrados (Informalidad)",
  sprintf("Porcentaje de asalariados sin descuento jubilatorio %dT%d", ANO_RECIENTE, TRIM_RECIENTE),
  "Tasa de No Registro (%)",
  label_col = "label_provincia",
  x_label = "Provincia",
  fill_color = "#f16913"
)

save_plot(g_informalidad, "informalidad_provincias.png", paths, width = 11, height = 8)
print(g_informalidad)

brecha_genero <- compute_activity_gender_gap(eph_base_labeled, ANO_RECIENTE, TRIM_RECIENTE)
print(brecha_genero, n = 10)

save_table(brecha_genero, sprintf("indicador_brecha_genero_provincias_%dT%d.csv", ANO_RECIENTE, TRIM_RECIENTE), paths)

g_brecha_genero <- plot_ranked_bars(
  brecha_genero,
  "brecha_actividad_pp",
  "Brecha de Género en Tasa de Actividad",
  sprintf("Diferencia en puntos porcentuales (Hombres - Mujeres) %dT%d", ANO_RECIENTE, TRIM_RECIENTE),
  "Brecha de Actividad (p.p.)",
  label_col = "label_provincia",
  x_label = "Provincia",
  fill_color = "#756bb1"
)

save_plot(g_brecha_genero, "brecha_genero_actividad_provincias.png", paths, width = 11, height = 8)
print(g_brecha_genero)

cuentapropismo <- compute_self_employment_rate(eph_base_labeled, ANO_RECIENTE, TRIM_RECIENTE)
print(cuentapropismo, n = 10)

save_table(cuentapropismo, sprintf("indicador_cuentapropismo_provincias_%dT%d.csv", ANO_RECIENTE, TRIM_RECIENTE), paths)

g_cuentapropismo <- plot_ranked_bars(
  cuentapropismo,
  "tasa_cuentaprop_pct",
  "Tasa de Cuentapropismo",
  sprintf("Porcentaje de ocupados que trabajan por su cuenta %dT%d", ANO_RECIENTE, TRIM_RECIENTE),
  "Tasa de Cuentapropismo (%)",
  label_col = "label_provincia",
  x_label = "Provincia",
  fill_color = "#31a354"
)

save_plot(g_cuentapropismo, "cuentapropismo_provincias.png", paths, width = 11, height = 8)
print(g_cuentapropismo)

resumen_indicadores <- summarise_structural_indicators(informalidad, brecha_genero, cuentapropismo)
print(resumen_indicadores, n = 15)

save_table(resumen_indicadores,
           sprintf("indicadores_estructurales_resumen_%dT%d.csv", ANO_RECIENTE, TRIM_RECIENTE),
           paths)
