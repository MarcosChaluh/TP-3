# Mapping helpers to keep label logic in a single place.

#' Return a named vector with the short labels for agglomerados
#' Retorna un vector nombrado con etiquetas cortas para los 31 aglomerados de la EPH.
agglomerado_short_labels <- function() {
  c(
    "2"  = "Gran La Plata",
    "3"  = "Bahía Blanca",
    "4"  = "Gran Rosario",
    "5"  = "Gran Santa Fe",
    "6"  = "Gran Paraná",
    "7"  = "Posadas",
    "8"  = "Gran Resistencia",
    "9"  = "Com. Rivadavia",
    "10" = "Gran Mendoza",
    "12" = "Corrientes",
    "13" = "Gran Córdoba",
    "14" = "Concordia",
    "15" = "Formosa",
    "17" = "Neuquén-Plottier",
    "18" = "S. del Estero",
    "19" = "Jujuy-Palpalá",
    "20" = "Río Gallegos",
    "22" = "Gran Catamarca",
    "23" = "Salta",
    "25" = "La Rioja",
    "26" = "San Luis",
    "27" = "Gran San Juan",
    "29" = "Gran Tucumán",
    "30" = "Santa Rosa",
    "31" = "Ushuaia-R.Gde",
    "32" = "CABA",
    "33" = "GBA",
    "34" = "Mar del Plata",
    "36" = "Río Cuarto",
    "38" = "San Nicolás",
    "91" = "Rawson-Trelew",
    "93" = "Viedma"
  )
}

#' Return a named vector mapping agglomerados to provinces
agglomerado_to_province <- function() {
  c(
    "2" = "Buenos Aires",
    "3" = "Buenos Aires",
    "4" = "Santa Fe",
    "5" = "Santa Fe",
    "6" = "Entre Ríos",
    "7" = "Misiones",
    "8" = "Chaco",
    "9" = "Chubut",
    "10" = "Mendoza",
    "12" = "Corrientes",
    "13" = "Córdoba",
    "14" = "Entre Ríos",
    "15" = "Formosa",
    "17" = "Neuquén",
    "18" = "Santiago del Estero",
    "19" = "Jujuy",
    "20" = "Santa Cruz",
    "22" = "Catamarca",
    "23" = "Salta",
    "25" = "La Rioja",
    "26" = "San Luis",
    "27" = "San Juan",
    "29" = "Tucumán",
    "30" = "La Pampa",
    "31" = "Tierra del Fuego",
    "32" = "Ciudad Autónoma de Buenos Aires",
    "33" = "Buenos Aires",
    "34" = "Buenos Aires",
    "36" = "Córdoba",
    "38" = "Buenos Aires",
    "91" = "Chubut",
    "93" = "Río Negro"
  )
}

#' Return short labels for provinces
province_short_labels <- function() {
  c(
    "Buenos Aires" = "Buenos Aires",
    "Catamarca" = "Catamarca",
    "Chaco" = "Chaco",
    "Chubut" = "Chubut",
    "Ciudad Autónoma de Buenos Aires" = "CABA",
    "Corrientes" = "Corrientes",
    "Córdoba" = "Córdoba",
    "Entre Ríos" = "Entre Ríos",
    "Formosa" = "Formosa",
    "Jujuy" = "Jujuy",
    "La Pampa" = "La Pampa",
    "La Rioja" = "La Rioja",
    "Mendoza" = "Mendoza",
    "Misiones" = "Misiones",
    "Neuquén" = "Neuquén",
    "Río Negro" = "Río Negro",
    "Salta" = "Salta",
    "San Juan" = "San Juan",
    "San Luis" = "San Luis",
    "Santa Cruz" = "Santa Cruz",
    "Santa Fe" = "Santa Fe",
    "Santiago del Estero" = "Santiago del Estero",
    "Tierra del Fuego" = "Tierra del Fuego",
    "Tucumán" = "Tucumán"
  )
}

#' Attach a `label_corta` column with the agglomerado nickname
with_agglomerado_labels <- function(data, agglomerado_col = "AGLOMERADO", label_col = "label_corta") {
  labels <- agglomerado_short_labels()
  aglo_values <- data[[agglomerado_col]]
  aglo_num <- suppressWarnings(as.numeric(aglo_values))
  resolved <- labels[as.character(aglo_num)]
  resolved[is.na(resolved)] <- as.character(aglo_num[is.na(resolved)])
  data[[label_col]] <- resolved
  data
}

#' Attach province labels using the province column
with_province_labels <- function(data, province_col = "PROVINCIA", label_col = "label_provincia") {
  labels <- province_short_labels()
  province_values <- data[[province_col]]
  resolved <- labels[province_values]
  resolved[is.na(resolved)] <- province_values[is.na(resolved)]
  data[[label_col]] <- resolved
  data
}

#' Derive provinces from agglomerado codes and attach labels
add_province_from_agglomerado <- function(data, agglomerado_col = "AGLOMERADO", province_col = "PROVINCIA", label_col = "label_provincia") {
  mapping <- agglomerado_to_province()
  aglo_values <- data[[agglomerado_col]]
  aglo_num <- suppressWarnings(as.numeric(aglo_values))
  province <- mapping[as.character(aglo_num)]
  data[[province_col]] <- unname(province)
  data <- with_province_labels(data, province_col = province_col, label_col = label_col)
  data
}

#' Return region labels as a named vector
region_labels <- function() {
  c(
    "1" = "GBA",
    "40" = "Noroeste (NOA)",
    "41" = "Noreste (NEA)",
    "42" = "Cuyo",
    "43" = "Pampeana",
    "44" = "Patagonia"
  )
}

#' Attach region labels to a data frame
with_region_labels <- function(data, region_col = "REGION", label_col = "label_region") {
  labels <- region_labels()
  region_values <- data[[region_col]]
  region_num <- suppressWarnings(as.numeric(region_values))
  resolved <- labels[as.character(region_num)]
  resolved[is.na(resolved)] <- as.character(region_num[is.na(resolved)])
  data[[label_col]] <- resolved
  data
}
