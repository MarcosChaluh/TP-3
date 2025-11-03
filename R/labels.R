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
