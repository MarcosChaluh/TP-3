# Mapping helpers to keep label logic in a single place.

#' Return a named vector with the short labels for agglomerados
agglomerado_short_labels <- function() {
  c(
    "33" = "GBA",
    "32" = "CABA",
    "13" = "Gran Córdoba",
    "4"  = "Gran Rosario",
    "10" = "Gran Mendoza"
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
