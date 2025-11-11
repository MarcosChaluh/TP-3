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

#' Normalize agglomerado identifiers for reliable comparisons
normalize_agglomerado_key <- function(values) {
  if (is.null(values)) {
    return(character())
  }

  keys <- as.character(values)
  keys[keys %in% c("", "NA", "NaN")] <- NA_character_
  normalized <- iconv(keys, from = "", to = "ASCII//TRANSLIT")
  normalized <- gsub("[^[:alnum:]]+", " ", normalized)
  normalized <- trimws(gsub("\\s+", " ", normalized))
  normalized <- tolower(normalized)
  normalized[normalized == ""] <- NA_character_
  normalized
}

#' Return province shares for agglomerados spanning multiple provinces
#'
#' Shares approximate the population split of each multi-provincial aglomerado
#' so survey weights can be allocated proportionally when data are expanded to
#' the provincial level.
multi_province_agglomerado_shares <- function() {
  data.frame(
    agglomerado_key = c(
      rep(c("38", "San Nicolas - Villa Constitucion", "San Nicolás - Villa Constitución"), each = 2),
      rep(c("93", "Viedma - Carmen de Patagones", "Viedma - Carmen de Patagones."), each = 2)
    ),
    PROVINCIA = c(
      rep(c("Buenos Aires", "Santa Fe"), times = 3),
      rep(c("Buenos Aires", "Río Negro"), times = 3)
    ),
    share = c(
      rep(c(0.64, 0.36), times = 3),
      rep(c(0.30, 0.70), times = 3)
    ),
    stringsAsFactors = FALSE
  )
}

#' Return identifiers for agglomerados spanning multiple provinces
multi_province_agglomerados <- function() {
  unique(multi_province_agglomerado_shares()$agglomerado_key)
}

#' Return a named vector mapping agglomerados to provinces
agglomerado_to_province <- function() {
  base_mapping <- c(
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
    "91" = "Chubut"
  )

  name_mapping <- c(
    "Gran La Plata" = "Buenos Aires",
    "Bahia Blanca - Cerri" = "Buenos Aires",
    "Bahía Blanca - Cerri" = "Buenos Aires",
    "Gran Rosario" = "Santa Fe",
    "Gran Santa Fe" = "Santa Fe",
    "Gran Parana" = "Entre Ríos",
    "Gran Paraná" = "Entre Ríos",
    "Posadas" = "Misiones",
    "Gran Resistencia" = "Chaco",
    "Cdro. Rivadavia - R.Tilly" = "Chubut",
    "Gran Mendoza" = "Mendoza",
    "Corrientes" = "Corrientes",
    "Gran Cordoba" = "Córdoba",
    "Gran Córdoba" = "Córdoba",
    "Concordia" = "Entre Ríos",
    "Formosa" = "Formosa",
    "Neuquen - Plottier" = "Neuquén",
    "Neuquén - Plottier" = "Neuquén",
    "S. del Estero - La Banda" = "Santiago del Estero",
    "Jujuy - Palpala" = "Jujuy",
    "Jujuy - Palpalá" = "Jujuy",
    "Rio Gallegos" = "Santa Cruz",
    "Río Gallegos" = "Santa Cruz",
    "Gran Catamarca" = "Catamarca",
    "Salta" = "Salta",
    "La Rioja" = "La Rioja",
    "San Luis - El Chorrillo" = "San Luis",
    "Gran San Juan" = "San Juan",
    "Gran Tucuman - T. Viejo" = "Tucumán",
    "Gran Tucumán - T. Viejo" = "Tucumán",
    "Santa Rosa - Toay" = "La Pampa",
    "Ushuaia - Rio Grande" = "Tierra del Fuego",
    "Ushuaia - Río Grande" = "Tierra del Fuego",
    "Ciudad de Buenos Aires" = "Ciudad Autónoma de Buenos Aires",
    "Partidos del GBA" = "Buenos Aires",
    "Mar del Plata - Batan" = "Buenos Aires",
    "Mar del Plata - Batán" = "Buenos Aires",
    "Rio Cuarto" = "Córdoba",
    "Río Cuarto" = "Córdoba",
    "Rawson - Trelew" = "Chubut"
  )

  mapping <- c(base_mapping, name_mapping)
  multi <- multi_province_agglomerados()
  multi <- multi[multi %in% names(mapping)]
  if (length(multi) > 0) {
    mapping[multi] <- NA_character_
  }
  mapping
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
  missing <- is.na(resolved) & !is.na(aglo_values)
  if (any(missing)) {
    resolved[missing] <- as.character(aglo_values[missing])
  }
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
add_province_from_agglomerado <- function(
    data,
    agglomerado_col = "AGLOMERADO",
    province_col = "PROVINCIA",
    label_col = "label_provincia"
) {
  mapping <- agglomerado_to_province()
  aglo_values <- data[[agglomerado_col]]
  aglo_num <- suppressWarnings(as.numeric(aglo_values))
  province <- mapping[as.character(aglo_num)]

  missing <- is.na(province) & !is.na(aglo_values)
  if (any(missing)) {
    province[missing] <- mapping[as.character(aglo_values[missing])]
  }

  missing <- is.na(province) & !is.na(aglo_values)
  if (any(missing)) {
    mapping_lower <- mapping
    names(mapping_lower) <- tolower(names(mapping_lower))
    province[missing] <- mapping_lower[tolower(as.character(aglo_values[missing]))]
  }

  data[[province_col]] <- unname(province)
  shares_lookup <- multi_province_agglomerado_shares()
  if (nrow(shares_lookup) > 0) {
    shares_lookup$key_norm <- normalize_agglomerado_key(shares_lookup$agglomerado_key)
    shares_lookup <- shares_lookup[!is.na(shares_lookup$key_norm), c("key_norm", "PROVINCIA", "share")]

    match_by_keys <- function(values) {
      keys_norm <- normalize_agglomerado_key(values)
      valid <- which(!is.na(keys_norm))
      if (length(valid) == 0) {
        return(data.frame(row_id = integer(), key_norm = character(), PROVINCIA = character(), share = numeric(), stringsAsFactors = FALSE))
      }

      dplyr::left_join(
        data.frame(row_id = valid, key_norm = keys_norm[valid], stringsAsFactors = FALSE),
        shares_lookup,
        by = "key_norm"
      )
    }

    matches <- dplyr::bind_rows(
      match_by_keys(as.character(aglo_values)),
      match_by_keys(as.character(aglo_num))
    )

    if (nrow(matches) > 0) {
      matches <- matches[!is.na(matches$PROVINCIA) & !is.na(matches$share), , drop = FALSE]
      if (nrow(matches) > 0) {
        matches <- matches[!duplicated(matches[c("row_id", "PROVINCIA")]), , drop = FALSE]

        data[["..row_id"]] <- seq_len(nrow(data))

        multi_rows <- dplyr::semi_join(data, matches, by = c("..row_id" = "row_id"))
        other_rows <- dplyr::anti_join(data, matches, by = c("..row_id" = "row_id"))

        expanded <- dplyr::left_join(matches, multi_rows, by = c("row_id" = "..row_id"))
        expanded[[province_col]] <- expanded$PROVINCIA

        weight_cols <- intersect(c("PONDERA", "PONDIH", "PONDIIO", "PONDII"), names(expanded))
        if (length(weight_cols) > 0) {
          for (col in weight_cols) {
            expanded[[col]] <- expanded[[col]] * expanded$share
          }
        }

        drop_cols <- c("row_id", "PROVINCIA", "share", "key_norm")
        drop_cols <- drop_cols[drop_cols %in% names(expanded)]
        if (length(drop_cols) > 0) {
          expanded <- expanded[, setdiff(names(expanded), drop_cols), drop = FALSE]
        }

        if ("..row_id" %in% names(other_rows)) {
          other_rows <- other_rows[, setdiff(names(other_rows), "..row_id"), drop = FALSE]
        }
        if ("..row_id" %in% names(expanded)) {
          expanded <- expanded[, setdiff(names(expanded), "..row_id"), drop = FALSE]
        }

        data <- dplyr::bind_rows(other_rows, expanded)
      }
    }
  }

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
