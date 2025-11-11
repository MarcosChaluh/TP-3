source(file.path("R", "labels.R"))

ensure_map_dependencies <- function() {
  required <- c("sf", "rnaturalearth", "rnaturalearthdata")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(
      sprintf(
        "Los paquetes %s son necesarios para generar los mapas provinciales. \\nInstalalos antes de volver a ejecutar el script.",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

argentina_province_map <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) {
      return(cache)
    }

    ensure_map_dependencies()

    provinces_sf <- rnaturalearth::ne_states(country = "argentina", returnclass = "sf")

    canonical_names <- c(
      "Buenos Aires" = "Buenos Aires",
      "Catamarca" = "Catamarca",
      "Chaco" = "Chaco",
      "Chubut" = "Chubut",
      "Ciudad Autónoma de Buenos Aires" = "Ciudad Autónoma de Buenos Aires",
      "Ciudad de Buenos Aires" = "Ciudad Autónoma de Buenos Aires",
      "Distrito Federal" = "Ciudad Autónoma de Buenos Aires",
      "Corrientes" = "Corrientes",
      "Córdoba" = "Córdoba",
      "Cordoba" = "Córdoba",
      "Entre Ríos" = "Entre Ríos",
      "Entre Rios" = "Entre Ríos",
      "Formosa" = "Formosa",
      "Jujuy" = "Jujuy",
      "La Pampa" = "La Pampa",
      "La Rioja" = "La Rioja",
      "Mendoza" = "Mendoza",
      "Misiones" = "Misiones",
      "Neuquén" = "Neuquén",
      "Neuquen" = "Neuquén",
      "Río Negro" = "Río Negro",
      "Rio Negro" = "Río Negro",
      "Salta" = "Salta",
      "San Juan" = "San Juan",
      "San Luis" = "San Luis",
      "Santa Cruz" = "Santa Cruz",
      "Santa Fe" = "Santa Fe",
      "Santiago del Estero" = "Santiago del Estero",
      "Tierra del Fuego" = "Tierra del Fuego",
      "Tierra del Fuego, Antártida e Islas del Atlántico Sur" = "Tierra del Fuego",
      "Tucumán" = "Tucumán",
      "Tucuman" = "Tucumán"
    )

    provinces_sf <- provinces_sf %>%
      dplyr::mutate(
        PROVINCIA = canonical_names[name],
        PROVINCIA = dplyr::coalesce(PROVINCIA, canonical_names[name_es])
      ) %>%
      dplyr::filter(!is.na(PROVINCIA))

    labels <- province_short_labels()

    cache <<- provinces_sf %>%
      dplyr::mutate(
        map_label = labels[PROVINCIA],
        map_label = dplyr::coalesce(map_label, PROVINCIA)
      ) %>%
      dplyr::select(PROVINCIA, map_label, geometry)

    cache
  }
})

attach_province_geometry <- function(data) {
  map_sf <- argentina_province_map()

  if (!"PROVINCIA" %in% names(data)) {
    stop("El data frame debe incluir una columna PROVINCIA para vincular el mapa.", call. = FALSE)
  }

  if (!"label_provincia" %in% names(data)) {
    data <- with_province_labels(data)
  }

  data %>%
    dplyr::left_join(map_sf, by = "PROVINCIA") %>%
    dplyr::mutate(
      label_provincia = dplyr::coalesce(label_provincia, map_label)
    ) %>%
    dplyr::select(-map_label) %>%
    sf::st_as_sf()
}
