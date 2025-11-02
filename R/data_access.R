source(file.path("R", "project_paths.R"))

#' Load a processed dataset
#'
#' @param filename Name of the RDS file stored under `data/processed`.
#' @param paths Output of `project_paths()` (optional).
load_processed_data <- function(filename, paths = project_paths()) {
  readr::read_rds(processed_data_path(filename, paths))
}

#' Load a raw dataset
#'
#' @param filename Name of the file located under `data/raw`.
load_raw_data <- function(filename, paths = project_paths()) {
  file.path(paths$data$raw, filename)
}
