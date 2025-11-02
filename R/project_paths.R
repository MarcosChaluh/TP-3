# Utility helpers to work with the project directory structure.

#' Project directory paths
#'
#' Returns the canonical locations used across the project. The helper accepts
#' an optional `root` parameter so scripts executed from subdirectories can still
#' resolve the folders correctly.
project_paths <- function(root = ".") {
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  list(
    root = root,
    data = list(
      raw = file.path(root, "data", "raw"),
      processed = file.path(root, "data", "processed")
    ),
    outputs = list(
      plots = file.path(root, "outputs", "plots"),
      tables = file.path(root, "outputs", "tables")
    ),
    scripts = file.path(root, "scripts"),
    r = file.path(root, "R")
  )
}

#' Ensure the project folder structure exists
#'
#' This helper is idempotent and will silently create any missing directories.
#' It returns the same structure as `project_paths()` so that callers can chain
#' both operations in a single call.
ensure_project_structure <- function(paths = project_paths()) {
  dir.create(paths$data$raw, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$data$processed, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$outputs$plots, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$outputs$tables, recursive = TRUE, showWarnings = FALSE)
  invisible(paths)
}

#' Helper to build a path inside the processed data directory
processed_data_path <- function(filename, paths = project_paths()) {
  file.path(paths$data$processed, filename)
}

#' Helper to build a path inside the raw data directory
raw_data_path <- function(filename, paths = project_paths()) {
  file.path(paths$data$raw, filename)
}

#' Helper to build a path inside the tables output directory
output_table_path <- function(filename, paths = project_paths()) {
  file.path(paths$outputs$tables, filename)
}

#' Helper to build a path inside the plots output directory
output_plot_path <- function(filename, paths = project_paths()) {
  file.path(paths$outputs$plots, filename)
}
