source(file.path("R", "project_paths.R"))

#' Save a tibble/data frame inside the tables directory
save_table <- function(data, filename, paths = project_paths(), ...) {
  readr::write_csv(data, output_table_path(filename, paths), ...)
}

#' Save a ggplot object inside the plots directory
save_plot <- function(plot, filename, paths = project_paths(), width = 11, height = 7, dpi = 300, ...) {
  ggplot2::ggsave(output_plot_path(filename, paths), plot = plot, width = width, height = height, dpi = dpi, ...)
}
