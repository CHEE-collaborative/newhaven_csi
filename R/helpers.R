# nolint start object_usage_linter
#' Generate objects for root, data, input, and output directories.
set_directories <- function(root = here::here()) {
  assign("dir_home", file.path(root), envir = .GlobalEnv)
  assign("dir_data", file.path(dir_home, "data"), envir = .GlobalEnv)
  assign("dir_input", file.path(dir_data, "input"), envir = .GlobalEnv)
  assign("dir_output", file.path(dir_data, "output"), envir = .GlobalEnv)
}
# nolint end object_usage_linter
