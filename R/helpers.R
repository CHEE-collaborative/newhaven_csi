# nolint start
#' Generate objects for root, data, input, and output directories.
set_directories <- function(root = here::here()) {
  assign("dir_home", file.path(root), envir = .GlobalEnv)
  assign("dir_data", file.path(dir_home, "data"), envir = .GlobalEnv)
  assign("dir_input", file.path(dir_data, "input"), envir = .GlobalEnv)
  assign("dir_output", file.path(dir_data, "output"), envir = .GlobalEnv)
  assign(
    "chr_towns_path",
    file.path(
      dir_input, "CT_Vicinity_Town_Polygon", "CT_Vicinity_Town_Polygon.shp"
    ),
    envir = .GlobalEnv
  )
}

#' Generate New Haven boundary data from path to neighborhoods shapefile.
generate_newhaven <- function(path, crs = 2234) {
  # Check file exists
  testthat::expect_true(file.exists(path))
  # Import shapefile and project to Connecticut crs.
  sf_towns <- sf::st_read(path) %>%
    sf::st_transform(crs)
  # Select new haven
  sf_newhaven <- sf_towns[
    which(sf_towns$TOWN_NAME == "New Haven" & sf_towns$LABEL_FLAG == "True"),
    "geometry"
  ]
  return(sf_newhaven)
}

#' Apply min-max normalization.
normalize <- function(x) {
  xrange <- range(x, na.rm = TRUE)
  return((x - xrange[1]) / (xrange[2] - xrange[1]))
}
# nolint end
