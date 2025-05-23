################################################################################
# R settings, packages and reusable variables in the global environment.

################################################################################
# Generated variables
# dir_home - root working directory (character)
# dir_data - data directory (character)
# dir_input - data input directory (character)
# dir_output - data output directory (character)
# int_crs_ct - Connecticut coordinate reference system code (integer)
# chr_towns_path - Path to New Haven town boundary shapefile.

################################################################################
# Install and load runtime packages from CRAN.
chr_packages <- c(
  "dplyr", "sf", "testthat", "gstat", "tmap", "remotes", "here",
  "osmextract", "spdep", "progressr", "tidyr", "vtable", "GGally",
  "Matrix", "psych", "kableExtra", "sp", "terra", "raster", "plotly",
  "exactextractr", "tidycensus"
)

install_if_missing <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, repos = "https://cloud.r-project.org")
  }
  library(package, character.only = TRUE)
}

invisible(lapply(chr_packages, install_if_missing))

################################################################################
# Install GitHub packages.
chr_remotes <- c("yonghah/esri2sf", "Columbia-PRIME/pcpr")

for (r in seq_along(chr_remotes)) {
  chr_remote <- strsplit(chr_remotes[r], "/")[[1]][2]
  if (!requireNamespace(chr_remote)) {
    remotes::install_github(chr_remotes[r])
  }
  library(chr_remote, character.only = TRUE)
}

################################################################################
# Source functions.
dir_home <- here::here()
source(file.path(dir_home, "R", "helpers.R"))
source(file.path(dir_home, "R", "regrid_ok.R"))

################################################################################
# Set home, data, input, and output directories.
set_directories()

################################################################################
# Set Connecticut state plane coordinate reference system.
int_crs_ct <- 2234L
