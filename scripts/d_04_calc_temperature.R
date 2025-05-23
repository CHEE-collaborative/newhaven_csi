################################################################################
# Calculate temperature statistics for census block groups.

################################################################################
# Source variables from a_00_initiate.R
source(file.path(here::here(), "scripts", "a_00_initiate.R"))

################################################################################
# Import CSI polygons data.
chr_sf_csi_path <- file.path(dir_output, "c_01", "sf_csi_polygons.rds")
testthat::expect_true(file.exists(chr_sf_csi_path))
sf_csi_polygons <- readRDS(chr_sf_csi_path)
