################################################################################
# Merge all Community Serverance Index (CSI) data with environmental and
# demographic conditions data.

################################################################################
# Source variables from a_00_initiate.
source(file.path(here::here(), "scripts", "a_00_initiate.R"))

################################################################################
# Import CSI polygons data.
chr_sf_csi_path <- file.path(dir_output, "c_01", "sf_csi_polygons.rds")
testthat::expect_true(file.exists(chr_sf_csi_path))
sf_csi_polygons <- readRDS(chr_sf_csi_path)
sf_csi <- sf_csi_polygons[,
  c("GEOID20", "csi", "csi_normal", "csi_100", "MR1", "MR2")
]

################################################################################
# List output files.
chr_output_files <- list.files(dir_output, full.names = TRUE, recursive = TRUE)
chr_output_files <- grep("d_\\d{2}", chr_output_files, value = TRUE)

################################################################################
# Import all output (`.rds`) files.
for (c in seq_along(chr_output_files)) {
  chr_split <- strsplit(chr_output_files[c], "/")[[1]]
  chr_object <- gsub(".rds", "", chr_split[length(chr_split)])
  assign(chr_object, readRDS(chr_output_files[c]))
}

################################################################################
# Merge CSI data with environmental and demographic conditions.
sf_csi_cond <- dplyr::left_join(
  sf_csi,
  sf::st_drop_geometry(sf_tree),
  by = "GEOID20"
) %>%
  dplyr::left_join(sf::st_drop_geometry(sf_no2_proj), by = "GEOID20") %>%
  dplyr::left_join(sf::st_drop_geometry(sf_pm25_proj), by = "GEOID20") %>%
  dplyr::left_join(sf::st_drop_geometry(sf_cdd), by = "GEOID20") %>%
  dplyr::left_join(sf::st_drop_geometry(sf_demographic_nh), by = "GEOID20")

################################################################################
# Save output.
chr_csi_cond_path <- file.path(dir_output, "d_06", "sf_csi_cond.rds")
saveRDS(sf_csi_cond, chr_csi_cond_path)
