################################################################################
# Merge census block group CSI values with environmental conditions data.

################################################################################
# Source variables from a_00_initiate.R
source(file.path(here::here(), "scripts", "a_00_initiate.R"))

################################################################################
# Import CSI polygons data.
chr_sf_csi_path <- file.path(dir_output, "c_01", "sf_csi_polygons.rds")
testthat::expect_true(file.exists(chr_sf_csi_path))
sf_csi_polygons <- readRDS(chr_sf_csi_path)

################################################################################
# Import "greenness" data.
chr_green_path <- file.path(
  dir_input, "greenness", "New_Haven_census_RS_AC_veg_share.shp"
)
testthat::expect_true(file.exists(chr_green_path))
sf_green <- sf::read_sf(chr_green_path) %>%
  sf::st_transform(crs = int_crs_ct)

plot(sf::st_geometry(sf_green))

################################################################################
# Merge CSI and "greenness" data.
sf_csi_polygons$GEOID20 %in% sf_green$GEOID20
nchar(sf_csi_polygons$GEOID20[1])
nchar(sf_green$GEOID20[1])
plot(sf::st_geometry(sf_green), border = "red")
plot(sf::st_geometry(sf_csi_polygons), add = TRUE)

################################################################################
# Aggregate block level to block group level.
sf::st_intersection(
  sf::st_geometry(sf_csi_polygons),
  sf::st_geometry(sf_green)
)

sf_green$GEOID_bg <- substr(sf_green$GEOID20, 1, 12)
names(sf_green)

sf::st_intersection(
  sf_csi_polygons,
  sf_green
)

setdiff(unique(sf_green$GEOID_bg), sf_csi_polygons$GEOID20)


sf_debug <- sf_csi_polygons[
  sf_csi_polygons$GEOID20 %in% unique(sf_green$GEOID_bg),
]
plot(sf::st_geometry(sf_debug))



################################################################################
# [BUG] Generated "greenness" data appears to be census block level, not
#       census block groups.
################################################################################

################################################################################
# Import NO2 data.
chr_no2_path <- list.files(
  file.path(dir_input, "no2"),
  recursive = TRUE,
  full.names = TRUE,
  pattern = ".nc4"
)
testthat::expect_true(unique(sapply(chr_no2_path, file.exists)))
terra_no2 <- terra::rast()
for (n in seq_along(chr_no2_path)) {
  int_no2_year <- strsplit(chr_no2_path, "_")[[1]][11]
  terra_no2_n <- terra::rast(chr_no2_path[n])[[1]]
  terra::time(terra_no2_n) <- as.integer(int_no2_year)
  names(terra_no2_n) <- paste0(names(terra_no2_n), "_", int_no2_year)
  terra_no2_n_proj <- terra::project(terra_no2_n, paste0("EPSG:", int_crs_ct))
  terra_no2 <- c(terra_no2, terra_no2_n_proj)
}
testthat::expect_true(as.logical(grep("2234", terra::crs(terra_no2))))

################################################################################
# Calculate 5-year average NO2 values for census block groups.
terra_no2$mean <- mean(terra_no2)
sf_csi_polygons$mean_no2 <- exactextractr::exact_extract(
  terra_no2$mean,
  sf_csi_polygons,
  fun = "mean"
)
