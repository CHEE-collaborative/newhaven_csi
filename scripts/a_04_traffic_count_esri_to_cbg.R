# ALINE: Changes in this script include:
# 1. nyc borough shapefile -> new haven neighborhood shapefile
# 2. nomenclature shifts: boroughs -> neighborhoods;
#    nyc -> newhaven; newyorkcity -> newhaven
# Interpolate traffic intensity data to census block groups

################################################################################
# Declare root directory, folder locations and load essential stuff
source(file.path(here::here(), "R/helpers.R"))
csi_directories()

################################################################################
# Set coordinate reference system to Connecticut state plane
# https://epsg.io/2234.
ct_crs <- 2234

################################################################################
# Import neighborhood boundary data.
neighborhoods_path <- file.path(
  dir_input,
  "NewHaven_NeighbhorhoodBoundaries",
  "NewHaven_NeighbhorhoodBoundaries.shp"
)
testthat::expect_true(file.exists(neighborhoods_path))
neighborhoods <- sf::st_read(neighborhoods_path) %>%
  sf::st_transform(ct_crs)

################################################################################
# Merge neighborhoods for New Haven boundary.
newhaven_boundaries <- sf::st_union(neighborhoods)
spatial_context <- newhaven_boundaries

################################################################################
# Import AADT data generated in a_03_prep_traffic.R
traffic_esri_path <- file.path(dir_output, "a_03", "traffic_counts_esri.rds")
testthat::expect_true(file.exists(traffic_esri_path))
traffic_esri <- readRDS(traffic_esri_path)

################################################################################
# Project AADT data to Connecticut coordinate reference system.
traffic_esri_proj <- sf::st_transform(traffic_esri, ct_crs)
colnames(traffic_esri_proj)[
  which(colnames(traffic_esri_proj) == "Traffic1")
] <- "aadt"

################################################################################
# Load Smart Location Database variables.
sld_us_loc_path <- file.path(
  dir_output, "a_01", "smart_location_data_subset.rds"
)
testthat::expect_true(file.exists(sld_us_loc_path))
sld_us_loc <- readRDS(sld_us_loc_path)
grid <- sf::st_centroid(sld_us_loc[, c("GEOID20")]) %>%
  sf::st_transform(ct_crs)

traffic_esri_id_cntxt <- sapply(
  sf::st_intersects(traffic_esri_proj, spatial_context),
  function(x) length(x) > 0
)
traffic_esri_cntxt <- traffic_esri_proj[traffic_esri_id_cntxt, ]

grid_id_cntxt <- sapply(
  sf::st_intersects(grid, spatial_context),
  function(x) length(x) > 0
)
grid_contxt <- grid[grid_id_cntxt, ]

################################################################################
# Apply regrid_ok function
# From https://github.com/jaime-benavides/community_severance_nyc/blob/main/code/functions/functions.R
source(file.path(dir_home, "R", "regrid_ok.R"))
uk_mean_uniform_ok <- regrid_ok(
  non_uniform_data = sf::as_Spatial(traffic_esri_cntxt), # traffic_esri_cntxt
  target_grid = sf::as_Spatial(grid_contxt),
  crs_sim = ct_crs
) # grid_contxt
colnames(uk_mean_uniform_ok)[1] <- "aadt"
uk_mean_uniform_ok$GEOID20 <- grid_contxt$GEOID20

################################################################################
# Save output.
uk_mean_uniform_ok_path <- file.path(
  dir_output, "a_04", "traffic_count_2_grid_sld_newhaven.rds"
)
if (!file.exists(uk_mean_uniform_ok_path)) {
  saveRDS(uk_mean_uniform_ok, uk_mean_uniform_ok_path)
}
