# Aline Changed:
# nyc borough shapefile -> new haven neighborhood shapefile

# Prepare traffic co2 emissions data at census block group level

################################################################################
# Declare root directory, folder locations and load essential stuff
dir_home <- file.path(here::here())
dir_data <- file.path(dir_home, "data")
dir_input <- file.path(dir_data, "input")
dir_output <- file.path(dir_data, "output")

################################################################################
# Set coordinate reference system to Connecticut state plane
# https://epsg.io/2234.
ct_crs <- 2234

################################################################################
# Load grids.
library(dplyr)
sld_us_loc_path <- file.path(
  dir_output, "a_01", "smart_location_data_subset.rds"
)
testthat::expect_true(file.exists(sld_us_loc_path))
sld_us_loc <- readRDS(sld_us_loc_path)
grid <- sf::st_centroid(sld_us_loc[, c("GEOID20")]) %>%
  sf::st_transform(ct_crs)

################################################################################
# Load neighborhoods.
neighborhoods_path <- file.path(
  dir_input,
  "NewHaven_NeighbhorhoodBoundaries",
  "NewHaven_NeighbhorhoodBoundaries.shp"
)
testthat::expect_true(file.exists(neighborhoods_path))
neighborhoods <- sf::st_read(neighborhoods_path) %>%
  sf::st_transform(ct_crs)
newhaven_boundaries <- sf::st_union(neighborhoods)
spatial_context <- newhaven_boundaries

################################################################################
# Define grid context.
grid_id_cntxt <- sapply(
  sf::st_intersects(grid, spatial_context),
  function(x) length(x) > 0
)
grid_contxt <- grid[grid_id_cntxt, ]
grid_contxt_df <- grid_contxt
sf::st_geometry(grid_contxt_df) <- NULL

################################################################################
# Import traffic emission data.
traffic_co2_emis_path <- file.path(dir_input, "DARTE_v2.gdb")
testthat::expect_true(file.exists(traffic_co2_emis_path))
traffic_co2_emis <- sf::read_sf(traffic_co2_emis_path)

################################################################################
# Calculate emissions per area
traffic_co2_emis$traffic_co2_emis <-
  traffic_co2_emis$kgco2_2017 / traffic_co2_emis$bg_area_m2
traffic_co2_emis <- traffic_co2_emis[, c("GEOID", "traffic_co2_emis")]

traffic_co2_emis_df <- traffic_co2_emis
sf::st_geometry(traffic_co2_emis_df) <- NULL
colnames(traffic_co2_emis_df)[1] <- "GEOID20"

grid_contxt_df <- dplyr::left_join(
  grid_contxt_df, traffic_co2_emis_df, by = "GEOID20"
)

################################################################################
# Save output.
grid_contxt_df_path <- file.path(
  dir_output, "a_08", "traffic_co2_emis_newhaven.rds"
)
if (!file.exists(grid_contxt_df_path)) {
  saveRDS(grid_contxt_df, grid_contxt_df_path)
}
