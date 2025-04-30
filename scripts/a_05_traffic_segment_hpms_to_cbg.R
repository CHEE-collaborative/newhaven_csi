# ALINE: Changes in this script:
# 1. Benavides used 2019 data for aadt, we used 2022
# 2. nyc borough shapefile -> new haven neighborhood shapefile
# 3. nomenclature shifts: boroughs -> neighborhoods; nyc -> newhaven;
#    newyorkcity -> newhaven
# 4. our aadt_segments_p has misisng values, while the nyc peoeple did not.
#    This may be because nyc has more complete traffic data.
# 5. Related to the kriging analysis later one:
# Due to the large range of AADT values in New Haven and gaps in data,
# kriging produced unstable variogram fitting. While the interpolation provides
# an estimate, values should be interpreted with caution, especially in areas
# with sparse traffic data.
# Interpolate traffic intensity data to census block groups.

################################################################################
# Declare root directory, folder locations and load essential stuff
source(file.path(here::here(), "R/helpers.R"))
csi_directories()

################################################################################
# Set coordinate reference system to Connecticut state plane
# https://epsg.io/2234.
ct_crs <- 2234

################################################################################
# Import AADT data generated in a_03_prep_traffic.R.
# Benavides used 2019 data, we used 2022 for AADT
aadt_newhaven_2022_path <- file.path(dir_output, "a_03", "aadt_ct_2022.rds")
testthat::expect_true(file.exists(aadt_newhaven_2022_path))
aadt_newhaven_2022 <- readRDS(aadt_newhaven_2022_path)
aadt_segments <- aadt_newhaven_2022

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

################################################################################
# Define AADT segments
aadt_segments <- sf::st_transform(, crs = ct_crs)
aadt_segments_id_cntxt <- sapply(
  sf::st_intersects(aadt_segments, spatial_context),
  function(x) length(x) > 0
)
aadt_segments_contxt <- aadt_segments[aadt_segments_id_cntxt, ]
aadt_segments_contxt <- sf::st_cast(aadt_segments_contxt, "LINESTRING")

################################################################################
# Select three points per line to represent the street segment in interpolation.
n_points <- 3
road_points <- sf::st_transform(aadt_segments_contxt, ct_crs) %>%
  sf::st_line_sample(n = n_points, type = "regular") %>%
  sf::st_cast("POINT")
aadt_segments_p <- sf::st_sf(
  aadt = rep(aadt_segments_contxt$aadt, each = n_points),
  geom = road_points
)

################################################################################
# Kriging from mid point.
# Aline adapted below -----------
# adapted from Criado et al. (2022) https://earth.bsc.es/gitlab/es/universalkriging/-/blob/production/general/UK_mean.R
# UK_mean_uniform_ok <- regrid_ok(non_uniform_data = sf::as_Spatial(aadt_segments_p), # traffic_esri_cntxt
                                #target_grid = sf::as_Spatial(grid_contxt), crs_sim = crs) # grid_contxt
# ALINE: To the line of code above, I got the error: Error in na.fail.default(list(aadt = c(88700L, 88700L, 88700L, 104100L, :
# missing values in object - new york data is likely more complete than new haven data
# So, I tried troubleshooting it
sum(is.na(aadt_segments_p$aadt))  # Count missing AADT values
#2199
# since there are large gaps - i ran a spatial interpolation to estimate AADT for missing roads
library(spdep)

missing_idx <- which(is.na(aadt_segments_p$aadt))
# Confirm the number of missing values
testthat::expect_equal(length(missing_idx), 2199)

# Create nearest neighbors
coords <- sf::st_coordinates(sf::st_centroid(aadt_segments_p))
nb <- spdep::knearneigh(coords, k = 10)
# tried k = 3 and still had 1276 nas
# tried k = 5 and still has 568 nas
# tried k = 7 and still has 262 nas
nb_list <- spdep::knn2nb(nb, sym = TRUE) # Ensure connections across sub-graphs

# Function to assign missing values from nearest neighbors
assign_aadt_from_neighbors <- function(index) {
  neighbors <- nb_list[[index]]
  neighbor_aadt <- aadt_segments_p$aadt[neighbors]
  mean(neighbor_aadt, na.rm = TRUE)
}

# Apply interpolation to missing AADT values
aadt_segments_p$aadt[missing_idx] <- sapply(
  missing_idx,
  assign_aadt_from_neighbors
)

# verify that the spatial interpolation worked
sum(is.na(aadt_segments_p$aadt))
# 64

# Fill in remaining NAs with dataset average AADT
aadt_segments_p$aadt[is.na(aadt_segments_p$aadt)] <- mean(
  aadt_segments_p$aadt,
  na.rm = TRUE
)
testthat::expect_equal(sum(is.na(aadt_segments_p$aadt)), 0)

#---------
#When I tried the code below again, I got warnings
#UK_mean_uniform_ok <- regrid_ok(non_uniform_data = sf::as_Spatial(aadt_segments_p), # traffic_esri_cntxt
                                #target_grid = sf::as_Spatial(grid_contxt), crs_sim = crs) # grid_contxt
#I got warnings saying: In fit.variogram(experimental_variogram, model = vgm(psill = psill,  ... :
#value out of range in 'bessel_k'
#this warning is likely due to new york having more stable constant traffic while new haven has low to very high traffic
summary(aadt_segments_p$aadt)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#100    4700    7252   21784   16400  152500 
#If you must use the kriging output, [include in manuscript]
#Due to the large range of AADT values in New Haven and gaps in data, 
#kriging produced unstable variogram fitting. While the interpolation provides an estimate,
#values should be interpreted with caution, especially in areas with sparse traffic data.

library(gstat)
source(file.path(dir_home, "R", "regrid_ok.R"))
uk_mean_uniform_ok <- regrid_ok(
  non_uniform_data = sf::as_Spatial(aadt_segments_p), # traffic_esri_cntxt
  target_grid = sf::as_Spatial(grid_contxt), # grid context
  crs_sim = ct_crs
)

# still got warnings, but i will mention it in my manuscript
colnames(uk_mean_uniform_ok)[1] <- "aadt"
uk_mean_uniform_ok$GEOID20 <- grid_contxt$GEOID20

################################################################################
# Save output.
uk_mean_uniform_ok_path <- file.path(
  dir_output, "a_05", "traffic_segment_2_grid_sld_newhaven.rds"
)
if (!file.exists(uk_mean_uniform_ok_path)) {
  saveRDS(uk_mean_uniform_ok, uk_mean_uniform_ok_path)
}
