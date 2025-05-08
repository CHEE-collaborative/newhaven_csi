################################################################################
# Interpolate Highway Performance Monitoring System (HPMS) data to census
# block groups.

################################################################################
################################################################################
################################################################################
# Due to the large range of AADT values in New Haven and gaps in data,
# kriging produced unstable variogram fitting. While the interpolation provides
# an estimate, values should be interpreted with caution, especially in areas
# with sparse traffic data.
################################################################################
################################################################################
################################################################################

################################################################################
# Source variables from a_00_initiate.R
source(file.path(here::here(), "scripts", "a_00_initiate.R"))

################################################################################
# Import New Haven boundary.
sf_nh_boundary <- generate_newhaven(path = chr_towns_path, crs = int_crs_ct)
sf_context <- sf_nh_boundary

################################################################################
# Import HPMS data (generated in a_03_prep_traffic.R).
chr_ct_hpms_proj_path <- file.path(dir_output, "a_03", "sf_ct_hpms_proj.rds")
testthat::expect_true(file.exists(chr_ct_hpms_proj_path))
sf_ct_hpms_proj <- readRDS(chr_ct_hpms_proj_path)
sf_hpms_segments <- sf_ct_hpms_proj

################################################################################
# Import grid.
chr_grid_nh <- file.path(dir_output, "a_04", "sf_grid_nh.rds")
testthat::expect_true(file.exists(chr_grid_nh))
sf_grid_nh <- readRDS(chr_grid_nh)

################################################################################
# Intersection to identify New Haven roads for HPMS.
sf_hpms_segments_nh <- sf::st_intersection(sf_hpms_segments, sf_context)
sf_hpms_string_nh <- sf::st_cast(sf_hpms_segments_nh, "LINESTRING")

################################################################################
# Select three points per line to represent the street segment in interpolation.
int_npoints <- 3L
sf_hpms_points_nh <- sf::st_line_sample(
  sf_hpms_string_nh, n = int_npoints, type = "regular"
) %>%
  sf::st_cast("POINT")
sf_hpms_segments_points_nh <- sf::st_sf(
  aadt = rep(sf_hpms_string_nh$aadt, each = int_npoints),
  geom = sf_hpms_points_nh
)

################################################################################
# Kriging from mid point.
# Aline adapted below -----------
# adapted from Criado et al. (2022) https://earth.bsc.es/gitlab/es/universalkriging/-/blob/production/general/UK_mean.R
# UK_mean_uniform_ok <- regrid_ok(
#   non_uniform_data = sf::as_Spatial(sf_hpms_segments_points_nh),
#   target_grid = sf::as_Spatial(sf_grid_nh),
#   crs_sim = int_crs_ct
# )
# ALINE: To the line of code above, I got the error: Error in na.fail.default(list(aadt = c(88700L, 88700L, 88700L, 104100L, :
# missing values in object - new york data is likely more complete than new haven data

################################################################################
# Identify points with missing values.
sum(is.na(sf_hpms_segments_points_nh$aadt))
int_aadt_na <- which(is.na(sf_hpms_segments_points_nh$aadt))

################################################################################
# 1. Why nearest neighbors to fill in NA values?
# 2. Why 3, 5, ... etc as number of neighbors/
# 3. Once interpolation with knn completed, why fill with mean?
# 4. Difference between AADT from this dataset and previously calculated?
# 5. What is meant by "new york has more stable constant traffic than
#    New Haven"? Quantification? Not valid justification.

################################################################################
# Create nearest neighbors
sf_hpms_segments_coords <- sf::st_coordinates(
  sf::st_centroid(sf_hpms_segments_points_nh)
)
knn_hmps <- spdep::knearneigh(sf_hpms_segments_coords, k = 3)
# tried k = 3 and still had 1276 NAs
# tried k = 5 and still has 568 NAs
# tried k = 7 and still has 262 NAs

################################################################################
# Ensure connections across sub-groups
nb_hpms <- spdep::knn2nb(knn_hmps, sym = TRUE)

################################################################################
# Function to assign missing values from nearest neighbors
assign_aadt_from_neighbors <- function(index) {
  neighbors <- nb_hpms[[index]]
  neighbor_aadt <- sf_hpms_segments_points_nh$aadt[neighbors]
  mean(neighbor_aadt, na.rm = TRUE)
}

################################################################################
# Apply knn-mean to missing AADT values.
sf_hpms_segments_points_nh$aadt[int_aadt_na] <- sapply(
  int_aadt_na,
  assign_aadt_from_neighbors
)

################################################################################
# Recheck missing values.
sum(is.na(sf_hpms_segments_points_nh$aadt))

################################################################################
# Fill in remaining NAs with dataset average AADT.
sf_hpms_segments_points_nh$aadt[
  is.na(sf_hpms_segments_points_nh$aadt)
] <- mean(
  sf_hpms_segments_points_nh$aadt,
  na.rm = TRUE
)

################################################################################
# Ensure no missing values.
testthat::expect_equal(sum(is.na(sf_hpms_segments_points_nh$aadt)), 0)

################################################################################
################################################################################
# I got warnings saying: In fit.variogram(experimental_variogram,
#model = vgm(psill = psill,  ... : value out of range in 'bessel_k'
# this warning is likely due to new york having more stable constant
# traffic while new haven has low to very high traffic
summary(sf_hpms_segments_points_nh$aadt)
# If you must use the kriging output, [include in manuscript]
# Due to the large range of AADT values in New Haven and gaps in data,
# kriging produced unstable variogram fitting. While the interpolation
# provides an estimate, values should be interpreted with caution,
# especially in areas with sparse traffic data.
################################################################################
################################################################################

################################################################################
# Interpolate HPMS values to census block groups.
sf_regrid_hpms_cbg <- regrid_ok(
  non_uniform_data = sf::as_Spatial(sf_hpms_segments_points_nh),
  target_grid = sf::as_Spatial(sf_grid_nh),
  crs_sim = int_crs_ct
)
colnames(sf_regrid_hpms_cbg)[1] <- "aadt_fhwa_segm"
sf_regrid_hpms_cbg$GEOID20 <- sf_grid_nh$GEOID20
sf_regrid_hpms_cbg <- sf_regrid_hpms_cbg[
  , grep("var", names(sf_regrid_hpms_cbg), invert = TRUE)
]

################################################################################
# Save output.
chr_regrid_hpms_path <- file.path(dir_output, "a_05", "sf_regrid_hpms_cbg.rds")
if (!file.exists(chr_regrid_hpms_path)) {
  saveRDS(sf_regrid_hpms_cbg, chr_regrid_hpms_path)
}
