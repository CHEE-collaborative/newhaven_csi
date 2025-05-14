################################################################################
# Interpolate Highway Performance Monitoring System (HPMS) data to census
# block groups.

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
# Intersection to identify New Haven roads for HPMS (2 mile buffer).
sf_hpms_segments_nh_b <- sf::st_intersection(
  sf_hpms_segments, sf::st_buffer(sf_context, 10560)
)
sf_hpms_string_nh <- sf::st_cast(sf_hpms_segments_nh_b, "LINESTRING")

################################################################################
# Select three points per line to represent the street segment in interpolation.
int_npoints <- 3L
sf_hpms_sample_cast <- sf::st_line_sample(
  sf_hpms_string_nh, n = int_npoints, type = "regular"
) %>%
  sf::st_cast("POINT")
sf_hpms_sample <- sf::st_sf(
  aadt = rep(sf_hpms_string_nh$aadt, each = int_npoints),
  geom = sf_hpms_sample_cast
)

################################################################################
# Identify points with valid/missing `aadt` values.
log_hpms_na <- is.na(sf_hpms_sample$aadt)

sf_hpms_valid <- sf_hpms_sample[!log_hpms_na, ]
testthat::expect_false(NA %in% sf_hpms_valid$aadt)

sf_hpms_na <- sf_hpms_sample[log_hpms_na, ]
testthat::expect_equal(unique(sf_hpms_na$aadt), as.integer(NA))

################################################################################
# Inverse distance weighting (IDW) for NA value gap filling.
gstat_idw_hpms <- gstat::gstat(
  formula = aadt ~ 1,
  data = sf_hpms_valid,
  nmax = 15,
  set = list(idp = 2)
)
sf_hpms_idw <- stats::predict(gstat_idw_hpms, sf_hpms_na)
sf_hpms_sample$aadt[log_hpms_na] <- sf_hpms_idw$var1.pred

################################################################################
# Ensure no missing values.
testthat::expect_equal(sum(is.na(sf_hpms_sample$aadt)), 0)

################################################################################
# Interpolate HPMS values to census block groups.
sf_regrid_hpms_cbg <- regrid_ok(
  non_uniform_data = sf::as_Spatial(sf_hpms_sample),
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
saveRDS(sf_regrid_hpms_cbg, chr_regrid_hpms_path)
