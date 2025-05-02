################################################################################
# Interpolate Average Annual Traffic Density (AADT) data to census block groups.

################################################################################
# Source variables from a_00_initiate.R
source(file.path(here::here(), "scripts", "a_00_initiate.R"))

################################################################################
# Import New Haven boundary.
sf_nh_boundary <- generate_newhaven(path = chr_towns_path, crs = int_crs_ct)
sf_context <- sf_nh_boundary

################################################################################
# Import AADT data (generated in a_03_prep_traffic.R).
chr_aadt_path <- file.path(dir_output, "a_03", "traffic_counts_esri.rds")
# chr_aadt_path <- file.path(dir_output, "a_03", "sf_aadt_proj.rds")
testthat::expect_true(file.exists(chr_aadt_path))
sf_aadt_proj <- readRDS(chr_aadt_path) %>%
  sf::st_transform(crs = int_crs_ct)

################################################################################
# Rename "Traffic1" as "aadt"
colnames(sf_aadt_proj)[
  which(colnames(sf_aadt_proj) == "Traffic1")
] <- "aadt"

################################################################################
# Load Smart Location Database variables.
chr_sld_variables_path <- file.path(
  dir_output, "a_01", "sf_sld_variables_proj.rds"
)
testthat::expect_true(file.exists(chr_sld_variables_path))
sf_sld_variables_proj <- readRDS(chr_sld_variables_path)

################################################################################
# Create centroids with census block group GEOID.
sf_sld_geoid <- sf::st_centroid(sf_sld_variables_proj[, c("GEOID20")])

################################################################################
# Intersection to identify New Haven roads for AADT.
sf_aadt_nh <- sf::st_intersection(sf_aadt_proj, sf_context)

################################################################################
# Intersection to identify New Haven census block groups in SLD.
sf_grid_nh <- sf::st_intersection(sf_sld_geoid, sf_context)
##### [BUG] Spatial intersection results in 105 censsu block groups, when
#####       import from `tigris::block_groups()` reports 120 (see scratch.R)

################################################################################
# Interpolate AADT values to census block groups.
# From https://github.com/jaime-benavides/community_severance_nyc/blob/main/code/functions/functions.R
source(file.path(dir_home, "R", "regrid_ok.R"))
sf_regrid_aadt_cbg <- regrid_ok(
  non_uniform_data = sf::as_Spatial(sf_aadt_nh),
  target_grid = sf::as_Spatial(sf_grid_nh),
  crs_sim = int_crs_ct
)
colnames(sf_regrid_aadt_cbg)[1] <- "aadt"
sf_regrid_aadt_cbg$GEOID20 <- sf_regrid_aadt_cbg$GEOID20

################################################################################
# Save output.
chr_regrid_aadt_cbg <- file.path(
  dir_output, "a_04", "sf_regrid_aadt_cbg.rds"
)
if (!file.exists(chr_regrid_aadt_cbg)) {
  saveRDS(sf_regrid_aadt_cbg, chr_regrid_aadt_cbg)
}

chr_sld_nh <- file.path(dir_output, "a_04", "sf_grid_nh.rds")
if (!file.exists(chr_sld_nh)) saveRDS(sf_grid_nh, chr_sld_nh)
