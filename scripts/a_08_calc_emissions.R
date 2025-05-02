################################################################################
# Calculate traffic CO2 emissions for census block groups.

################################################################################
# Source variables from a_00_initiate.R
source(file.path(here::here(), "scripts", "a_00_initiate.R"))

################################################################################
# Import New Haven boundary.
sf_nh_boundary <- generate_newhaven(path = chr_towns_path, crs = int_crs_ct)
sf_context <- sf_nh_boundary

################################################################################
# Import grid.
chr_grid_nh <- file.path(dir_output, "a_04", "sf_grid_nh.rds")
testthat::expect_true(file.exists(chr_grid_nh))
sf_grid_nh <- readRDS(chr_grid_nh)

################################################################################
# Import traffic CO2 emission data.
chr_co2_emis_path <- file.path(dir_input, "DARTE_v2.gdb")
testthat::expect_true(file.exists(chr_co2_emis_path))
df_co2_emis <- sf::read_sf(chr_co2_emis_path) %>%
  sf::st_drop_geometry()

################################################################################
# Calculate CO2 emissions per m^2 for census block groups.
df_co2_emis$co2_emis_per_m2 <-
  df_co2_emis$kgco2_2017 / df_co2_emis$bg_area_m2
df_co2_emis_filter <- df_co2_emis[, c("GEOID", "co2_emis_per_m2")]

################################################################################
# Merge CO2 emissions with census block group grid.
sf_co2_emis <- dplyr::left_join(
  sf_grid_nh, df_co2_emis_filter, by = dplyr::join_by(GEOID20 == GEOID)
)
testthat::expect_identical("EPSG:2234", sf::st_crs(sf_co2_emis)$input)

################################################################################
# Save output.
chr_sf_co2_path <- file.path(dir_output, "a_08", "sf_co2_emis.rds")
if (!file.exists(chr_sf_co2_path)) saveRDS(sf_co2_emis, chr_sf_co2_path)
