################################################################################
# Query demographic data for census block groups.

################################################################################
# Source variables from a_00_initiate.R
source(file.path(here::here(), "scripts", "a_00_initiate.R"))

################################################################################
# Import CSI polygons data.
chr_sf_csi_path <- file.path(dir_output, "c_01", "sf_csi_polygons.rds")
testthat::expect_true(file.exists(chr_sf_csi_path))
sf_csi_polygons <- readRDS(chr_sf_csi_path)

################################################################################
# Load variable list for 2019 ACS 5-year
df_acs5_vars <- tidycensus::load_variables(2019, "acs5", cache = TRUE)

################################################################################
# Demographic variables of interest.
chr_demographic_vars <- c(
  total = "B03002_001",
  white = "B03002_003",
  black = "B03002_004",
  native = "B03002_005",
  asian = "B03002_006",
  pacific = "B03002_007",
  other = "B03002_008",
  multirace = "B03002_009",
  hispanic = "B03002_012"
)
testthat::expect_true(all(chr_demographic_vars %in% df_acs5_vars$name))

################################################################################
# Query demographic variables for Connecticut census block groups.
sf_demographic <- tidycensus::get_acs(
  geography = "block group",
  state = "CT",
  variables = chr_demographic_vars,
  year = 2019,
  survey = "acs5",
  geometry = TRUE,
  output = "wide"
) %>%
  sf::st_transform(crs = int_crs_ct)

################################################################################
# Filter to New Haven census block groups.
sf_demographic_nh <- sf_demographic[
  sf_demographic$GEOID %in% sf_csi_polygons$GEOID20,
  grep("NAME|M$", names(sf_demographic), invert = TRUE)
]
sf_demographic_nh$nonwhiteE <-
  sf_demographic_nh$totalE - sf_demographic_nh$white
sf_demographic_nh$perc_white <-
  sf_demographic_nh$whiteE / sf_demographic_nh$totalE * 100
sf_demographic_nh$perc_nonwhite <-
  sf_demographic_nh$nonwhiteE / sf_demographic_nh$totalE * 100
names(sf_demographic_nh) <- gsub("GEOID", "GEOID20", names(sf_demographic_nh))

################################################################################
# Save output.
chr_demographic_path <- file.path(dir_output, "d_05", "sf_demographic_nh.rds")
saveRDS(sf_demographic_nh, chr_demographic_path)
