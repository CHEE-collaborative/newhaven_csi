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
# Set API key.
tidycensus::census_api_key("YOUR_API_KEY", install = TRUE)

################################################################################
# Load variable list for 2019 ACS 5-year
df_acs5_vars <- tidycensus::load_variables(2019, "acs5", cache = TRUE)

################################################################################
# Demographic variables of interest.
chr_demographic_vars <- c(
  total = "B02001_001",
  white = "B02001_002",
  black = "B02001_003",
  native = "B02001_004",
  asian = "B02001_005",
  pacific = "B02001_006",
  other = "B02001_007",
  multirace = "B02001_008"
)
testthat::expect_true(all(chr_demographic_vars %in% df_acs5_vars$name))

################################################################################
# Query demographic variables for Connecticut census block groups.
sf_demographic <- get_acs(
  geography = "block group",
  state = "CT",
  variables = demographic_vars,
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

################################################################################
# Save output.
chr_demographic_path <- file.path(dir_output, "d_05", "sf_demographic_nh.rds")
saveRDS(sf_demographic_nh, chr_demographic_path)
