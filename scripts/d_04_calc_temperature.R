################################################################################
# Calculate temperature statistics for census block groups.

################################################################################
# Source variables from a_00_initiate.R
source(file.path(here::here(), "scripts", "a_00_initiate.R"))
source(file.path(here::here(), "R", "tejm_analysis.R"))
source(file.path(here::here(), "R", "cooling_days.R"))

################################################################################
# Import CSI polygons data.
chr_sf_csi_path <- file.path(dir_output, "c_01", "sf_csi_polygons.rds")
testthat::expect_true(file.exists(chr_sf_csi_path))
sf_csi_polygons <- readRDS(chr_sf_csi_path)

################################################################################
# Set Daymet data directory.
dir_daymet <- file.path(dir_input, "daymet")
testthat::expect_true(dir.exists(dir_daymet))

################################################################################
# Create `tmax` and `vp` data folders if they dont't already exist.
if (!dir.exists(file.path(dir_daymet, "tmax"))) {
  dir.create(file.path(dir_daymet, "tmax"))
}
testthat::expect_true(dir.exists(file.path(dir_daymet, "tmax")))

if (!dir.exists(file.path(dir_daymet, "vp"))) {
  dir.create(file.path(dir_daymet, "vp"))
}
testthat::expect_true(dir.exists(file.path(dir_daymet, "vp")))

################################################################################
# Download Daymet temperature and vapor pressure data.
DownloadDaymetData(
  years = 2019,
  # location = c(-72.99777, 41.24645, -72.86083, 41.35038)
  location = c(41.24645, -72.99777, 41.35038, -72.86083)
)

################################################################################
# Calculate cooling degree days.
source(file.path(here::here(), "R", "cooling_days.R"))
df_cdd <- cooling_degree_days(
  years = 2019,
  tmax_path = file.path(dir_input, "daymet", "tmax"),
  vp_path = file.path(dir_input, "daymet", "vp"),
  threshold = 65,
  locs = sf::st_centroid(sf_csi_polygons),
  locs_id = "GEOID20",
  summarize = TRUE
)

head(df_cdd)
summary(df_cdd)
str(df_cdd)
