################################################################################
# Calculate temperature statistics for census block groups.

################################################################################
# Source variables from a_00_initiate.R
source(file.path(here::here(), "scripts", "a_00_initiate.R"))
source(file.path(here::here(), "R", "tejm_analysis.R"))

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
# create data folder if it doesn't already exist
if (!dir.exists(file.path(dir_daymet, "tmax"))) {
  dir.create(file.path(dir_daymet, "tmax"))
}
testthat::expect_true(dir.exists(file.path(dir_daymet, "tmax")))

# create a HICDD folder if it doesnt exist already
if (!dir.exists(file.path(dir_daymet, "vp"))) {
  dir.create(file.path(dir_daymet, "vp"))
}
testthat::expect_true(dir.exists(file.path(dir_daymet, "vp")))

################################################################################
# Download Daymet temperature and vapor pressure data.
DownloadDaymetData(
  years = 2019,
  # location = c(-72.99777, 41.24645, -72.86083, 41.35038)
  location = c(41.24645, -72.99777,  41.35038, -72.86083)
)


list_ghpym <- GetHicddsPerYear_monthly(
  year = 2019, census_data = sf_csi_polygons
)
names(list_ghpym[[1]])

days_in_season <- list_ghpym[[1]]$days_in_season
tmax_array_cooling_season_F <- list_ghpym[[2]]$tmax_array_cooling_season_F
relative_humidity <- list_ghpym[[3]]$relative_humidity


names(list_ghpym)


heat_index <- sapply(
  1:days_in_season,
  function(j) {
    GetHeatIndex(
      t = tmax_array_cooling_season_F[,, j],
      rh = relative_humidity[,, j]
    )
  }
)
