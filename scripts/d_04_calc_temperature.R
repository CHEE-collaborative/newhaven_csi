################################################################################
# Calculate cooling degree statistics for census block groups.

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
sf_cdd <- sf_csi_polygons[, "GEOID20"]

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
################################################################################
##### [BUG]: Download function requires debugging.
# Download Daymet temperature and vapor pressure data.
DownloadDaymetData(
  years = 2019,
  # location = c(-72.99777, 41.24645, -72.86083, 41.35038)
  location = c(41.24645, -72.99777, 41.35038, -72.86083)
)
################################################################################
################################################################################

################################################################################
# Calculate cooling degree days.
df_cdd <- cooling_degree_days(
  years = 2019,
  tmax_path = file.path(dir_input, "daymet", "tmax"),
  vp_path = file.path(dir_input, "daymet", "vp"),
  threshold = 65,
  locs = sf_csi_polygons,
  locs_id = "GEOID20",
  summarize = TRUE
)

################################################################################
# Merge with CSI polygons.
sf_cdd <- merge(sf_cdd, df_cdd, by = "GEOID20")

################################################################################
# Save output.
chr_sf_cdd_path <- file.path(dir_output, "d_04", "sf_cdd.rds")
saveRDS(sf_cdd, chr_sf_cdd_path)

################################################################################
# Import New Haven boundary.
sf_nh_boundary <- generate_newhaven(path = chr_towns_path, crs = int_crs_ct)
sf_context <- sf_nh_boundary

################################################################################
# Import Connecticut towns.
sf_ct_towns <- sf::read_sf(chr_towns_path) %>%
  sf::st_transform(crs = int_crs_ct) %>%
  dplyr::filter(LAND_CLASS == "Land") %>%
  dplyr::filter(STATE_NAME == "Connecticut")

################################################################################
# Import Freight Analysis Framework 5.0 Model Network Database data.
chr_faf5_path <- file.path(dir_output, "c_01", "sf_faf5_123_nh.rds")
testthat::expect_true(file.exists(chr_faf5_path))
sf_faf5_123_nh <- readRDS(chr_faf5_path)

################################################################################
# Plot CDD values.
ggplot_cdd_faf5 <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = sf_ct_towns,
    col = "grey50",
    fill = NA,
    lwd = 1
  ) +
  ggplot2::geom_sf(
    data = sf_context,
    fill = "black",
    alpha = 0.1,
    color = "black",
    lwd = 1
  ) +
  ggplot2::geom_sf(
    data = sf_cdd,
    aes(fill = CDD),
    color = "black"
  ) +
  ggplot2::scale_fill_distiller(
    palette = "Reds",
    name = "Cooling Degree Days",
    direction = 1
  ) +
  ggplot2::geom_sf(data = sf_faf5_123_nh, aes(color = "Roadway"), lwd = 1) +
  ggplot2::scale_color_manual(values = c("Roadway" = "black"), name = "") +
  ggplot2::geom_sf(
    data = sf_ct_towns,
    col = "grey50",
    fill = NA,
    lwd = 1
  ) +
  ggplot2::coord_sf(
    xlim = sf::st_bbox(sf_cdd)[c("xmin", "xmax")],
    ylim = sf::st_bbox(sf_cdd)[c("ymin", "ymax")]
  ) +
  ggplot2::theme_bw()
ggplot_cdd_faf5
