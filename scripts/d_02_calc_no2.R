################################################################################
# Calculate NO2 statistics for census block groups.

################################################################################
# Source variables from a_00_initiate.R
source(file.path(here::here(), "scripts", "a_00_initiate.R"))

################################################################################
# Import CSI polygons data.
chr_sf_csi_path <- file.path(dir_output, "c_01", "sf_csi_polygons.rds")
testthat::expect_true(file.exists(chr_sf_csi_path))
sf_csi_polygons <- readRDS(chr_sf_csi_path)
sf_no2 <- sf_csi_polygons[, "GEOID20"]

################################################################################
# Import NO2 data.
chr_no2_path <- list.files(
  file.path(dir_input, "no2"),
  recursive = TRUE,
  full.names = TRUE,
  pattern = ".nc4"
)
testthat::expect_true(unique(sapply(chr_no2_path, file.exists)))
rast_no2 <- terra::rast()
for (n in seq_along(chr_no2_path)) {
  int_no2_year <- strsplit(chr_no2_path[n], "_")[[1]][11]
  rast_no2_n <- terra::rast(chr_no2_path[n])[[1]]
  terra::time(rast_no2_n) <- as.integer(int_no2_year)
  names(rast_no2_n) <- paste0(names(rast_no2_n), "_", int_no2_year)
  rast_no2 <- c(rast_no2, rast_no2_n)
}

################################################################################
# Project polygons to native coordinate reference system of `rast_no2`
sf_csi_polygons_proj <- sf::st_transform(
  sf_csi_polygons, crs = terra::crs(rast_no2)
)

################################################################################
# Calculate 2019 and 5-year average NO2 values for census block groups.
rast_no2$mean <- mean(rast_no2[[grep("20", names(rast_no2))]])
sf_no2$no2_mean <- exactextractr::exact_extract(
  rast_no2$mean,
  sf_csi_polygons_proj,
  fun = "mean"
)
sf_no2$no2_2019 <- exactextractr::exact_extract(
  rast_no2$Tropospheric_NO2_2019,
  sf_csi_polygons_proj,
  fun = "mean"
)

################################################################################
# Re-project polygons to Connecticut state plane coordinate reference system.
sf_no2_proj <- sf::st_transform(sf_no2, crs = int_crs_ct)

################################################################################
# Save output.
chr_no2_path <- file.path(dir_output, "d_02", "sf_no2_proj.rds")
saveRDS(sf_no2_proj, chr_no2_path)

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
# Plot mean NO2 values.
ggplot_no2_mean_faf5 <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = sf_ct_towns, col = "grey50", fill = NA, lwd = 1
  ) +
  ggplot2::geom_sf(
    data = sf_context,
    fill = "black",
    alpha = 0.1,
    color = "black",
    lwd = 1
  ) +
  ggplot2::geom_sf(
    data = sf_no2_proj, aes(fill = no2_mean), color = "black"
  ) +
  ggplot2::scale_fill_distiller(
    palette = "YlGnBl", name = "NO2 Concentration (Mean 2019 - 2024)"
  ) +
  ggplot2::geom_sf(data = sf_faf5_123_nh, aes(color = "Roadway"), lwd = 1) +
  ggplot2::scale_color_manual(values = c("Roadway" = "black"), name = "") +
  ggplot2::geom_sf(
    data = sf_ct_towns, col = "grey50", fill = NA, lwd = 1
  ) +
  ggplot2::coord_sf(
    xlim = sf::st_bbox(sf_no2_proj)[c("xmin", "xmax")],
    ylim = sf::st_bbox(sf_no2_proj)[c("ymin", "ymax")]
  ) +
  ggplot2::theme_bw()
ggplot_no2_mean_faf5

################################################################################
# Plot 2019 NO2 values.
ggplot_no2_2019_faf5 <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = sf_ct_towns, col = "grey50", fill = NA, lwd = 1
  ) +
  ggplot2::geom_sf(
    data = sf_context,
    fill = "black",
    alpha = 0.1,
    color = "black",
    lwd = 1
  ) +
  ggplot2::geom_sf(
    data = sf_no2_proj, aes(fill = no2_2019), color = "black"
  ) +
  ggplot2::scale_fill_distiller(
    palette = "YlGnBl", name = "NO2 Concentration (2019)"
  ) +
  ggplot2::geom_sf(data = sf_faf5_123_nh, aes(color = "Roadway"), lwd = 1) +
  ggplot2::scale_color_manual(values = c("Roadway" = "black"), name = "") +
  ggplot2::geom_sf(
    data = sf_ct_towns, col = "grey50", fill = NA, lwd = 1
  ) +
  ggplot2::coord_sf(
    xlim = sf::st_bbox(sf_no2_proj)[c("xmin", "xmax")],
    ylim = sf::st_bbox(sf_no2_proj)[c("ymin", "ymax")]
  ) +
  ggplot2::theme_bw()
ggplot_no2_2019_faf5
