################################################################################
# Calculate PM2.5 statistics for census block groups.

################################################################################
# Source variables from a_00_initiate.R
source(file.path(here::here(), "scripts", "a_00_initiate.R"))

################################################################################
# Import CSI polygons data.
chr_sf_csi_path <- file.path(dir_output, "c_01", "sf_csi_polygons.rds")
testthat::expect_true(file.exists(chr_sf_csi_path))
sf_csi_polygons <- readRDS(chr_sf_csi_path)
sf_pm25 <- sf_csi_polygons[, "GEOID20"]

################################################################################
# Import PM2.5 data.
chr_pm25_path <- list.files(
  file.path(dir_input, "pm2.5"),
  recursive = TRUE,
  full.names = TRUE,
  pattern = ".nc"
)
testthat::expect_true(unique(sapply(chr_pm25_path, file.exists)))
rast_pm25 <- terra::rast()
for (p in seq_along(chr_pm25_path)) {
  int_pm25_year <- substr(
    strsplit(chr_pm25_path[p], "America.")[[1]][2], 1, 4
  )
  rast_pm25_n <- terra::flip(
    terra::rast(chr_pm25_path[p]), direction = "vertical"
  )
  terra::time(rast_pm25_n) <- as.integer(int_pm25_year)
  names(rast_pm25_n) <- paste0(names(rast_pm25_n), "_", int_pm25_year)
  rast_pm25 <- c(rast_pm25, rast_pm25_n)
}

################################################################################
# Project polygons to native coordinate reference system of `rast_pm25`
sf_csi_polygons_proj <- sf::st_transform(
  sf_csi_polygons, crs = terra::crs(rast_pm25)
)

################################################################################
# Calculate 2019 and 5-year average pm25 values for census block groups.
rast_pm25$mean <- mean(rast_pm25[[grep("GWRPM25", names(rast_pm25))]])
sf_pm25$pm25_mean <- exactextractr::exact_extract(
  rast_pm25$mean,
  sf_csi_polygons_proj,
  fun = "mean"
)
sf_pm25$pm25_2019 <- exactextractr::exact_extract(
  rast_pm25$GWRPM25_2019,
  sf_csi_polygons_proj,
  fun = "mean"
)

################################################################################
# Re-project polygons to Connecticut state plane coordinate reference system.
sf_pm25_proj <- sf::st_transform(sf_pm25, crs = int_crs_ct)

################################################################################
# Save output.
chr_sf_pm25_path <- file.path(dir_output, "d_03", "sf_pm25_proj.rds")
saveRDS(sf_pm25_proj, chr_sf_pm25_path)

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
# Plot mean PM2.5 values.
ggplot_pm25_mean_faf5 <- ggplot2::ggplot() +
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
    data = sf_pm25_proj, aes(fill = pm25_mean), color = "black"
  ) +
  ggplot2::scale_fill_distiller(
    palette = "YlOrRd", name = "PM2.5 Concentration (Mean 2019 - 2024)"
  ) +
  ggplot2::geom_sf(data = sf_faf5_123_nh, aes(color = "Roadway"), lwd = 1) +
  ggplot2::scale_color_manual(values = c("Roadway" = "black"), name = "") +
  ggplot2::geom_sf(
    data = sf_ct_towns, col = "grey50", fill = NA, lwd = 1
  ) +
  ggplot2::coord_sf(
    xlim = sf::st_bbox(sf_pm25_proj)[c("xmin", "xmax")],
    ylim = sf::st_bbox(sf_pm25_proj)[c("ymin", "ymax")]
  ) +
  ggplot2::theme_bw()
ggplot_pm25_mean_faf5

################################################################################
# Plot 2019 PM2.5 values.
ggplot_pm25_2019_faf5 <- ggplot2::ggplot() +
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
    data = sf_pm25_proj, aes(fill = pm25_2019), color = "black"
  ) +
  ggplot2::scale_fill_distiller(
    palette = "YlOrRd", name = "PM2.5 Concentration (2019)"
  ) +
  ggplot2::geom_sf(data = sf_faf5_123_nh, aes(color = "Roadway"), lwd = 1) +
  ggplot2::scale_color_manual(values = c("Roadway" = "black"), name = "") +
  ggplot2::geom_sf(
    data = sf_ct_towns, col = "grey50", fill = NA, lwd = 1
  ) +
  ggplot2::coord_sf(
    xlim = sf::st_bbox(sf_pm25_proj)[c("xmin", "xmax")],
    ylim = sf::st_bbox(sf_pm25_proj)[c("ymin", "ymax")]
  ) +
  ggplot2::theme_bw()
ggplot_pm25_2019_faf5
