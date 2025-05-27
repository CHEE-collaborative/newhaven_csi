################################################################################
# Plot Community Severance Index (CSI) for New Haven.

################################################################################
# Source variables from a_00_initiate.R
source(file.path(here::here(), "scripts", "a_00_initiate.R"))

################################################################################
# Import CSI data.
chr_csi_path <- file.path(dir_output, "b_01", "sf_csi_nh.rds")
testthat::expect_true(file.exists(chr_csi_path))
sf_csi_nh <- readRDS(chr_csi_path)

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
# Import New Haven polygons.
sf_ct_2019 <- tigris::block_groups(
  state = "CT", county = "New Haven", cb = TRUE, year = 2019
) %>%
  sf::st_transform(crs = int_crs_ct)
sf_nh_2019 <- sf::st_intersection(sf_ct_2019, sf_context)
sf_nh_polygons <- sf_nh_2019[, "GEOID"]
names(sf_nh_polygons)[1] <- "GEOID20"

################################################################################
# Load Smart Location Database variables.
chr_sld_variables_path <- file.path(
  dir_output, "a_01", "sf_sld_variables_proj.rds"
)
testthat::expect_true(file.exists(chr_sld_variables_path))
sf_sld_variables_proj <- readRDS(chr_sld_variables_path)

################################################################################
# Add population data from Smart location Database.
sf_csi_nh_pop <- merge(
  sf_csi_nh,
  sf::st_drop_geometry(sf_sld_variables_proj[, c("GEOID20", "TotPop")]),
  by = "GEOID20"
)

################################################################################
# Merge CSI data with New Haven polygons.
sf_csi_polygons <- merge(
  sf_nh_polygons,
  sf::st_drop_geometry(sf_csi_nh_pop),
  by = "GEOID20"
)

################################################################################
# Import Freight Analysis Framework 5.0 Model Network Database data.
chr_faf5_path <- file.path(dir_input, "FAF5Network", "F5F_NEWHAVEN.shp")
testthat::expect_true(file.exists(chr_faf5_path))
sf_faf5_network <- sf::read_sf(chr_faf5_path) %>%
  sf::st_transform(crs = int_crs_ct)
testthat::expect_identical("EPSG:2234", sf::st_crs(sf_faf5_network)$input)
sf_faf5_123 <- sf_faf5_network[sf_faf5_network$F_Class %in% c(1, 2, 3), ]
sf_faf5_123_proj <- sf::st_transform(sf_faf5_123, crs = int_crs_ct)
sf_faf5_123_nh <- sf_faf5_123_proj[sf_context, ]
testthat::expect_identical("EPSG:2234", sf::st_crs(sf_faf5_123_nh)$input)
sf_faf5_123_nh$Roadway <- "Roadway"

################################################################################
# Plot Community Severance Index with FAF5 roads (ggplot2).
ggplot_csi_faf5 <- ggplot2::ggplot() +
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
    data = sf_csi_polygons, aes(fill = csi_normal), color = "black"
  ) +
  ggplot2::scale_fill_distiller(
    palette = "Blues", name = "CSI", direction = 1
  ) +
  ggplot2::geom_sf(data = sf_faf5_123_nh, aes(color = "Roadway"), lwd = 1) +
  ggplot2::scale_color_manual(values = c("Roadway" = "black"), name = "") +
  ggplot2::geom_sf(
    data = sf_ct_towns, col = "grey50", fill = NA, lwd = 1
  ) +
  ggplot2::coord_sf(
    xlim = sf::st_bbox(sf_csi_polygons)[c("xmin", "xmax")],
    ylim = sf::st_bbox(sf_csi_polygons)[c("ymin", "ymax")]
  ) +
  ggplot2::theme_bw()
ggplot_csi_faf5

################################################################################
# Save `ggplot_csi_faf5`.
chr_csi_faf5_path <- paste0(
  "figures/",
  "ggplot_csi_faf5_",
  format(Sys.time(), "%m%d_%H%M"),
  ".png"
)
png(chr_csi_faf5_path)
ggplot_csi_faf5
dev.off()

################################################################################
# Plot barrier data with FAF5 roads (ggplot2).
ggplot_barrier_faf5 <- ggplot2::ggplot() +
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
    data = sf_csi_polygons,
    aes(fill = barrier_factor_faf5_raw),
    color = "black"
  ) +
  ggplot2::scale_fill_distiller(
    palette = "Oranges", name = "Barrier Factor", direction = 1
  ) +
  ggplot2::geom_sf(data = sf_faf5_123_nh, aes(color = "Roadway"), lwd = 1) +
  ggplot2::scale_color_manual(values = c("Roadway" = "black"), name = "") +
  ggplot2::coord_sf(
    xlim = sf::st_bbox(sf_csi_polygons)[c("xmin", "xmax")],
    ylim = sf::st_bbox(sf_csi_polygons)[c("ymin", "ymax")]
  ) +
  ggplot2::theme_bw()
ggplot_barrier_faf5

################################################################################
# Save `ggplot_csi_faf5`.
chr_barrier_faf5_path <- paste0(
  "figures/",
  "ggplot_barrier_faf5_",
  format(Sys.time(), "%m%d_%H%M"),
  ".png"
)
png(chr_barrier_faf5_path)
ggplot_barrier_faf5
dev.off()

################################################################################
# Save CSI polygons data.
chr_sf_polygons_path <- file.path(dir_output, "c_01", "sf_csi_polygons.rds")
saveRDS(sf_csi_polygons, chr_sf_polygons_path)

chr_faf5_123_path <- file.path(dir_output, "c_01", "sf_faf5_123_nh.rds")
saveRDS(sf_faf5_123_nh, chr_faf5_123_path)
