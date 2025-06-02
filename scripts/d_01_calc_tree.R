################################################################################
# Calculate tree cover statistics for census block groups.

################################################################################
# Source variables from a_00_initiate.R
source(file.path(here::here(), "scripts", "a_00_initiate.R"))

################################################################################
# Import CSI polygons data.
chr_sf_csi_path <- file.path(dir_output, "c_01", "sf_csi_polygons.rds")
testthat::expect_true(file.exists(chr_sf_csi_path))
sf_csi_polygons <- readRDS(chr_sf_csi_path)
sf_tree <- sf_csi_polygons[, "GEOID20"]

################################################################################
# Import "greenness" data.
chr_tree_path <- file.path(dir_input, "greenness", "tree_cover_New_Haven.tif")
testthat::expect_true(file.exists(chr_tree_path))
rast_tree <- terra::rast(chr_tree_path)

################################################################################
# Project polygons to native coordinate reference system of `rast_tree`.
vect_tree <- terra::vect(sf_tree) %>%
  terra::project(terra::crs(rast_tree))

################################################################################
# Extract cell values by polygon with exact cell weights.
df_extract <- terra::extract(
  rast_tree,
  vect_tree,
  fun = NULL,
  cells = TRUE
)

################################################################################
# Calculate % of cells = 1 within each polygon
df_aggregate <- aggregate(
  df_extract[, "classification"],
  by = list(df_extract$ID),
  FUN = function(x) mean(x == 1) * 100
)

################################################################################
# Merge with CSI polygons.
sf_tree$tree_cover_perc <- df_aggregate$x

################################################################################
# Save output.
chr_sf_tree_path <- file.path(dir_output, "d_01", "sf_tree.rds")
saveRDS(sf_tree, chr_sf_tree_path)

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
# Plot tree cover percentage values.
ggplot_tree_faf5 <- ggplot2::ggplot() +
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
    data = sf_tree,
    aes(fill = tree_cover_perc),
    color = "black"
  ) +
  ggplot2::scale_fill_distiller(
    palette = "Greens",
    name = "Percent (%) Tree Cover",
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
    xlim = sf::st_bbox(sf_tree)[c("xmin", "xmax")],
    ylim = sf::st_bbox(sf_tree)[c("ymin", "ymax")]
  ) +
  ggplot2::theme_bw()
ggplot_tree_faf5
