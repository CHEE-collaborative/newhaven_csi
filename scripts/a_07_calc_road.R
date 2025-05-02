################################################################################
# Calculate road infrastructure statistics for census block groups.

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
# Import Freight Analysis Framework 5.0 Model Network Database data.
chr_faf5_path <- file.path(dir_input, "FAF5Network", "F5F_NEWHAVEN.shp")
testthat::expect_true(file.exists(chr_faf5_path))
sf_faf5_network <- sf::read_sf(chr_faf5_path) %>%
  sf::st_transform(crs = int_crs_ct)
testthat::expect_identical("EPSG:2234", sf::st_crs(sf_faf5_network)$input)

################################################################################
# Filter to 'Interstate', 'Principal Arterial - Other Freeways and Expressways',
# and 'Principal Arterial - Other'
sf_faf5_123 <- sf_faf5_network[sf_faf5_network$F_Class %in% c(1, 2, 3), ]
sf_faf5_123_proj <- sf::st_transform(sf_faf5_123, crs = int_crs_ct)
sf_faf5_123_nh <- sf_faf5_123_proj[sf_context, ]
testthat::expect_identical("EPSG:2234", sf::st_crs(sf_faf5_123_nh)$input)

################################################################################
# Import OSM road infrastructure data (generated in a_06_prep_osm.R).
chr_osm_nh_path <- file.path(dir_output, "a_06", "sf_osm_nh_filter.rds")
testthat::expect_true(file.exists(chr_osm_nh_path))
sf_osm_nh_filter <- readRDS(chr_osm_nh_path)
testthat::expect_identical("EPSG:2234", sf::st_crs(sf_osm_nh_filter)$input)

################################################################################
# Extract driving network following Larkin et al., (2017).
chr_major_roads <- c(
  "motorway", "motorway_link", "trunk", "trunk_link", "primary",
  "secondary", "primary_link", "secondary_link"
)
chr_minor_roads <- c("tertiary", "tertiary_link", "unclassified")
chr_residential_roads <- c("residential")
chr_osm_roads <- c(chr_major_roads, chr_minor_roads, chr_residential_roads)
sf_osm_roads <- sf_osm_nh_filter[sf_osm_nh_filter$highway %in% chr_osm_roads, ]
testthat::expect_identical("EPSG:2234", sf::st_crs(sf_osm_roads)$input)
sf_osm_roads_nh <- sf_osm_roads[sf_context, ]

################################################################################
# Match OSM road categories to CSI road categories
chr_cs_roadcat <- c(
  "motorway", "motorway", "trunk", "trunk", "primary", "primary", "secondary",
  "secondary", "tertiary", "tertiary", "residential", "residential",
  "residential", "residential"
)
chr_osm_roadcat <- c(
  "motorway", "motorway_link", "trunk", "trunk_link", "primary",
  "primary_link", "secondary", "secondary_link", "tertiary", "tertiary_link",
  "residential", "residential_link", "unclassified", "unclassified_link"
)
df_osm_roadcat <- data.frame(
  cs_roadcat = chr_cs_roadcat,
  osm_roadcat = chr_osm_roadcat
)

################################################################################
# Calculate distance to major roads according to OSM.
sf_grid_osm_nh <- sf_grid_nh
for (c in seq_along(chr_cs_roadcat)) {
  chr_osm_cat <- df_osm_roadcat[
    which(df_osm_roadcat$cs_roadcat == chr_cs_roadcat[c]),
    "osm_roadcat"
  ]
  sf_osm_cat <- sf_osm_roads_nh[sf_osm_roads_nh$highway %in% chr_osm_cat, ]
  chr_osm_varname <- paste0(chr_cs_roadcat[c], "_dist")
  if (nrow(sf_osm_cat) > 0) {
    sf_osm_cat_nh <- sf::st_intersection(sf_osm_cat, sf_context)
    sf_osm_dist <- sf::st_distance(sf_grid_osm_nh, sf_osm_cat_nh)
    sf_grid_osm_nh[, chr_osm_varname] <- apply(
      sf_osm_dist, 1, FUN = min, na.rm = TRUE
    )
  } else {
    sf_grid_osm_nh[, chr_osm_varname] <- NA
  }
}

################################################################################
# Match FAF5 road categories to CSI road categories.
chr_faf5_roadcat <- c(
  "interstate_highway", "freeways_expressways", "other_princ_arter"
)
chr_fhwa_roadcat <- c(1L, 2L, 3L)
df_faf5_roadcat <- data.frame(
  faf5_roadcat = chr_faf5_roadcat,
  fhwa_roadcat = chr_fhwa_roadcat
)

################################################################################
# Calculate distance to major roads according to FAF5.
sf_grid_faf5_nh <- sf_grid_nh
for (c in seq_along(chr_faf5_roadcat)) {
  chr_fhwa_cat <- df_faf5_roadcat[
    which(df_faf5_roadcat$faf5_roadcat == chr_faf5_roadcat[c]),
    "fhwa_roadcat"
  ]
  sf_faf5_cat <- sf_faf5_123_nh[sf_faf5_123_nh$F_Class %in% chr_fhwa_cat, ]
  chr_faf5_varname <- paste0(chr_faf5_roadcat[c], "_dist")
  if (nrow(sf_faf5_cat) > 0) {
    sf_faf5_cat_nh <- sf::st_intersection(sf_faf5_cat, sf_context)
    sf_faf5_dist <- sf::st_distance(sf_grid_faf5_nh, sf_faf5_cat_nh)
    sf_grid_faf5_nh[, chr_faf5_varname] <- apply(
      sf_faf5_dist, 1, FUN = min, na.rm = TRUE
    )
  } else {
    sf_grid_faf5_nh[, chr_faf5_varname] <- NA
  }
}

################################################################################
# Merge OSM distance and FAF5 distance data.
sf_grid_dist <- merge(
  sf_grid_osm_nh,
  sf::st_drop_geometry(sf_grid_faf5_nh),
  by = "GEOID20"
)

################################################################################
# Calculate proximity to major roadsmetric with local `normalize()` function.
chr_road_vars <- grep("_dist", names(sf_grid_dist), value = TRUE)
for (v in seq_along(chr_road_vars)) {
  chr_road_var_prox <- gsub("_dist", "_prox", chr_road_vars[v])
  sf_grid_dist[[chr_road_var_prox]] <-
    (1 - normalize(sf_grid_dist[[chr_road_vars[v]]]))
}

################################################################################
# Drop distance metrics.
sf_grid_prox <- sf_grid_dist[
  , grep("_dist", names(sf_grid_dist), invert = TRUE)
]

################################################################################
# Save output.
chr_grid_prox_path <- file.path(dir_output, "a_07", "sf_grid_prox.rds")
if (!file.exists(chr_grid_prox_path)) saveRDS(sf_grid_prox, chr_grid_prox_path)
