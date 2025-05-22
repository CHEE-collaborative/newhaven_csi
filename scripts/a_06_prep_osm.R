################################################################################
# Prepare OpenStreetMaps data.

################################################################################
# Source variables from a_00_initiate.R
source(file.path(here::here(), "scripts", "a_00_initiate.R"))

################################################################################
# Road characteristics query for `osmextract:oe_get`.
chr_osm_network_tags <- c(
  "lanes", "maxspeed", "access", "service", "barrier", "surface", "tiger:cfcc",
  "parking:lane:both", "parking:lane:left", "parking:lane:right"
)

################################################################################
# Import New Haven boundary.
sf_nh_boundary <- generate_newhaven(chr_towns_path)
sf_nh_bbox <- sf::st_as_sfc(sf::st_bbox(sf_nh_boundary))

################################################################################
# Get OpenStreetMap data with pre-defined options. Save in `data/input/`.
chr_osm_gpkg_path <- file.path(dir_input, "geofabrik_connecticut-latest.gpkg")
if (file.exists(chr_osm_gpkg_path)) {
  sf_osm_nh <- osmextract::oe_read(chr_osm_gpkg_path) %>%
    sf::st_transform(crs = int_crs_ct)
} else {
  sf_osm_nh <- osmextract::oe_get(
    sf_nh_bbox,
    extra_tags = chr_osm_network_tags,
    download_directory = file.path(dir_input),
    layer = "lines",
    osmconf_ini = NULL,
    force_vectortranslate = TRUE,
    never_skip_vectortranslate = FALSE,
    boundary = NULL,
    boundary_type = c("spat", "clipsrc"),
    quiet = FALSE
  ) %>%
    sf::st_transform(crs = int_crs_ct)
}
testthat::expect_identical("EPSG:2234", sf::st_crs(sf_osm_nh)$input)

################################################################################
# Filter to desired highway characteristics and drop unused columns.
chr_filter_highway <- c(
  "abandoned", "bus_guideway", "byway", "construction", "corridor", "elevator",
  "fixme", "escalator", "gallop", "historic", "no", "planned", "platform",
  "proposed", "cycleway", "pedestrian", "bridleway", "path", "footway", "steps",
  "extraFORtest"
)
chr_filter_access <- c("private", "no")

sf_osm_nh_filter <- sf_osm_nh[
  which(!sf_osm_nh$highway %in% chr_filter_highway &
      !sf_osm_nh$access %in% chr_filter_access
  ), !colnames(sf_osm_nh) %in% c("waterway", "aerialway", "man_made")
]
testthat::expect_identical("EPSG:2234", sf::st_crs(sf_osm_nh_filter)$input)

################################################################################
# Save output.
chr_osm_nh_path <- file.path(dir_output, "a_06", "sf_osm_nh_filter.rds")
saveRDS(sf_osm_nh_filter, chr_osm_nh_path)
