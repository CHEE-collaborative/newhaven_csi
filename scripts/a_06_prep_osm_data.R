# Aline Changed:
# 1. changed the test_bbox from nyc --> new haven area
# 2. Benavides had crs = 4326 for the bounding boxes, but the whole rest of the project is in crs=2163
# So, i changed the crs for thr bounding boxes to 2163
# Updated CRS to match Connecitcut state plane 2234
# 3. Benavides was inconsistent about where to save northeast-latest.gpkg (meaning, raw vs geometry);
# so, I saved it to geometry (and titled it connecticut-latest.gpkg)

# Prepare OpenStreetMaps data.

################################################################################
# Declare root directory, folder locations and load essential stuff
dir_home <- file.path(here::here())
dir_data <- file.path(dir_home, "data")
dir_input <- file.path(dir_data, "input")
dir_output <- file.path(dir_data, "output")

################################################################################
# Set coordinate reference system to Connecticut state plane
# https://epsg.io/2234.
ct_crs <- 2234

################################################################################
# Define bounding box.
# ALINE CHANGED:
us_cont_bbox <- sf::st_as_sfc(
  sf::st_bbox(
    c(xmin = -130, xmax = -60, ymax = 51, ymin = 21),
    crs = ct_crs
  )
)
# The test_bbox is for NYC, so I changed it to be for New Haven
# test_bbox <- sf::st_as_sfc(sf::st_bbox(c(xmin = -73.8, xmax = -74.1, ymax = 41.2, ymin = 40.4), crs = 4326)) 
# New Haven Bounding Box
test_bbox <- sf::st_as_sfc(
  sf::st_bbox(c(xmin = -73.0, xmax = -72.85,ymax = 41.36, ymin = 41.23),
  crs = ct_crs)
)

################################################################################
# Import US boundary data.
us_boundaries_path <- file.path(
  dir_input, "nation", "cb_2018_us_nation_20m.shp"
)
testthat::expect_true(file.exists(us_boundaries_path))
us_boundaries <- sf::st_read(us_boundaries_path)
us_boundaries_proj <- sf::st_transform(us_boundaries, ct_crs)
us_boundaries_crop <- sf::st_intersection(us_boundaries_proj, us_cont_bbox)

################################################################################
# Road characteristics (make query for osm data).
driving_network_v_t_opts <- c(
  "-where", "
    (highway IS NOT NULL)
    AND
    (highway NOT IN (
    'abandoned', 'bus_guideway', 'byway', 'construction', 'corridor', 'elevator',
    'fixme', 'escalator', 'gallop', 'historic', 'no', 'planned', 'platform',
    'proposed', 'cycleway', 'pedestrian', 'bridleway', 'path', 'footway',
    'steps'
    ))
    AND
    (access NOT IN ('private', 'no'))
    AND
    (service NOT ILIKE 'private%')
    ")
driving_network_ext_tgs <- c(
  "lanes", "maxspeed", "access", "service", "barrier", "surface", "tiger:cfcc",
  "parking:lane:both", "parking:lane:left", "parking:lane:right"
)

################################################################################
# Save OpenStreetMap as .pbf
# They had "us-northeast-latest.osm.pbf" and I used connecticut-latest.osm.pbf
pbf <- file.path(dir_input, "connecticut-latest.osm.pbf")
testthat::expect_true(file.exists(pbf))

################################################################################
# road network and parking data call.
library(osmextract)
osmextract::oe_vectortranslate(
  pbf,
  layer = "lines",
  vectortranslate_options = driving_network_v_t_opts,
  osmconf_ini = NULL,
  extra_tags = driving_network_ext_tgs,
  force_vectortranslate = TRUE,
  never_skip_vectortranslate = FALSE,
  boundary = NULL,
  boundary_type = c("spat", "clipsrc"),
  quiet = FALSE
)

################################################################################
# Storing the just saved file in a known path and delete unused variables
# They say raw below, but they originally had the file in geometry, so i am
# keeping it there
# osm_driving_network <- osmextract::oe_read(paste0(raw.data.folder, "us-northeast-latest.gpkg"))
osm_driving_network <- osmextract::oe_read(
  file.path(dir_input, "connecticut-latest.gpkg")
)
osm_driving_network_proj <- sf::st_transform(
  osm_driving_network, ct_crs
)
unused_vars_ind <- which(
  colnames(osm_driving_network_proj) %in% c("waterway", "aerialway", "man_made")
)
osm_driving_network_proj <- osm_driving_network_proj[, -unused_vars_ind]

################################################################################
# Save output.
osm_driving_network_path <- file.path(
  dir_output, "a_06", "osm_driving_network_northeast.rds"
)
if (!file.exists(osm_driving_network_path)) {
  saveRDS(osm_driving_network_proj, osm_driving_network_path)
}
