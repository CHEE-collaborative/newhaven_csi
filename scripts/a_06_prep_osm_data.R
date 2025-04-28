#Aline Changed:
#1. changed the test_bbox from nyc --> new haven area
#2. Benavides had crs= 4326 for the bounding boxes, but the whole rest of the project is in crs=2163
#So, i changed the crs for thr bounding boxes to 2163
#3. Benavides was inconsistent about where to save northeast-latest.gpkg (meaning, raw vs geometry);
#so, I saved it to geometry (and titled it connecticut-latest.gpkg)

#a_06_prep_osm_data.R

# script aim: prepare openstreetmaps data
# First step to load packages etc.
# 1a Declare root directory, folder locations and load essential stuff
project.folder = paste0(print(here::here()),'/')
source(paste0(project.folder,'init_directory_structure.R'))
source(paste0(functions.folder,'script_initiate.R'))

# set coordinate reference system
crs <- 2163

### load data
#ALINE CHANGED: 
us_cont_bbox <- sf::st_as_sfc(sf::st_bbox(c(xmin = -130, xmax = -60, ymax = 51, ymin = 21), crs = 2163)) 
#The test_bbox is for NYC, so I changed it to be for New Haven
#test_bbox <- sf::st_as_sfc(sf::st_bbox(c(xmin = -73.8, xmax = -74.1, ymax = 41.2, ymin = 40.4), crs = 4326)) 
# New Haven Bounding Box
test_bbox <- sf::st_as_sfc(sf::st_bbox(c(xmin = -73.0, xmax = -72.85,ymax = 41.36, ymin = 41.23), crs = 2163))

us_boundaries <- sf::st_read("data/raw/demography/nation/cb_2018_us_nation_20m.shp")
us_boundaries <- sf::st_transform(us_boundaries, 2163)
us_boundaries <- sf::st_intersection(us_boundaries, us_cont_bbox)


neighborhoods <- sf::st_read(paste0(demography.data.folder, "NewHaven_NeighbhorhoodBoundaries.shp")) %>%
  sf::st_transform(crs) %>%
  sf::st_union()

## road characteristics (make query for osm data)
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
driving_network_ext_tgs <- c("lanes", "maxspeed", "access", "service", "barrier", "surface", "tiger:cfcc", "parking:lane:both", "parking:lane:left", "parking:lane:right")

#They had "us-northeast-latest.osm.pbf" and I used connecticut-latest.osm.pbf
pbf = file.path(paste0(geometry.data.folder, "connecticut-latest.osm.pbf"))

# road network and parking data call
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
  quiet = FALSE)

# storing the just saved file in a known path and delete unused variables
#They say raw below, but they originally had the file in geometry, so i am keeping it there
#osm_driving_network <- osmextract::oe_read(paste0(raw.data.folder, "us-northeast-latest.gpkg"))
osm_driving_network <- osmextract::oe_read(paste0(geometry.data.folder, "connecticut-latest.gpkg"))

unused_vars_ind <- which(colnames(osm_driving_network) %in% c("waterway", "aerialway", "man_made"))
osm_driving_network <- osm_driving_network[,-unused_vars_ind]
osm_driving_network <- saveRDS(osm_driving_network, paste0(generated.data.folder, "osm_driving_network_northeast.rds"))
