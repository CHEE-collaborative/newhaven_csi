# Aline Changed:
# 1. nyc shapefile boroughs --> new haven neighborhoods
# 2. They had FAF5Network.gdb --> I used F5F_NEWHAVEN.shp
# 3. Replaced data_in_cs --> with grid_contxt

# Generate road infrastructure input data

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
# Load grids.
library(dplyr)
sld_us_loc_path <- file.path(
  dir_output, "a_01", "smart_location_data_subset.rds"
)
testthat::expect_true(file.exists(sld_us_loc_path))
sld_us_loc <- readRDS(sld_us_loc_path)
grid <- sf::st_centroid(sld_us_loc[, c("GEOID20")]) %>%
  sf::st_transform(ct_crs)

################################################################################
# Load neighborhoods.
neighborhoods_path <- file.path(
  dir_input,
  "NewHaven_NeighbhorhoodBoundaries",
  "NewHaven_NeighbhorhoodBoundaries.shp"
)
testthat::expect_true(file.exists(neighborhoods_path))
neighborhoods <- sf::st_read(neighborhoods_path) %>%
  sf::st_transform(ct_crs)
newhaven_boundaries <- sf::st_union(neighborhoods)
spatial_context <- newhaven_boundaries

################################################################################
# Urban grid
grid_id_cntxt <- sapply(
  sf::st_intersects(grid, spatial_context),
  function(x) length(x) > 0
)
grid_contxt <- grid[grid_id_cntxt, ]

################################################################################
# Road infrastructure from faf 5 model
# Freight Analysis Framework 5.0 Model Network Database
faf5_network_path <- file.path(dir_input, "FAF5Network", "F5F_NEWHAVEN.shp")
testthat::expect_true(file.exists(faf5_network_path))
faf5_network <- sf::read_sf(faf5_network_path)

################################################################################
# Interstate, Principal Arterial - Other Freeways and Expressways, and
# Principal Arterial - Other
faf5_highways <- faf5_network[which(faf5_network$F_Class %in% c(1, 2, 3)), ]
faf5_highways <- faf5_highways %>%
  sf::st_transform(ct_crs)
faf5_highways_newhaven <- faf5_highways[spatial_context, ]

################################################################################
# road infrastructure generated at community_severance_main_us
osm_driving_network_path <- file.path(
  dir_output, "a_06", "osm_driving_network_northeast.rds"
)
testthat::expect_true(file.exists(osm_driving_network_path))
osm_driving_network <- readRDS(osm_driving_network_path)

################################################################################
# extract driving network following Larkin et al., (2017)
# Major roads were derived from OSM motorways, motorway links, trunks, trunk
# links, primary and secondary roads and links
os_highway_roads <- c(
  # Major roads were derived from OSM motorways, motorway links, trunks, trunk
  # links, primary and secondary roads and links.
  "motorway", "motorway_link", "trunk", "trunk_link", "primary", "secondary",
  "primary_link", "secondary_link",
  # Minor roads were derived from OSM tertiary roads and tertiary road links.
  "tertiary", "tertiary_link", "unclassified",
  # Residential roads were derived from OSM residential roads and residential
  # road links
  "residential"
)
roads_ne_us <- osm_driving_network[
  which(osm_driving_network$highway %in% os_highway_roads),
]
# roads_ne_us <- sf::st_transform(roads_ne_us, config$crs)
# ^^ line of code gave me the error that "Error in config$crs : object of
# type 'closure' is not subsettable"
# So, I changed it to what I have below
roads_ne_us <- sf::st_transform(roads_ne_us, ct_crs)
roads_ne_us_newhaven <- roads_ne_us[spatial_context, ]

# motorway dist
cs_category <- c(
  "motorway", "motorway", "trunk", "trunk", "primary", "primary", "secondary",
  "secondary", "tertiary", "tertiary", "residential", "residential",
  "residential", "residential"
)
osm_category <- c(
  "motorway", "motorway_link", "trunk", "trunk_link", "primary",
  "primary_link", "secondary", "secondary_link", "tertiary", "tertiary_link",
  "residential", "residential_link", "unclassified", "unclassified_link"
)
road_types <- data.frame(
  cs_category = cs_category,
  osm_category = osm_category
)
cats <- unique(road_types$cs_category)
for (c in 1:length(cats)) {
  cat <- cats[c]
  osm_types <- road_types[which(road_types$cs_category == cat), "osm_category"]
  # [BUG] OSM `$highway` data generated in a_06_prep_osm_data.R does not match
  # expectation from current code base.
  roads_ne_us_loc <- roads_ne_us[which(roads_ne_us$highway %in% osm_types), ]
  if (nrow(roads_ne_us_loc) > 0) {
    roads_ne_us_loc_id_cntxt <- sapply(
      sf::st_intersects(roads_ne_us_loc, spatial_context),
      function(x) length(x) > 0
    )
    roads_ne_us_loc_test <- roads_ne_us_loc[roads_ne_us_loc_id_cntxt, ]
    dist <- sf::st_distance(grid_contxt, roads_ne_us_loc_test)
    var_name <- paste0(cat, "_dist")
    grid_contxt[, var_name] <- apply(dist, 1, FUN = min, na.rm = TRUE)
  } else {
    grid_contxt[, var_name] <- NA
  }
}

################################################################################
# distance to major roads according to faf 5
# FHWA highway functional class designation:
# 1 - Interstate
# 2 - Principal Arterial - Other Freeways and Expressways
# 3 - Principal Arterial - Other
cs_category <- c("interstate_highway", "freeways_expressways", 'other_princ_arter')
fhwa_category <- c(1, 2, 3)
road_types <- data.frame(cs_category = cs_category, fhwa_category = fhwa_category)
cats <- unique(road_types$cs_category)
for(c in 1:length(cats)){
  cat <- cats[c]
  fhwa_types <- road_types[which(road_types$cs_category == cat), "fhwa_category"]
  roads_loc <- faf5_highways[which(faf5_highways$F_Class %in% fhwa_types),]
  if(nrow(roads_loc) > 0){
    roads_loc_id_cntxt <- sapply(sf::st_intersects(roads_loc, spatial_context),function(x){length(x)>0})
    
    roads_loc_test <- roads_loc[roads_loc_id_cntxt, ]
    
    dist <- sf::st_distance(grid_contxt, roads_loc_test)
    var_name <- paste0(cat, "_dist")
    grid_contxt[,var_name] <- apply(dist, 1, FUN = min, na.rm = TRUE)
  } else {
    grid_contxt[,var_name] <- NA
  }
}


# distance metrics to proximity (reverse direction)
#They bring in data_in_cs here, but we haven't made that dataset
#I think they mean grid_context here, so I used that 
#max_motorway <- max(data_in_cs$motorway_dist) 
#data_in_cs$motorway_prox <- (max_motorway - data_in_cs$motorway_dist) / max_motorway
max_motorway <- max(grid_contxt$motorway_dist, na.rm = TRUE)
grid_contxt$motorway_prox <- (max_motorway - grid_contxt$motorway_dist) / max_motorway


# checking if abouve method is consistent with normalization below
normalize <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}
test <- grid_contxt
test$motorway_prox_2 <- 1- normalize(test$motorway_dist)
summary(test[,c("motorway_prox", "motorway_prox_2")])
# yes, it is,same result
#I got the same result

max_primary <- max(grid_contxt$primary_dist) 
grid_contxt$primary_prox <- (max_primary - grid_contxt$primary_dist) / max_primary

max_secondary <- max(grid_contxt$secondary_dist) 
grid_contxt$secondary_prox <- (max_secondary - grid_contxt$secondary_dist) / max_secondary

max_tertiary <- max(grid_contxt$tertiary_dist) 
grid_contxt$tertiary_prox <- (max_tertiary - grid_contxt$tertiary_dist) / max_tertiary

max_residential <- max(grid_contxt$residential_dist) 
grid_contxt$residential_prox <- (max_residential - grid_contxt$residential_dist) / max_residential

max_trunk <- max(grid_contxt$trunk_dist) 
grid_contxt$trunk_prox <- (max_trunk - grid_contxt$trunk_dist) / max_trunk

max_interstate_highway <- max(grid_contxt$interstate_highway_dist) 
grid_contxt$interstate_highway_prox <- (max_interstate_highway - grid_contxt$interstate_highway_dist) / max_interstate_highway

max_freeways_expressways <- max(grid_contxt$freeways_expressways_dist) 
grid_contxt$freeways_expressways_prox <- (max_freeways_expressways - grid_contxt$freeways_expressways_dist) / max_freeways_expressways

max_other_princ_arter <- max(grid_contxt$other_princ_arter_dist)
grid_contxt$other_princ_arter_prox <- (max_other_princ_arter - grid_contxt$other_princ_arter_dist) / max_other_princ_arter


grid_contxt <- grid_contxt[,-which(colnames(grid_contxt) %in% c("motorway_dist", "primary_dist", "secondary_dist", "tertiary_dist", "residential_dist", "trunk_dist",
                                                                "interstate_highway_dist", "freeways_expressways_dist", "other_princ_arter_dist"))]

saveRDS(grid_contxt, paste0(generated.data.folder, "road_inf_dist_2_grid_sld_newhaven.rds"))

