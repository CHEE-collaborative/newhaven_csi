#ALINE: Changes in this script include:
#1. nyc borough shapefile --> new haven neighborhood shapefile
#2. nomenclature shifts: boroughs --> neighborhoods; nyc=newhaven; newyorkcity=newhaven

#a_04_traffic_count_esri.cbg.R
# script aim: interpolate traffic intensity data to census block groups
# First step to load packages etc.
# 1a Declare root directory, folder locations and load essential stuff
project.folder = paste0(print(here::here()),'/')
source(paste0(project.folder,'init_directory_structure.R'))
source(paste0(functions.folder,'script_initiate.R'))

# set coordinate reference system
crs <- 2163

neighborhoods <- sf::st_read(paste0(demography.data.folder, "NewHaven_NeighbhorhoodBoundaries.shp")) %>%
  sf::st_transform(crs)

newhaven_boundaries <- sf::st_union(neighborhoods)
spatial_context <- newhaven_boundaries
# read data
# data generated at a_03_prep_traffic.R 
traffic_esri <- readRDS(paste0(generated.data.folder, "traffic_counts_esri.rds"))
traffic_esri <- sf::st_transform(traffic_esri, crs)
colnames(traffic_esri)[which(colnames(traffic_esri) == "Traffic1")] <- "aadt"

#CONTINUE HERE
# load grids
sld_us_loc <- readRDS(paste0(generated.data.folder, "smart_location_data_subset.rds"))
grid <- sf::st_centroid(sld_us_loc[,c("GEOID20")]) %>%
  sf::st_transform(crs)

traffic_esri_id_cntxt <- sapply(sf::st_intersects(traffic_esri, spatial_context),function(x){length(x)>0})
traffic_esri_cntxt <- traffic_esri[traffic_esri_id_cntxt, ]

grid_id_cntxt <- sapply(sf::st_intersects(grid, spatial_context),function(x){length(x)>0})
grid_contxt <- grid[grid_id_cntxt, ]

# regrid function can be found at code/functions

UK_mean_uniform_ok <- regrid_ok(non_uniform_data = sf::as_Spatial(traffic_esri_cntxt), # traffic_esri_cntxt
                                target_grid = sf::as_Spatial(grid_contxt), crs_sim = crs) # grid_contxt
colnames(UK_mean_uniform_ok)[1] <- "aadt"
UK_mean_uniform_ok$GEOID20 <- grid_contxt$GEOID20

saveRDS(UK_mean_uniform_ok, paste0(generated.data.folder, "traffic_count_2_grid_sld_newhaven.rds"))
