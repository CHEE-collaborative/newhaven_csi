#a_08_traffic_co2_emissions_to_cbg.R

#Aline Changed:
#nyc borough shapefile -> new haven neighborhood shapefile

# script aim: prepare traffic co2 emissions data at census block group level
# First step to load packages etc.
# 1a Declare root directory, folder locations and load essential stuff
project.folder = paste0(print(here::here()),'/')
source(paste0(project.folder,'init_directory_structure.R'))
source(paste0(functions.folder,'script_initiate.R'))

# set coordinate reference system
crs <- 2163

# load grids
sld_us_loc <- readRDS(paste0(generated.data.folder, "smart_location_data_subset.rds"))
grid <- sf::st_centroid(sld_us_loc[,c("GEOID20")]) %>%
  sf::st_transform(crs)

neighborhoods <- sf::st_read(paste0(demography.data.folder, "NewHaven_NeighbhorhoodBoundaries.shp")) %>%
  sf::st_transform(crs)

newhaven_boundaries <- sf::st_union(neighborhoods)
spatial_context <- newhaven_boundaries


grid_id_cntxt <- sapply(sf::st_intersects(grid, spatial_context),function(x){length(x)>0})
grid_contxt <- grid[grid_id_cntxt, ]

grid_contxt_df <- grid_contxt
sf::st_geometry(grid_contxt_df) <- NULL

traffic_co2_emis <- sf::read_sf(paste0(traffic.data.folder, "DARTE_v2.gdb"))
traffic_co2_emis$traffic_co2_emis <- traffic_co2_emis$kgco2_2017 / traffic_co2_emis$bg_area_m2
traffic_co2_emis <- traffic_co2_emis[,c("GEOID", "traffic_co2_emis")]

traffic_co2_emis_df <- traffic_co2_emis
sf::st_geometry(traffic_co2_emis_df) <- NULL
colnames(traffic_co2_emis_df)[1] <- "GEOID20"

grid_contxt_df <- dplyr::left_join(grid_contxt_df, traffic_co2_emis_df, by = "GEOID20")

saveRDS(grid_contxt_df, paste0(generated.data.folder, "traffic_co2_emis_newhaven.rds"))
