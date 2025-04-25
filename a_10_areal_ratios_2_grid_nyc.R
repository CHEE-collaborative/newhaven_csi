#a_10_areal_ratios_2_grid_nyc.R

#WARNING: this is the R script with the most amount of changes from the original code
#Aline changed:
#1. nyc borough shapefile --> new haven neighborhood shapefile
#2. New Haven does not have publicly accessible sidewalk GIS data
#So, replace nyc edges.shp --> homemade NH_Sidewalk.shp
#Also, based on CT building policy, all sidewalks must be at least 5 feet 1.524 m
# https://ecode360.com/28168812
#So, I forced that assumption onto our sidewalk shapefile
#3. New Haven was working on their new haven road dataset, so we had to troubleshoot
#we used a road dataset from:
#https://catalog.data.gov/dataset/tiger-line-shapefile-2021-county-new-haven-county-ct-all-roads
# crs matching:
#My output is EPSG:9311 (NAD27 / US National Atlas Equal Area) instead of EPSG:2163 (NAD83 / US National Atlas Equal Area).
#4. I created a bike lane map using the Yale CityofNewHaven5_bikemappdf
# I mapped out bike path, bike lane, and bike route marked with shared lane markings from a pdf (created a shp)
# +I excluded unmarked bike route

# script aim: estimate area of sidewalk in census block group surroundings
# First step to load packages etc.
# 1a Declare root directory, folder locations and load essential stuff
project.folder = paste0(print(here::here()),'/')
#for some reason, this stopped working: source(paste0(project.folder,'init_directory_structure.R'))
#for some reason, this stopped working: source(paste0(functions.folder,'script_initiate.R'))

#Manually defining the folders
project.folder <- here::here()
generated.data.folder <- file.path(project.folder, "generated_data/")
functions.folder <- file.path(project.folder, "functions/")
demography.data.folder <- file.path(project.folder, "data/demography/")
geometry.data.folder <- file.path(project.folder, "data/geometry/")


# set coordinate reference system
crs <- 2163

# load grids
# load grids
sld_us_loc <- readRDS(paste0("data/generated/smart_location_data_subset.rds"))
grid <- sf::st_centroid(sld_us_loc[,c("GEOID20")]) %>%
  sf::st_transform(crs)
rm(sld_us_loc)

# load testing areas
# obtain a spatial context for an example
neighborhoods <- sf::st_read(paste0("data/raw/demography/NewHaven_NeighbhorhoodBoundaries.shp")) %>%
  sf::st_transform(crs)

newhaven_boundaries <- sf::st_union(neighborhoods)

spatial_context <- newhaven_boundaries
rm(newhaven_boundaries, neighborhoods)

# limit to spatial context
grid_id_cntxt <- sapply(sf::st_intersects(grid, spatial_context),function(x){length(x)>0})

grid_contxt <- grid[grid_id_cntxt, ]
rm(grid)
grid_contxt$id_local <- 1:nrow(grid_contxt)

# load sidewalks 
#New Haven does not have publicly accessible sidewalk GIS data
edges <- sf::read_sf(paste0("data/raw/geometry/NH_Sidewalk.shp"))
#Creating a new column for sidewalk width (based on NH building code)
#New Aline code ------------------------------------------
# Define sidewalk width based on city policy (5 feet = 1.524 meters)
edges$width <- 1.524  
#LIMITATION: Width Accuracy:
#NYC: Precise width measurements (maybe via surveys or sensors).
#NH: Uniform width assumption (1.524 m for all).
#^^This adds uncertainty, but it’s acceptable for exploratory or city-wide aggregate analysis
#as long as you acknowledge the assumption in methods.

# Compute sidewalk length using spatial geometry
edges$length <- sf::st_length(edges)  

# Transform coordinate reference system (ensure it's correct)
edges <- edges %>%
  sf::st_transform(crs)

# View the new columns
head(edges)
colnames(edges)
# " [1] "F_id"       "foot"       "footway"    "highway"    "name_stree" "source"     "surface"   
#[8] "bridge"     "layer"      "lit"        "covered"    "tunnel"     "crossing"   "smoothness"
#[15] "width"      "bicycle"    "name"       "Shape_Leng" "geometry"   "length" 

# estimate area of sidewalk 
edges$sidewalk_area_m <- as.numeric(edges$width) * as.numeric(edges$length)

# load new haven roads in order to estimate area occupied by roads 
street_newhaven <- sf::read_sf(paste0('data/raw/geometry/Roadway_Designated_Urban_Area/Roadway_Designated_Urban_Area.shp'))
roads_contxt <- street_newhaven %>%
  sf::st_transform(4326) %>% # intermediate step needed to make it work :)
  sf::st_transform(crs)

colnames(roads_contxt)
#[1] "OBJECTID"   "TOWN_NAME"  "ROUTE_ID"   "BEGIN_POIN" "END_POINT"  "RIS_UA_COD" "HPMS_UA_CO"
#[8] "UA_CODE_DE" "YEAR"       "GlobalID"   "Shape__Len" "geometry

sf::st_crs(roads_contxt)
#it is in meters

#So, I don't need to transform to meters
# transform measures to meters
#roads_contxt$shape_leng_m <- roads_contxt$shape_leng * 0.3048 
roads_contxt$shape_leng_m <- roads_contxt$Shape__Len  # already in meters

# Assign a default road width if not present (18-24 feet typical for two-lane road/5.5–7.3m)
roads_contxt$st_width_m <- 6.5

#we don't have width for these new haven roads:
#roads_contxt$st_width_m <- roads_contxt$st_width * 0.3048 
roads_contxt$st_area <- roads_contxt$shape_leng_m * roads_contxt$st_width_m

#Read in bike lanes separately
bike_lanes <- sf::st_read("data/raw/geometry/bikelanes_2163.shp") %>%
  sf::st_transform(crs)

sf::st_crs(bike_lanes)
#in meters = don't need to convert
colnames(bike_lanes)
#[1] "Name"         "FolderPath"   "SymbolID"     "AltMode"      "Base"         "Clamped"     
#[7] "Extruded"     "Snippet"      "PopupInfo"    "Shape_Leng"   "geometry"     "bike_width_m"

#  bike lane area (assuming 5 feet = 1.524 meters width)
#The minimum width of a bike lane in New Haven, Connecticut is usually 5 feet, but may be 4 feet in some locations
bike_lanes$bike_width_m <- 1.524  # standard bike lane width
bike_lanes$length_m <- bike_lanes$Shape_Leng
bike_lanes$length_m <- as.numeric(bike_lanes$length_m)

# Calculate area
bike_lanes$bike_lane_area <- bike_lanes$length_m * bike_lanes$bike_width_m

#Just double-checking by length shape for bike lanes
head(cbind(bike_lanes$Shape_Leng, sf::st_length(bike_lanes)))
#> head(cbind(bike_lanes$Shape_Leng, sf::st_length(bike_lanes)))
#[,1]     [,2]
#[1,] 1533.412 1547.296
#[2,] 1296.459 1307.230
#[3,] 1158.747 1161.141
#[4,] 1306.879 1301.168
#[5,] 1581.672 1588.006
#[6,] 2272.110 2272.247
#all good, continuing on

# Run estimation function on 10 groups of spatial units
n_groups <- 10
group_1 <- 1:as.integer(nrow(grid_contxt)/10)
group_2 <- group_1 + as.integer(nrow(grid_contxt)/10)
group_3 <- group_2 + as.integer(nrow(grid_contxt)/10)
group_4 <- group_3 + as.integer(nrow(grid_contxt)/10)
group_5 <- group_4 + as.integer(nrow(grid_contxt)/10)
group_6 <- group_5 + as.integer(nrow(grid_contxt)/10)
group_7 <- group_6 + as.integer(nrow(grid_contxt)/10)
group_8 <- group_7 + as.integer(nrow(grid_contxt)/10)
group_9 <- group_8 + as.integer(nrow(grid_contxt)/10)
group_10 <- c(group_9 + as.integer(nrow(grid_contxt)/10), nrow(grid_contxt))

#I had to remove the "nyc" from this function in the functions.R file
#+ had to adapt the function a bit cause my bike lanes are not connected to my road data
#The function:
estim_areal_road_ped_ratio  <- function(sp_unit_pos) {
  sp_unit <- grid_contxt[sp_unit_pos[1],]
  influence_area <- sf::st_buffer(sp_unit, dist = 804.672)
  
  roads_buffer_id <- sapply(sf::st_intersects(roads_contxt, influence_area), function(x) length(x) > 0)
  roads_local <- roads_contxt[roads_buffer_id, ]
  
  sidewalk_buffer_id <- sapply(sf::st_intersects(edges, influence_area), function(x) length(x) > 0)
  sidewalk_local <- edges[sidewalk_buffer_id, ]
  
  # Add this here for bike lanes
  bikes_buffer_id <- sapply(sf::st_intersects(bike_lanes, influence_area), function(x) length(x) > 0)
  bikes_local <- bike_lanes[bikes_buffer_id, ]
  bikes_local_area <- sum(bikes_local$bike_lane_area, na.rm = TRUE)
  
  if(nrow(roads_local) > 0){
    roads_local_area <- sum(roads_local$st_area, na.rm = TRUE)
    sidewalk_local_area <- sum(sidewalk_local$sidewalk_area_m, na.rm = TRUE)
    
    # Adjust road area to subtract bike lanes if desired:
    roads_local_area <- roads_local_area - bikes_local_area
    
    streets_local_area <- roads_local_area + bikes_local_area + sidewalk_local_area
    
    sidewalk_to_road <- sidewalk_local_area / roads_local_area
    sidewalk_to_street <- sidewalk_local_area / streets_local_area
    road_to_street <- roads_local_area / streets_local_area
  } else {
    streets_local_area <- NA
    roads_local_area <- NA
    bikes_local_area <- NA
    sidewalk_local_area <- NA
    sidewalk_to_road <- NA
    sidewalk_to_street <- NA
    road_to_street <- NA
  }
  
  areal_ratio <- c(
    sp_unit_pos[2],
    streets_local_area, roads_local_area,
    bikes_local_area, sidewalk_local_area,
    sidewalk_to_road, sidewalk_to_street, road_to_street
  )
  return(areal_ratio)}


areal_road_ped_ratio_coll <- data.frame()
for(grp_n in 1:10){
  grp <- get(paste0("group_", grp_n))
  print(paste0("working on group ", grp_n))
  
  sp_unit_df <- as.data.frame(grid_contxt[grp,])
  sp_unit_df$sp_unit_pos <- grp
  sp_unit_ap <- as.array(unlist(sp_unit_df[,c("sp_unit_pos", "id_local")]))
  dim(sp_unit_ap) <- c(sp_unit_pos = nrow(sp_unit_df), col = 2)
  
  # function estim_areal_road_ped_ratio should be in your functions.R
  areal_road_ped_ratio <- multiApply::Apply(
    list(sp_unit_ap),
    target_dims = 'col',
    fun = estim_areal_road_ped_ratio,
    ncores = 4  # or config$geom_prec_n_cores if you're still using config
  )$output1
  
  areal_road_ped_ratio <- t(areal_road_ped_ratio)
  colnames(areal_road_ped_ratio) <- c(
    "id_local", "streets_local_area", "roads_local_area",
    "bikes_local_area", "sidewalk_local_area",
    "sidewalk_to_road", "sidewalk_to_street", "road_to_street"
  )
  
  areal_road_ped_ratio_coll <- rbind(areal_road_ped_ratio_coll, areal_road_ped_ratio)
  
  if (grp_n < 10){
    saveRDS(areal_road_ped_ratio_coll, paste0(generated.data.folder, "areal_ratio_partial_group_", grp_n, ".rds"))
  } else {
    saveRDS(areal_road_ped_ratio_coll, paste0(generated.data.folder, "areal_ratio_all.rds"))
  }
  
  rm(areal_road_ped_ratio, sp_unit_ap)
}
rm(roads_contxt)

# Final processing: load full results
areal_ratio_nh <- readRDS(paste0(generated.data.folder, "areal_ratio_all.rds"))
areal_ratio_nh$road_to_street[which(areal_ratio_nh$road_to_street < 0)] <- NA 

# Reload centroids to join GEOID20 back in
sld_us_loc <- readRDS(paste0("data/generated/smart_location_data_subset.rds"))
grid <- sf::st_centroid(sld_us_loc[, c("GEOID20")]) %>%
  sf::st_transform(crs)
rm(sld_us_loc)

# Recreate spatial context
neighborhoods <- sf::st_read("data/raw/demography/NewHaven_NeighbhorhoodBoundaries.shp") %>%
  sf::st_transform(crs)
newhaven_boundaries <- sf::st_union(neighborhoods)
spatial_context <- newhaven_boundaries
rm(newhaven_boundaries, neighborhoods)

# Limit grid again to context and attach local ID
grid_id_cntxt <- sapply(sf::st_intersects(grid, spatial_context), function(x) length(x) > 0)
grid_contxt <- grid[grid_id_cntxt, ]
grid_contxt$id_local <- 1:nrow(grid_contxt)

# Prepare grid data frame for join
grid_contxt_df <- grid_contxt
sf::st_geometry(grid_contxt_df) <- NULL

# Join GEOID20 into final table
areal_ratio_nh <- dplyr::left_join(areal_ratio_nh, grid_contxt_df[, c("id_local", "GEOID20")])

# Keep only the final relevant columns
areal_ratio_nh <- areal_ratio_nh[, c("GEOID20", "sidewalk_to_road", "sidewalk_local_area", "roads_local_area", "road_to_street")]

# Save final file
saveRDS(areal_ratio_nh, paste0(generated.data.folder, "areal_ratio_nh.rds"))
