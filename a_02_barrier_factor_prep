#ALINE: Changes in this script include:
#1. nyc borough shapefile --> new haven neighborhood shapefile
#2. nomenclature shifts: boroughs --> neighborhoods; nyc=newhaven; newyorkcity=newhaven
#3. osm_driving_network_northeast.rds --> connecticut-latest.osm.pbf
#4. roads_ne_us --> roads_CT_us
#5. I can't get the two final plots to work

# script aim: prepare barrier factor variables for input to CSI
# First step to load packages etc.
# 1a Declare root directory, folder locations and load essential stuff
project.folder = paste0(print(here::here()),'/')
source(paste0(project.folder,'init_directory_structure.R'))
source(paste0(functions.folder,'script_initiate.R'))

### set resolution
crs <- 2163 #
geom_prec_n_cores <- 4

# load grids
# load grids
# data file generated at XXXX.R
sld_us_loc <- readRDS(paste0(generated.data.folder, "smart_location_data_subset.rds"))
grid <- sf::st_centroid(sld_us_loc[,c("GEOID20")]) %>%
  sf::st_transform(crs)
rm(sld_us_loc)

# load spatial context
#ALINE: CHANGED THE SHAPEFILE FROM NYC --> NEW HAVEN
# obtain NYC shapefile - find link for download in readme
neighborhoods <- sf::st_read(paste0(demography.data.folder, "NewHaven_NeighbhorhoodBoundaries.shp")) %>%
  sf::st_transform(crs)

newhaven_boundaries <- sf::st_union(neighborhoods)

spatial_context <- newhaven_boundaries
rm(newhaven_boundaries, neighborhoods)
# read urban grid
grid_id_cntxt <- sapply(sf::st_intersects(grid, spatial_context),function(x){length(x)>0})

grid_contxt <- grid[grid_id_cntxt, ]
rm(grid)
grid_contxt$id_local <- 1:nrow(grid_contxt)

#ALINE: Changed the dataset here
#Original benavides code had a .rds file, but tom already clipped it for CT and is a .bdf file
#So our file reads in differently than the osm_driving_network_northeast.rds one from Benavides
#Original Benavides code -------------
# road infrastructure osm data downloaded from open street maps - find link in readme
#osm_driving_network <- readRDS(paste0(generated.data.folder, "osm_driving_network_northeast.rds"))
#-------------------------------------
#Aline's new code:
# Define file path for Connecticut OSM data
osm_file <- file.path(project.folder, "data", "generated", "connecticut-latest.osm.pbf")
# Extract road network from OSM (only roads)
osm_driving_network <- oe_read(osm_file, layer = "lines")
#-------------------------------------


# extract driving network following Larkin et al., (2017)
os_highway_roads <- c(
  #Major roads were derived from OSM motorways, motorway links, trunks, trunk links, primary and secondary roads and links.
  'motorway', 'motorway_link', 'trunk', 'trunk_link', 'primary', 'secondary', 'primary_link', 'secondary_link')
roads_CT_us <- osm_driving_network[which(osm_driving_network$highway %in% os_highway_roads),]
roads_CT_us <- sf::st_transform(roads_CT_us, crs)

roads_contxt_id <- sapply(sf::st_intersects(roads_CT_us, spatial_context),function(x){length(x)>0})
roads_contxt <- roads_CT_us[roads_contxt_id, ]
rm(roads_CT_us)

#ALINE: They had a .gdb and we used a New Haven-specific shapefile
#Original Benavides code -------------
# road infrastructure faf5 data downloaded from faf5 geodatabase - find link in readme
#faf5_network <- sf::read_sf(paste0(geometry.data.folder, "FAF5 Network.gdb"))
#-------------------------------------
#Aline's new code - calling in the .shp
faf5_shp_file <- "/Users/alinemaybank/Desktop/CSI_new/data/raw/geometry/FAF5Network/F5F_NEWHAVEN.shp"
# Read the shapefile
faf5_network <- sf::st_read(faf5_shp_file)
#-------------------------------------

faf5_highways <- faf5_network[which(faf5_network$F_Class %in% c(1,2,3)),]
faf5_highways <- faf5_highways %>%
  sf::st_transform(2163)


roads_contxt_id_fhwa <- sapply(sf::st_intersects(faf5_highways, spatial_context),function(x){length(x)>0})
roads_contxt_fhwa <- faf5_highways[roads_contxt_id_fhwa, ]
rm(faf5_highways)
estimate_barrier_spatial_units  <- function(sp_unit_pos) {
  sp_unit <- grid_contxt[sp_unit_pos[1],]
  print(paste0("working on position ", sp_unit_pos[1]))
  influence_area <- sf::st_buffer(sp_unit, dist = 804.672)
  roads_buffer_id <- sapply(sf::st_intersects(roads_contxt, influence_area),function(x){length(x)>0})
  roads_local <- roads_contxt[roads_buffer_id, ]
  sp_unit_buffer_id <- sapply(sf::st_intersects(grid_contxt, influence_area),function(x){length(x)>0})
  sp_unit_buffer_cents <- grid_contxt[sp_unit_buffer_id, ]
  sp_unit_buffer_cents <- sp_unit_buffer_cents[-which(sp_unit_buffer_cents$id_local == sp_unit$id_local),]
  if(nrow(sp_unit_buffer_cents) > 0) {
    # find visible centroids from the sp_unit segment
    # obtain blocked centroids from sp_unit segment and their view factor
    barrier_factor <- sapply(seq_along(1:length(sp_unit_buffer_cents$id_local)), function(r) { 
      ray <- sf::st_cast(sf::st_union(sp_unit,sp_unit_buffer_cents[r,]),"LINESTRING")
      inters_loc <- lengths(sf::st_intersects(ray, roads_local, sparse = TRUE)) > 0
      
      if(any(inters_loc == TRUE)){
        barrier_factor <- 1
      } else {
        barrier_factor <- 0
      }
      return(barrier_factor)
    })
    severed_sp_units  <- sp_unit_buffer_cents$id_local[which(barrier_factor != 0)]
    if(length(severed_sp_units) > 0){
      n_contxt_barr_sp_units <- length(sp_unit_buffer_cents$id_local[which(barrier_factor != 0)])
      barrier_sp_units <- paste(severed_sp_units, collapse = " ")
      barrier_factor <- 100 * length(severed_sp_units) / length(sp_unit_buffer_cents$id_local)
    } else {
      n_contxt_barr_sp_units <- 0
      barrier_factor <- 0
    }
  } else {
    n_contxt_barr_sp_units <- 0
    barrier_factor <- 0
  }
  barr <- c(sp_unit_pos[2], n_contxt_barr_sp_units, barrier_factor) #barrier_sp_units,
  return(barr)
}

# running a 10% each time for making the run easier
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

barrier_sp_units_osm_coll <- data.frame()
for(grp_n in 1:10){
  grp <- get(paste0("group_", grp_n))
  sp_unit_df <- as.data.frame(grid_contxt[grp,])
  sp_unit_df$sp_unit_pos <- grp
  sp_unit_ap <- as.array(unlist(sp_unit_df[,c("sp_unit_pos", "id_local")]))
  dim(sp_unit_ap) <- c(sp_unit_pos = nrow(sp_unit_df), col = 2)
  
  barrier_sp_units_osm <- multiApply::Apply(list(sp_unit_ap), target_dims  = 'col',
                                            fun = estimate_barrier_spatial_units, ncores = geom_prec_n_cores)$output1
  barrier_sp_units_osm <- t(barrier_sp_units_osm)
  colnames(barrier_sp_units_osm) <- c("id_local", "n_contxt_sp_units_osm",  "barrier_factor_osm") # "barrier_sp_units",
  barrier_sp_units_osm_coll <- rbind(barrier_sp_units_osm_coll, barrier_sp_units_osm)
  if (grp_n < 10){
    saveRDS(barrier_sp_units_osm_coll, paste0(generated.data.folder, "barrier_sp_units_osm_partial.rds"))
  } else {
    saveRDS(barrier_sp_units_osm_coll, paste0(generated.data.folder, "barrier_sp_units_osm_all.rds"))
  }
  rm(barrier_sp_units_osm, sp_unit_ap)
}
rm(roads_contxt)
# do for highways and principal arterials from FHWA
roads_contxt <- roads_contxt_fhwa
barrier_sp_units_fhwa_coll <- data.frame()
for(grp_n in 1:10){
  grp <- get(paste0("group_", grp_n))
  sp_unit_df <- as.data.frame(grid_contxt[grp,])
  sp_unit_df$sp_unit_pos <- grp
  sp_unit_ap <- as.array(unlist(sp_unit_df[,c("sp_unit_pos", "id_local")]))
  dim(sp_unit_ap) <- c(sp_unit_pos = nrow(sp_unit_df), col = 2)
  
  barrier_sp_units_fhwa <- multiApply::Apply(list(sp_unit_ap), target_dims  = 'col',
                                             fun = estimate_barrier_spatial_units, ncores = geom_prec_n_cores)$output1
  barrier_sp_units_fhwa <- t(barrier_sp_units_fhwa)
  colnames(barrier_sp_units_fhwa) <- c("id_local", "n_contxt_sp_units_fhwa",  "barrier_factor_fhwa") # "barrier_sp_units",
  barrier_sp_units_fhwa_coll <- rbind(barrier_sp_units_fhwa_coll, barrier_sp_units_fhwa)
  if (grp_n < 10){
    saveRDS(barrier_sp_units_fhwa_coll, paste0(generated.data.folder, "barrier_sp_units_fhwa_partial.rds"))
  } else {
    saveRDS(barrier_sp_units_fhwa_coll, paste0(generated.data.folder, "barrier_sp_units_fhwa_all.rds"))
  }
  rm(barrier_sp_units_fhwa, sp_unit_ap)
} # "barrier_sp_units",

readRDS(paste0(generated.data.folder, "barrier_sp_units_osm_all.rds"))
print(paste0(generated.data.folder, "barrier_sp_units_osm_all.rds"))


#Aline: my code weirdly didnt work to read in the file, so i did it manually
#Original code --------
#barrier_sp_units_osm <- readRDS(paste0(generated.data.folder, "barrier_sp_units_osm_all")) # these are generated using OSM
#-------
#Aline's code--------
barrier_sp_units_osm <- readRDS("/Users/alinemaybank/Desktop/CSI_new/data/generated/barrier_sp_units_osm_all.rds")
#-------

barrier_sp_units_fhwa <- readRDS(paste0(generated.data.folder, "barrier_sp_units_fhwa_all.rds")) # these are generated using FHWA
barrier_sp_units_all <- dplyr::left_join(barrier_sp_units_osm, barrier_sp_units_fhwa, by = "id_local", copy = T)
grid_contxt <- dplyr::left_join(grid_contxt, barrier_sp_units_all, by = "id_local", copy = T)
grid_contxt_df <- grid_contxt
sf::st_geometry(grid_contxt_df) <- NULL
saveRDS(grid_contxt_df, paste0(generated.data.folder, "barrier_sp_units_newhaven_for_use.rds"))
grid_contxt_df <- readRDS(paste0(generated.data.folder, "barrier_sp_units_newhaven_for_use.rds"))

# explore visually
barrier_sp_units_osm <- readRDS(paste0(generated.data.folder, "barrier_sp_units_osm_all.rds"))
barrier_sp_units_fhwa <- readRDS(paste0(generated.data.folder, "barrier_sp_units_fhwa_all.rds"))

grid_contxt_df <- dplyr::left_join(grid_contxt_df, barrier_sp_units_osm, by = "id_local") %>%
  dplyr::left_join(barrier_sp_units_fhwa, by = "id_local")


# # # check dist map vs road infrastructure
install.packages("tmap", dependencies = TRUE)
library(tmap)
#Tmap was not working
#Error: package or namespace load failed for ‘tmap’:
#package slot missing from signature for generic ‘coerce’
#and classes STFDF, RasterBrick
#cannot use with duplicate class names (the package may need to be re-installed)
#In addition: Warning message:
#  package ‘tmap’ was built under R version 4.3.3
#ORIGINAL BENAVIDES CODE ----------
tmap_mode(mode = "view")
tmap_options(check.and.fix = TRUE)
map_barrier_factor_osm_ctxt <-  tm_shape(sf::st_make_valid(spatial_context)) +
  tm_borders(alpha = 0.1, col = "black") +
  tm_shape(grid_contxt) +
  tm_dots("barrier_factor_osm", palette = 'Oranges', scale=2, style = "cont") +
  tm_shape(roads_contxt) +
  tm_lines()

map_barrier_factor_fhwa_ctxt <-  tm_shape(sf::st_make_valid(spatial_context)) +
  tm_borders(alpha = 0.1, col = "black") +
  tm_shape(grid_contxt) +
  tm_dots("barrier_factor_fhwa", palette = 'Oranges', scale=2, style = "cont") +
  tm_shape(roads_contxt_fhwa) +
  tm_lines()
# ----------
#Aline: I cant get this to plot
