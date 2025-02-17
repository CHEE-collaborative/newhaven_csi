#a_02_barrier_factor_prep.R

#Packages
install.packages("osmextract")  # For extracting OSM data from .pbf files
install.packages("sf")  # For spatial transformations
install.packages("multiApply")
install.packages("tmap")  # Install if not already installed
library(tmap)
library(sf)
library(osmextract)
library(multiApply)


### set resolution
crs <- 2163 #
geom_prec_n_cores <- 4

# load grids
# load grids
# data file generated at XXXX.R
sld_us_loc <- readRDS("generated_data/smart_location_data_subset.rds")

grid <- sf::st_centroid(sld_us_loc[,c("GEOID20")]) %>%
  sf::st_transform(crs)
rm(sld_us_loc)

# load spatial context
# obtain New Haven Neighborhood shapefile
#Changed the file name of borough --> neighborhood
neighborhood <- sf::st_read("NewHaven_NeighbhorhoodBoundaries.shp") %>%
  sf::st_transform(crs)

newhaven_boundaries <- sf::st_union(neighborhood)

spatial_context <- newhaven_boundaries
rm(newhaven_boundaries, neighborhood)

# Read urban grid and filter for grid points within New Haven boundaries
grid_id_cntxt <- sapply(sf::st_intersects(grid, spatial_context), function(x) {length(x) > 0})

grid_contxt <- grid[grid_id_cntxt, ]
rm(grid)
grid_contxt$id_local <- 1:nrow(grid_contxt)

# Road infrastructure osm data downloaded from open street maps - find link in readme
#Original benavides code had a .rds file, but tom already clipped it for CT and is a .bdf file
# Define path to your .pbf file
osm_file <- "connecticut-latest.osm.pbf"

# Read OSM roads data
osm_driving_network <- sf::st_read(osm_file, layer = "lines")
osm_driving_network <- oe_read(osm_file, layer = "lines")

# extract driving network following Larkin et al., (2017)
os_highway_roads <- c(
  'motorway', 'motorway_link', 'trunk', 'trunk_link', 
  'primary', 'secondary', 'primary_link', 'secondary_link')

# Keep only major roads
roads_ct <- osm_driving_network[osm_driving_network$highway %in% os_highway_roads, ]

# Transform to CRS 2163 (same as the rest of your data)
roads_ct <- sf::st_transform(roads_ct, crs)

roads_contxt_id <- sapply(sf::st_intersects(roads_ct, spatial_context), function(x) { length(x) > 0 })
roads_contxt <- roads_ct[roads_contxt_id, ]
rm(roads_ct)

# road infrastructure faf5 data downloaded from faf5 geodatabase
#they had a .gdb file and we used a .shp file
# Load the FAF5 road network from the Shapefile
faf5_network <- sf::st_read("FAF5 Network/F5F_NEWHAVEN.shp")

# Check column names to find the correct highway classification field
colnames(faf5_network)
faf5_highways <- faf5_network[faf5_network$F_Class %in% c(1, 2, 3), ]
faf5_highways <- sf::st_transform(faf5_highways, 2163)


roads_contxt_id_fhwa <- sapply(sf::st_intersects(faf5_highways, spatial_context), function(x) { length(x) > 0 })
roads_contxt_fhwa <- faf5_highways[roads_contxt_id_fhwa, ]
rm(faf5_highways)

estimate_barrier_spatial_units  <- function(sp_unit_pos) {
  sp_unit <- grid_contxt[sp_unit_pos[1],]
  print(paste0("working on position ", sp_unit_pos[1]))
  influence_area <- sf::st_buffer(sp_unit, dist = 804.672)
  
  roads_buffer_id <- sapply(sf::st_intersects(roads_contxt, influence_area), function(x) { length(x) > 0 })
  roads_local <- roads_contxt[roads_buffer_id, ]
  
  sp_unit_buffer_id <- sapply(sf::st_intersects(grid_contxt, influence_area), function(x) { length(x) > 0 })
  sp_unit_buffer_cents <- grid_contxt[sp_unit_buffer_id, ]
  sp_unit_buffer_cents <- sp_unit_buffer_cents[-which(sp_unit_buffer_cents$id_local == sp_unit$id_local),]
  
  if (nrow(sp_unit_buffer_cents) > 0) {
    barrier_factor <- sapply(seq_along(sp_unit_buffer_cents$id_local), function(r) { 
      ray <- sf::st_cast(sf::st_union(sp_unit, sp_unit_buffer_cents[r,]), "LINESTRING")
      inters_loc <- lengths(sf::st_intersects(ray, roads_local, sparse = TRUE)) > 0
      return(ifelse(any(inters_loc), 1, 0))
    })
    
    severed_sp_units <- sp_unit_buffer_cents$id_local[which(barrier_factor != 0)]
    n_contxt_barr_sp_units <- length(severed_sp_units)
    barrier_factor <- if (n_contxt_barr_sp_units > 0) 100 * n_contxt_barr_sp_units / length(sp_unit_buffer_cents$id_local) else 0
  } else {
    n_contxt_barr_sp_units <- 0
    barrier_factor <- 0
  }
  
  return(c(sp_unit_pos[2], n_contxt_barr_sp_units, barrier_factor))}

# Divide the grid into 10 groups for parallel processing
n_groups <- 10
groups <- split(1:nrow(grid_contxt), cut(1:nrow(grid_contxt), breaks=n_groups, labels=FALSE))

#TRY AGAIN
barrier_sp_units_osm_coll <- data.frame()
for (grp_n in 1:n_groups) {
  grp <- groups[[grp_n]]
  sp_unit_df <- as.data.frame(grid_contxt[grp,])
  sp_unit_df$sp_unit_pos <- grp
  
  # Convert to matrix with two columns
  sp_unit_ap <- matrix(unlist(sp_unit_df[,c("sp_unit_pos", "id_local")]), ncol = 2, byrow = FALSE)
  
  barrier_sp_units_osm <- multiApply::Apply(list(sp_unit_ap), target_dims = 2,
                                            fun = estimate_barrier_spatial_units, ncores = geom_prec_n_cores)$output1
  barrier_sp_units_osm <- t(barrier_sp_units_osm)
  
  colnames(barrier_sp_units_osm) <- c("id_local", "n_contxt_sp_units_osm", "barrier_factor_osm")
  barrier_sp_units_osm_coll <- rbind(barrier_sp_units_osm_coll, barrier_sp_units_osm)
  
  saveRDS(barrier_sp_units_osm_coll, paste0("generated_data/barrier_sp_units_osm_", grp_n, ".rds"))}
rm(roads_contxt)

# Process barrier factors using FAF5 highways
roads_contxt <- roads_contxt_fhwa
barrier_sp_units_fhwa_coll <- data.frame()

for (grp_n in 1:n_groups) {
  grp <- groups[[grp_n]]
  sp_unit_df <- as.data.frame(grid_contxt[grp,])
  sp_unit_df$sp_unit_pos <- grp
  
  # Ensure sp_unit_ap is a matrix with 2 columns
  sp_unit_ap <- matrix(unlist(sp_unit_df[,c("sp_unit_pos", "id_local")]), ncol = 2, byrow = FALSE)
  
  barrier_sp_units_fhwa <- multiApply::Apply(list(sp_unit_ap), target_dims = 2,
                                             fun = estimate_barrier_spatial_units, ncores = geom_prec_n_cores)$output1
  barrier_sp_units_fhwa <- t(barrier_sp_units_fhwa)
  
  colnames(barrier_sp_units_fhwa) <- c("id_local", "n_contxt_sp_units_fhwa", "barrier_factor_fhwa")
  barrier_sp_units_fhwa_coll <- rbind(barrier_sp_units_fhwa_coll, barrier_sp_units_fhwa)
  
  saveRDS(barrier_sp_units_fhwa_coll, paste0("generated_data/barrier_sp_units_fhwa_", grp_n, ".rds"))}

rm(barrier_sp_units_fhwa, sp_unit_ap)

# Load processed barrier data
getwd()
# List all the partial OSM files
osm_files <- list.files("generated_data/", pattern = "barrier_sp_units_osm_\\d+.rds", full.names = TRUE)

# Merge all the files
barrier_sp_units_osm_coll <- do.call(rbind, lapply(osm_files, readRDS))

# Save the merged file
saveRDS(barrier_sp_units_osm_coll, "generated_data/barrier_sp_units_osm_all.rds")

#call in the ALL file
barrier_sp_units_osm <- readRDS("generated_data/barrier_sp_units_osm_all.rds")  # OSM Data
##
# List all the partial FHWA files
fhwa_files <- list.files("generated_data/", pattern = "barrier_sp_units_fhwa_\\d+.rds", full.names = TRUE)

# Merge all FHWA files
barrier_sp_units_fhwa_coll <- do.call(rbind, lapply(fhwa_files, readRDS))

# Save the merged file
saveRDS(barrier_sp_units_fhwa_coll, "generated_data/barrier_sp_units_fhwa_all.rds")

barrier_sp_units_fhwa <- readRDS("generated_data/barrier_sp_units_fhwa_all.rds")  # FHWA Data

# Merge OSM and FHWA data on id_local
barrier_sp_units_all <- dplyr::left_join(barrier_sp_units_osm, barrier_sp_units_fhwa, by = "id_local", copy = TRUE)

# Merge with grid data
grid_contxt <- dplyr::left_join(grid_contxt, barrier_sp_units_all, by = "id_local", copy = TRUE)

# Save the final dataset
grid_contxt_df <- grid_contxt
sf::st_geometry(grid_contxt_df) <- NULL  # Remove geometry for saving
saveRDS(grid_contxt_df, "generated_data/barrier_sp_units_newhaven_for_use.rds")  # Updated filename for New Haven

# Reload the processed file for validation
grid_contxt_df <- readRDS("generated_data/barrier_sp_units_newhaven_for_use.rds")

###MAPPING
# Set tmap mode to interactive
# Set tmap mode to interactive (view mode)
tmap_mode("view")

# Ensure roads_contxt is defined before visualization
roads_contxt <- roads_ct  # Assign the correct road dataset

# Map barrier factors for OSM data
map_barrier_factor_osm_ctxt <-  tm_shape(sf::st_make_valid(spatial_context)) +
  tm_borders(alpha = 0.1, col = "black") +
  tm_shape(grid_contxt) +
  tm_dots("barrier_factor_osm", palette = 'Oranges', scale=2, style = "cont") +
  tm_shape(roads_contxt) +
  tm_lines()

# Map barrier factors for FHWA data
map_barrier_factor_fhwa_ctxt <-  tm_shape(sf::st_make_valid(spatial_context)) +
  tm_borders(alpha = 0.1, col = "black") +
  tm_shape(grid_contxt) +
  tm_dots("barrier_factor_fhwa", palette = 'Oranges', scale=2, style = "cont") +
  tm_shape(roads_contxt_fhwa) +
  tm_lines()

# Display the maps
map_barrier_factor_osm_ctxt
map_barrier_factor_fhwa_ctxt
