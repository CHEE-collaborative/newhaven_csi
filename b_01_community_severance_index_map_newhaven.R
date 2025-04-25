#b_01_community_severance_index_map_newhaven.R

#Things Aline changes:
#1. Switch out FAF5Network.gdb for F5F_NEWHAVEN.shp

# script aim: create maps for barrier factor and community severance index in New Haven
# First step to load packages etc.
# 1a Declare root directory, folder locations and load essential stuff
project.folder = paste0(print(here::here()),'/')
source(paste0(project.folder,'init_directory_structure.R'))
source(paste0(functions.folder,'script_initiate.R'))

# set coordinate reference system
crs <- 2163

sld_us_loc <- readRDS(paste0(generated.data.folder, "smart_location_data_subset.rds"))
dat_scores <- readRDS(paste0(generated.data.folder, "comm_sev_fa_scores_newhaven_dta_us.rds"))

sld_us_loc <- sld_us_loc%>%
  sf::st_transform(crs)

grid <- sf::st_centroid(sld_us_loc) 

# load spatial context (New Haven)
neighborhoods <- sf::st_read(paste0(demography.data.folder, "NewHaven_NeighbhorhoodBoundaries.shp")) %>%
  sf::st_transform(crs)

newhaven_boundaries <- sf::st_union(neighborhoods)
spatial_context <- newhaven_boundaries

# create grid for new haven
grid_id_cntxt <- sapply(sf::st_intersects(grid, spatial_context),function(x){length(x)>0})
grid_contxt <- grid[grid_id_cntxt, ]
sld_us_loc_df <- grid_contxt
sf::st_geometry(sld_us_loc_df) <- NULL

# add community severance index
grid_contxt <- dplyr::left_join(grid_contxt[,c("GEOID20", "TotPop", "NatWalkInd")], dat_scores[,c("GEOID20","MR1_norm")], by = "GEOID20")
grid_contxt <- grid_contxt[,c("GEOID20", "TotPop", "NatWalkInd", "MR1_norm")]
colnames(grid_contxt)[which(colnames(grid_contxt) == "MR1_norm")] <- "community_severance_index"

# back to dataframe
grid_contxt_df <- grid_contxt
sf::st_geometry(grid_contxt_df) <- NULL

# 1 rank census block groups from lower to higher community severance index
comm_sev <- grid_contxt_df

comm_sev_sf <- dplyr::left_join(comm_sev, sld_us_loc[,"GEOID20"], by = "GEOID20")
comm_sev_sf <- sf::st_as_sf(comm_sev_sf)
comm_sev_sf <- sf::st_transform(comm_sev_sf, crs) 

# leave out areas over water and parks for plotting
comm_sev_sf <- sf::st_intersection(comm_sev_sf,spatial_context)
park_geiods <- sld_us_loc_df[which(sld_us_loc_df$TotPop < 20),"GEOID20"]



# Leave out areas over water by intersecting with city boundary
comm_sev_sf <- sf::st_intersection(comm_sev_sf, spatial_context)

# Filter out park areas (very low population)
library(dplyr)

park_geiods <- sld_us_loc_df %>%
  dplyr::filter(TotPop < 20) %>%
  dplyr::pull(GEOID20)

# Only filter if park GEOIDs exist
if (length(park_geiods) > 0) {
  comm_sev_sf_p <- comm_sev_sf[!comm_sev_sf$GEOID20 %in% park_geiods, ]
} else {
  comm_sev_sf_p <- comm_sev_sf
}

# Optional: confirm rows remain
cat("Remaining rows in comm_sev_sf_p:", nrow(comm_sev_sf_p), "\n")
#Remaining rows in comm_sev_sf_p: 105 

#don't need=comm_sev_sf_p <- comm_sev_sf[-which(comm_sev_sf$GEOID20 %in% park_geiods$GEOID20),]


# read road infrastructure for plotting
#faf5_network <- sf::read_sf(paste0(geometry.data.folder, "FAF5Network.gdb"))
faf5_network <- sf::read_sf("data/raw/geometry/FAF5Network/F5F_NEWHAVEN.shp")
faf5_highways <- faf5_network[which(faf5_network$F_Class %in% c(1,2,3)),]
faf5_highways <- faf5_highways %>%
  sf::st_transform(crs)

# intersect with spatial context
roads_contxt_id_fhwa <- sapply(sf::st_intersects(faf5_highways, spatial_context),function(x){length(x)>0})
roads_contxt_fhwa <- faf5_highways[roads_contxt_id_fhwa, ]
rm(faf5_highways)

# prepare map of community severance index
# manuscript Figure 5
install.packages("tmap")
install.packages(c("tmap", "sp", "sf", "raster", "terra"))

library(tmap)

tmap_mode(mode = "view")
#Not working --> tmap_options(check.and.fix = TRUE)
#tmap code for v3 (from benavides code):
#map_community_severance_ctxt <-  tm_shape(sf::st_make_valid(spatial_context)) +
#  tm_borders(alpha = 0.1, col = "black") +
#  tm_shape(comm_sev_sf_p) +
#  tm_polygons("community_severance_index", palette = 'Reds', scale=2, style = "order",
#              title = "Community Severance Index") +
#  tm_shape(roads_contxt_fhwa) +
#  tm_lines(alpha = 0.5)

#new tmap code for v4
library(tmap)

map_community_severance_ctxt <- 
  tm_shape(sf::st_make_valid(spatial_context)) +
  tm_borders(lwd = 1, col = "black", fill_alpha = 0.1) +
  tm_shape(comm_sev_sf_p) +
  tm_polygons(
    fill = "community_severance_index",  # ✅ THIS is the correct field
    fill.scale = tm_scale_rank(values = "Blues"),  # change palette if you want
    fill.legend = tm_legend(title = "Community Severance Index"),
    border.col = "black"  # optional: add this to get visible polygon borders
  ) +
  tm_shape(roads_contxt_fhwa) +
  tm_lines(col_alpha = 0.5)


#aline troubeshooting why I keep getting the error: -------------
#> map_community_severance_ctxt
#Error in sfc[[1]] : subscript out of bounds
any(sf::st_is_empty(sf::st_geometry(spatial_context)))
any(sf::st_is_empty(sf::st_geometry(comm_sev_sf_p)))
any(sf::st_is_empty(sf::st_geometry(roads_contxt_fhwa)))
#all false = good

sf::st_crs(spatial_context)
sf::st_crs(comm_sev_sf_p)
sf::st_crs(roads_contxt_fhwa)
#all CRS match = good

summary(spatial_context)
summary(comm_sev_sf_p)
#🔥 comm_sev_sf_p has zero rows — it's empty.
#It looks like the data was wiped out when I did the parks geoid step
summary(roads_contxt_fhwa)

#---------------


# 
tmap_save(map_community_severance_ctxt, paste0(output.folder, "map_community_severance_index_newhaven.html"))

# read barrier factors for plotting

barrier_factors <- readRDS(paste0(generated.data.folder, "barrier_sp_units_newhaven_for_use.rds"))
comm_sev_sf_p <- dplyr::left_join(comm_sev_sf_p, barrier_factors)

#FINISH HERE
# manuscript Figure 2
map_barrier_factor_ctxt <- 
  tm_shape(sf::st_make_valid(spatial_context)) +
  tm_borders(lwd = 1, col = "black", fill_alpha = 0.1) + 
  tm_shape(comm_sev_sf_p) +
  tm_polygons(
    fill = "barrier_factor_fhwa",
    fill.scale = tm_scale_rank(values = "Oranges"),
    fill.legend = tm_legend(title = "Barrier Factor"),
    border.col = "black"
  ) +
  tm_shape(roads_contxt_fhwa) +
  tm_lines(col_alpha = 0.5)



tmap_save(map_barrier_factor_ctxt, paste0(output.folder, "map_barrier_factor_newhaven.html"))
