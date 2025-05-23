################################################################################
# Calculate area of sidewalks in census block groups.

################################################################################
# WARNING: this is the R script with the most amount of changes from the
# original code
# Aline changed:
# 1. nyc borough shapefile --> new haven neighborhood shapefile
# 2. New Haven does not have publicly accessible sidewalk GIS data
# So, replace nyc edges.shp --> homemade NH_Sidewalk.shp
# Also, based on CT building policy, all sidewalks must be at least 5 feet
# 1.524 m https://ecode360.com/28168812
# So, I forced that assumption onto our sidewalk shapefile
# 3. New Haven was working on their New Haven road dataset, so we had to
# troubleshoot we used a road dataset from:
# https://catalog.data.gov/dataset/tiger-line-shapefile-2021-county-new-haven-county-ct-all-roads
# crs matching:
# My output is EPSG:9311 (NAD27 / US National Atlas Equal Area) instead of
# EPSG:2163 (NAD83 / US National Atlas Equal Area).
# 4. I created a bike lane map using the Yale CityofNewHaven5_bikemappdf
# I mapped out bike path, bike lane, and bike route marked with shared lane
# markings from a pdf (created a shp)
# I excluded unmarked bike route

################################################################################
# Source variables from a_00_initiate.R
source(file.path(here::here(), "scripts", "a_00_initiate.R"))

################################################################################
# Import New Haven boundary.
sf_nh_boundary <- generate_newhaven(path = chr_towns_path, crs = int_crs_ct)
sf_context <- sf_nh_boundary

################################################################################
# Import grid.
chr_grid_nh <- file.path(dir_output, "a_04", "sf_grid_nh.rds")
testthat::expect_true(file.exists(chr_grid_nh))
sf_grid_nh <- readRDS(chr_grid_nh)
sf_grid_nh$id_local <- seq_len(nrow(sf_grid_nh))

################################################################################
# Import sidewalks data.
# New Haven does not have publicly accessible sidewalk GIS data
chr_sidewalk_path <- file.path(dir_input, "NH_Sidewalk", "NH_Sidewalk.shp")
testthat::expect_true(file.exists(chr_sidewalk_path))
sf_sidewalk <- sf::read_sf(chr_sidewalk_path) %>%
  sf::st_transform(crs = int_crs_ct)
testthat::expect_identical("EPSG:2234", sf::st_crs(sf_sidewalk)$input)

################################################################################
# Define sidewalk width based on city policy (5 feet).
sf_sidewalk$width <- 5.0
# LIMITATION: Width Accuracy:
# NYC: Precise width measurements (maybe via surveys or sensors).
# NH: Uniform width assumption (5.0 ft  for all).
# ^^This adds uncertainty, but it’s acceptable for exploratory or city-wide
# aggregate analysis as long as you acknowledge the assumption in methods.

################################################################################
# Calculate sidewalk length using `sf` geometry.
sf_sidewalk$length <- sf::st_length(sf_sidewalk)

################################################################################
# estimate area of sidewalk
sf_sidewalk$sidewalk_area_m <-
  as.numeric(sf_sidewalk$width) * as.numeric(sf_sidewalk$length)

################################################################################
# Import New Haven roads to estimate area occupied by roads.
chr_streets_path <- file.path(
  dir_input,
  "Roadway_Designated_Urban_Area",
  "Roadway_Designated_Urban_Area.shp"
)
testthat::expect_true(file.exists(chr_streets_path))
sf_streets <- sf::read_sf(chr_streets_path) %>%
  sf::st_transform(crs = int_crs_ct)
testthat::expect_identical("EPSG:2234", sf::st_crs(sf_streets)$input)

################################################################################
# Ensure length values are in US Feet to match Connecticut crs.
sf::st_length(sf_streets)[1:100]
sf_streets$shape_len_ft <- sf_streets$Shape__Len

################################################################################
# Assign road width if not present (18-24 feet typical for two-lane road).
sf_streets$shape_wid_ft <- (18 + 24) / 2

################################################################################
# Calculate area of streets.
sf_streets$st_area <- sf_streets$shape_len_ft * sf_streets$shape_wid_ft












################################################################################
# Read in bike lanes separately
bike_lanes <- sf::st_read("data/raw/geometry/bikelanes_2163.shp") %>%
  sf::st_transform(crs)

################################################################################
sf::st_crs(bike_lanes)

################################################################################
colnames(bike_lanes)

################################################################################
# bike lane area (assuming 5 feet = 1.524 meters width)
# The minimum width of a bike lane in New Haven, Connecticut is usually 5 feet,
# but may be 4 feet in some locations
bike_lanes$bike_width_m <- 1.524  # standard bike lane width
bike_lanes$length_m <- bike_lanes$Shape_Leng
bike_lanes$length_m <- as.numeric(bike_lanes$length_m)

################################################################################
# Calculate area
bike_lanes$bike_lane_area <- bike_lanes$length_m * bike_lanes$bike_width_m

################################################################################
# Just double-checking by length shape for bike lanes
head(cbind(bike_lanes$Shape_Leng, sf::st_length(bike_lanes)))

################################################################################
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

################################################################################
# I had to remove the "nyc" from this function in the functions.R file
# + had to adapt the function a bit cause my bike lanes are not connected to my road data
# The function:
estim_areal_road_ped_ratio  <- function(sp_unit_pos) {
  sp_unit <- grid_contxt[sp_unit_pos[1], ]
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
  return(areal_ratio)
}

################################################################################
areal_road_ped_ratio_coll <- data.frame()
for (grp_n in 1:10) {
  grp <- get(paste0("group_", grp_n))
  print(paste0("working on group ", grp_n))

  sp_unit_df <- as.data.frame(grid_contxt[grp,])
  sp_unit_df$sp_unit_pos <- grp
  sp_unit_ap <- as.array(unlist(sp_unit_df[,c("sp_unit_pos", "id_local")]))
  dim(sp_unit_ap) <- c(sp_unit_pos = nrow(sp_unit_df), col = 2)

  # function estim_areal_road_ped_ratio should be in your functions.R
  areal_road_ped_ratio <- multiApply::Apply(
    list(sp_unit_ap),
    target_dims = "col",
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

################################################################################
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
