#a_09_put_together_inputs_to_csi.R

#Aline changed:
#1. Nomenclature: nyc --> new haven
#2. ## Thus, No need for this step if -99999 is not present
#sld_us_loc_df <- sld_us_loc_df %>%
#dplyr::mutate(across(everything(), ~ dplyr::na_if(.x, -99999)))

# script aim: put together inputs to run community severance index over NYC
# First step to load packages etc.
# 1a Declare root directory, folder locations and load essential stuff
project.folder = paste0(print(here::here()),'/')
source(paste0(project.folder,'init_directory_structure.R'))
source(paste0(functions.folder,'script_initiate.R'))

# set coordinate reference system
crs <- 2163


# read data inputs
traffic_count_2_grid_sld_newhaven <- readRDS(paste0(generated.data.folder, "traffic_count_2_grid_sld_newhaven.rds"))
traffic_segment_2_grid_sld_newhaven <- readRDS(paste0(generated.data.folder, "traffic_segment_2_grid_sld_newhaven.rds"))
road_inf_dist_2_grid_sld_newhaven <- readRDS(paste0(generated.data.folder, "road_inf_dist_2_grid_sld_newhaven.rds"))
barrier_factor_newhaven <- readRDS(paste0(generated.data.folder, "barrier_sp_units_newhaven_for_use.rds"))
traffic_co2_emis_newhaven <- readRDS(paste0(generated.data.folder, "traffic_co2_emis_newhaven.rds"))

# load grids and smart location dataset subset
data_desc <- readRDS(paste0(generated.data.folder, "smart_location_data_subset_desc.rds"))
sld_us_loc <- readRDS(paste0(generated.data.folder, "smart_location_data_subset.rds"))
grid <- sf::st_centroid(sld_us_loc) %>%
  sf::st_transform(crs)

# load testing areas
# obtain a spatial context for an example
neighborhoods <- sf::st_read(paste0(demography.data.folder, "NewHaven_NeighbhorhoodBoundaries.shp")) %>%
  sf::st_transform(crs)

newhaven_boundaries <- sf::st_union(neighborhoods)
spatial_context <- newhaven_boundaries

# read urban grid
grid_id_cntxt <- sapply(sf::st_intersects(grid, spatial_context),function(x){length(x)>0})
grid_contxt <- grid[grid_id_cntxt, ]
sld_us_loc_df <- grid_contxt
sf::st_geometry(sld_us_loc_df) <- NULL
# subset data for community severance index estimation
sld_us_loc_df <- sld_us_loc_df[,c("GEOID20",  
                                  "D3AAO", "D3APO", 
                                  "D3B", "D3BAO", "NatWalkInd")]
colnames(sld_us_loc_df)[c(2:5)] <- c("autom_netw_dens", "pedest_netw_dens", "street_no_autom_inters_dens", "autom_inters_dens")

sld_us_loc_df <- as.data.frame(sld_us_loc_df)

#I got an error to the line below:
#Error in `dplyr::na_if()`:
#! Can't convert `y` <double> to match type of `x` <data.frame>.
#Run `rlang::last_trace()` to see where the error occurred.
#sld_us_loc_df <- dplyr::na_if(sld_us_loc_df, -99999)
#-----------------------
#So, I checked if there were any -99999 values in the dataset:
# Find columns containing -99999
cols_with_neg99999 <- sld_us_loc_df %>%
  summarise(across(where(is.numeric), ~ sum(. == -99999, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Column", values_to = "Count") %>%
  filter(Count > 0)
# Display the results
print(cols_with_neg99999)
#There are no -99999 values
summary(sld_us_loc_df)
#There are no -99999 values
> summary(sld_us_loc)
#the only columns with -99999 are D5BR and D5BE, neither of which are in the subsetted dataset
colnames(sld_us_loc_df)
#[1] "GEOID20"                     "autom_netw_dens"             "pedest_netw_dens"           
#[4] "street_no_autom_inters_dens" "autom_inters_dens"           "NatWalkInd" 

#---------------
## Thus, No need for this step if -99999 is not present
#sld_us_loc_df <- sld_us_loc_df %>%
  #dplyr::mutate(across(everything(), ~ dplyr::na_if(.x, -99999)))

# join data
traffic_count_2_grid_sld_newhaven_df <- traffic_count_2_grid_sld_newhaven
sf::st_geometry(traffic_count_2_grid_sld_newhaven_df) <- NULL
traffic_count_2_grid_sld_newhaven_df <- traffic_count_2_grid_sld_newhaven_df[,c("GEOID20", "aadt")]
colnames(traffic_count_2_grid_sld_newhaven_df) <- c("GEOID20", "aadt_esri_point")

traffic_segment_2_grid_sld_newhaven_df <- traffic_segment_2_grid_sld_newhaven
sf::st_geometry(traffic_segment_2_grid_sld_newhaven_df) <- NULL
traffic_segment_2_grid_sld_newhaven_df <- traffic_segment_2_grid_sld_newhaven_df[,c("GEOID20", "aadt")]
colnames(traffic_segment_2_grid_sld_newhaven_df) <- c("GEOID20", "aadt_fhwa_segm")


barrier_factor_newhaven <- barrier_factor_newhaven[,c("GEOID20", "barrier_factor_osm", "barrier_factor_fhwa")]

road_inf_dist_2_grid_sld_newhaven_df <- road_inf_dist_2_grid_sld_newhaven
sf::st_geometry(road_inf_dist_2_grid_sld_newhaven_df) <- NULL

data_in_cs <- dplyr::left_join(sld_us_loc_df, traffic_count_2_grid_sld_newhaven_df, by = "GEOID20") %>%
  dplyr::left_join(traffic_segment_2_grid_sld_newhaven_df, by = "GEOID20") %>%
  dplyr::left_join(road_inf_dist_2_grid_sld_newhaven_df, by = "GEOID20") %>%
  dplyr::left_join(traffic_co2_emis_newhaven, by = "GEOID20") %>%
  dplyr::left_join(barrier_factor_newhaven, by = "GEOID20")

# manuscript Table 2
# explore summary descriptive
summ_data_in_cs <- sumtable(data_in_cs, out = "return")
colnames(data_desc)[1] <- "Variable"
summ_data_in_cs <- dplyr::left_join(summ_data_in_cs, data_desc, by = "Variable")

# homogenize data ranges by scaling by standard deviation
data_in_cs_id <- data_in_cs[,c("GEOID20")]
dta <- data_in_cs[ , -c(which(colnames(data_in_cs) == "GEOID20"))]
dta_scaled = as.data.frame(apply(dta, 2, function(a) a/sd(a, na.rm = T)))
dta_prep <- cbind(data_in_cs_id, dta_scaled)
colnames(dta_prep)[1] <- "GEOID20"

# build dataframe for distributional characteristics (paper table)
# delete variables distance to road and add proximity based

vtable::st(dta, add.median = T,fit.page = '\\textwidth', digits = 2, out = 'latex')


# save data
dta_prep <- cbind(data_in_cs_id, dta_scaled)
colnames(dta_prep)[1] <- "GEOID20"
saveRDS(dta_prep, paste0(generated.data.folder, "community_severance_newhaven_input_data.rds"))
dta_prep <- cbind(data_in_cs_id, dta_scaled)
colnames(dta_prep)[1] <- "GEOID20"
saveRDS(dta_prep, "generated_data/community_severance_newhaven_input_data.rds")
