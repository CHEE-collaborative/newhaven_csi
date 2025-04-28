rm(list=ls())
graphics.off()

setwd("/Users/user/Documents/CSI")

library(ncdf4); library(dplyr); library(ggplot2); library(sf); library(ggmap); library(sf); library(stringr)
   # For background map (optional)
 

# List of NetCDF file names corresponding to each year
nc_files <- list(
  "2019" = "HAQ_TROPOMI_NO2_CONUS_QA75_L3_Annual_2019_V2.4_20240729.nc4",
  "2020" = "HAQ_TROPOMI_NO2_CONUS_QA75_L3_Annual_2020_V2.4_20240729.nc4",
  "2021" = "HAQ_TROPOMI_NO2_CONUS_QA75_L3_Annual_2021_V2.4_20240729.nc4",
  "2022" = "HAQ_TROPOMI_NO2_CONUS_QA75_L3_Annual_2022_V2.4_20240729.nc4",
  "2023" = "HAQ_TROPOMI_NO2_CONUS_QA75_L3_Annual_2023_V2.4_20240729.nc4",
  "2024" = "HAQ_TROPOMI_NO2_CONUS_QA75_L3_Annual_2024_V2.4_20250109.nc4"
)

# Function to read NetCDF file and extract data
process_nc_data <- function(year, file_name) {
  # Open the NetCDF file
  nc_file <- nc_open(file_name)
  
  # Extract variables
  lon <- ncvar_get(nc_file, "Longitude")
  lat <- ncvar_get(nc_file, "Latitude")
  no2 <- ncvar_get(nc_file, "Tropospheric_NO2")
  no2_obs <- ncvar_get(nc_file, "Number_obs")
  
  # Create a data frame
  df <- data.frame(
    Longitude = as.vector(lon),
    Latitude = as.vector(lat),
    NO2 = as.vector(no2),
    NO2_obs = as.vector(no2_obs),
    Year = as.numeric(year)  # Add year column for reference
  )
  
  # Close the NetCDF file
  nc_close(nc_file)
  
  return(df)
}

# Loop through the NetCDF files and store results in a list
NO2_data_list <- lapply(names(nc_files), function(year) {
  process_nc_data(year, nc_files[[year]])
})

# Combine all years into a single data frame
NO2_combined <- do.call(rbind, NO2_data_list)

# View the first few rows
head(NO2_combined)
library(dplyr)
NO2_19 = NO2_combined %>% filter(Year == 2019)

# Bounding box coordinates for New Haven, CT
lat_min <- 41.20
lat_max <- 41.45
lon_min <- -73.05
lon_max <- -72.80

# Filter NO2 data within the New Haven bounding box
NO2_NewHaven <- NO2_combined %>%
  filter(Latitude >= lat_min & Latitude <= lat_max,
         Longitude >= lon_min & Longitude <= lon_max)

# View a sample of filtered data
head(NO2_NewHaven)
# Load libraries

# Read the Census Block Group shapefile (replace with your file path)
census_blocks <- st_read("New_Haven_census_RS_AC_veg_share.shp")

#library(stringr)

library(gstat)
library(stars)
NO2_sf <- st_as_sf(NO2_NewHaven, coords = c("Longitude", "Latitude"), crs = 4326)

# Create a grid over the study area
bbox <- st_bbox(census_blocks)
grid <- st_as_stars(st_bbox(bbox), dx = 0.001, dy = 0.001)  # Adjust resolution as needed

# Perform interpolation (using IDW - Inverse Distance Weighting)
NO2_idw <- gstat(
  formula = NO2 ~ 1,
  locations = NO2_sf,
  set = list(idp = 2)  # Power parameter for IDW
) %>% predict(grid)

# Convert interpolated surface to points
NO2_interpolated <- st_as_sf(NO2_idw, as_points = TRUE) %>%
  rename(NO2 = var1.pred)



# Now perform spatial join with interpolated points
NO2_block_avg <- census_blocks %>%
  st_join(NO2_interpolated) %>%
  group_by(GEOID20) %>%
  summarise(
    avg_NO2 = mean(NO2, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  filter(!is.na(avg_NO2))

# Plot the result
p = ggplot() +
  geom_sf(data = NO2_block_avg, 
          aes(fill = avg_NO2), 
          color = "white", 
          size = 0.1) +
  scale_fill_viridis_c(
    option = "magma", 
    name = "NO2 Level",
    na.value = "grey90"
  ) +
  theme_minimal() +
  labs(
    title = "Interpolated NO2 Levels by Census Block in New Haven",
    subtitle = "Average Tropospheric NO2 Concentrations"
  )

p + scale_y_continuous(breaks = NULL) + scale_x_continuous(breaks = NULL)

p = ggplot() +
  geom_sf(data = NO2_block_avg, 
          aes(fill = avg_NO2), 
          color = "white", 
          size = 0.1) +
  scale_fill_viridis_c(
    option = "magma", 
    name = "NO2 Level",
    na.value = "grey90"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # This removes all grid lines
    panel.grid.major = element_blank(),  # This removes major grid lines
    panel.grid.minor = element_blank()   # This removes minor grid lines
  ) +
  labs(
    title = "Interpolated NO2 Levels by Census Block in New Haven",
    subtitle = "Average Tropospheric NO2 Concentrations"
  )
