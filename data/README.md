# data/
This folder contains all input and output (intermidate and final) data for New Haven Community Severance Index. Large data files are ignored from GitHub, but a zip file containing these files can be found at [LINK]. All data file paths used in the `scripts/*.R` files are relative to the root directory, so to access the data files and put in the correct location, run the following in R:

```r
download.file("this_is_a_link_placeholder.zip", "USER_PATH/Downloads/newhaven_csi_data.zip")
unzip("USER_PATH/Downloads/newhaven_csi_data.zip", exdir = "USER_PATH/newhaven_csi/data/")
```

## input/
- `SmartLocationDatabaseV3.zip`
  - Raw US EPA Smart Location Database (SLD) zip file
  - Data is compressed.
  - Unzipped in `a_01_preproc_smart_location_data.R` into `input/` folder (unzipping creates `SmartLocationDatabase.gdb` and technical documentation PDF)
- `NewHaven_NeighbhorhoodBoundaries`
  - Folder containing shapefile (`.shp`) and supporting files for New Haven neighborhood boundaries data.

## output/
- a_01/
  - `smart_location_data_subset.rds`: SLD filtered to 44 variables of interest
  - `smart_location_data_subset_desc.rds`: Variable descriptions and sources for SLD variables
- a_03/
  - `aadt_ct_2022.rds`: [BUG] See `a_03_prep_traffic.R` Line 30
  - `traffic_counts_esri.rds`: Average Annual Daily Traffic (AADT) data from ESRI
- a_04/
  - `traffic_count_2_grid_sld_newhaven.rds`: Traffic intensity data interpolated to census block groups.
  