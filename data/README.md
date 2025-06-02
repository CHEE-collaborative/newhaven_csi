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
- `nation`
  - Folder containing shapefile (`.shp`) and supporting files for United States boundaries data.
- `geofabrik_connecticut-latest.gpkg`
  - Geopackage (`.gpkg`) containing OpenStreetMap data.
  - Generated in `a_06_prep_osm.R` script.
- `geofabrik_connecticut-latest.osm.pbf`
  - `osmextract` data query.
  - Generated in `a_06_prep_osm.R` script.
- `DARTE_v2.gdb`
  - Geodatabase containing traffic CO2 emission data.
- `CT_Vicinity_Town_Polygon`
  - Folder containing shapefile (`.shp`) and supporting files for Connecticut and surrounding states' town boundaries.
- `FAF5Network`
  - Folder containing shapefile (`.shp`) and supporting files for Freight Analysis Framework 5.0 Model Network Database data.
- `NH_Sidewalk`
  - Folder containing shapefile (`.shp`) and supporting files for New Haven sidewalk data.
- `Roadway_Designated_Urban_Area`
  - Folder containing shapefile (`.shp`) and supporting files for New Haven roads as urban area data.

## output/
- a_01/
  - `sf_sld_variables_proj.rds`: SLD data filtered to 44 variables of interest and projected to Connecticut CRS. `sf`
  - `df_sld.rds`: SLD variable names, descriptions, and sources. `data.frame`

- a_02/
  - `sf_barrier_factors_nh.rds`: Barrier factor indices for census block groups calculated with OpenStreetMap and Freight Analysis Framework 5.0 Model Network Database datasets. `sf`

- a_03/
  - `sf_aadt_proj.rds`:  US Average Annual Daily Traffic (AADT) data from ESRI projected to Connecticut crs. `sf`
      [BUG] See `a_03_prep_traffic.R` Line 30
  - `sf_ct_hpms.rds`: Connecticut Highway Performance Monitoring System (HPMS) data projectedto Connecticut crs. `sf`

- a_04/
  - `sf_grid_nh.rds`: New Haven census block group centroids projected to Connecticut crs. `sf`
  - `sf_regrid_aadt_cbg.rds`: Traffic intensity data (counts) interpolated to census block groups. `sf`

- a_05/
  - `sf_regrid_hpms_cbg.rds.rds`: Traffic intensity (segments) data interpolated to census block groups. [BUG] See `a_05_traffic_segment_hpms_to_cbg.R` for missing AADT data values during interpolation.

- a_06/
  - `sf_osm_nh.rds`: OpenStreetMap driving network data for New Haven, projected to Connecticut crs. `sf`

- a_07/
  - `sf_grid_prox.rds`: Proximity to road type metrics calculated for census block groups and projected to Connecticut crs. `sf`

- a_08/
  - `sf_co2_emis.rds`: Traffic CO2 emissions per m^2 calculated for census block groups and projected to Connecticut crs. `sf`

- a_09/
  - `sf_csi_scale.rds`: CSI variables at the census block group level for New Haven boundary, scaled according to standard deviation, with spatial geometries (`POINT`). `sf`
  - `df_csi_geoid_scale.rds`: CSI variables at the census block group level for New Haven boundary, scaled according to standard deviation, without spatial geometries. `df`

- b_01/
  - `sf_csi_nh.rds`: CSI values and raw variables at the census block group level for New Haven boundary, with spatial geometries (`POINT`). `sf`
  - `sf_csi_nh.csv`: CSI values and raw variables saved as tabular data (CSV) file.
  
- c_01/
  - `sf_csi_polygons.rds`: CSI values, raw variables, and scaled variables at the census block group level for New Haven boundary, merged with polygon spatial geometries (`POLYGON`). `sf`
  - `sf_faf5_123_nh.rds`: FAF5 values filtered to primary, secondary, and tertiary roads for New Haven town boundary. `sf`

- d_01/
  - `sf_tree.rds`: Percentage tree cover for census block groups. `sf`

- d_02/
  - `sf_no2_proj.rds`: Average NO2 concentration in year 2019 for census block groups. `sf`

- d_03/
  - `sf_pm25_proj.rds`: Average PM2.5 concentration in year 2019 for census block groups. `sf`

- d_04/
  - `sf_cdd.rds`: Total Cooling Degree Days in 2019 for census block groups. `sf`

- d_05/
  - `sf_demographic_nh.rds`: Demographic data for New Haven census block grooups for 2019. `sf`

- d_06/
  - `sf_csi_cond.rds`: CSI data merged with all environmental and demographic conditions variables. `sf`
