# newhaven_csi
Remake of the Community Severance Index for New Haven based on the code by Benavides et al., 2024.

# Organization
## scripts/
`a_01_preproc_smart_location_data.R`
- Preparing spatial data sets from Smart Location Database.
- Input(s): `data/input/SmartLocationDatabase.gbd`
- Output(s): `data/output/a_01/smart_location_data_subset.rds`, `data/output/a_01/smart_location_data_subset_desc.rds`

`a_03_prep_traffic.R`
- Preparing traffic data from ESRI respoitories.
- Output(s): `data/output/a_03/aadt_ct_2022.rds`, `data/output/a_03/traffic_counts.esri.rds`

`a_04_traffic_count_esri_to_cbg.R`
- Interpolate traffic intensity (counts) data to census block groups.
- Input(s): `data/output/a_01/smart_location_data_subset.rds`, `data/output/a_03/traffic_counts_esri.rds`, `R/regrid_ok.R`
- Output(s): `data/output/a_04/traffic_count_2_grid_sld_newhaven.rds`

`a_05_traffic_segment_hpms_to_cbg.R`
- Interpolate traffic intensity (segments) data to census block groups.
- Input(s): `data/output/a_01/smart_location_data_subset.rds`, `data/output/a_03/traffic_counts_esri.rds`, `R/regrid_ok.R`
- Output(s): `data/output/a_05/traffic_segment_2_grid_sld_newhaven.rds`

`a_06_prep_osm_data.R`
- Prepare OpenStreetMaps data.
- Input(s): `data/input/nation/cb_2018_us_nation_20m.shp`, `data/input/connecticut-latest.osm.pbf`
- Output(s): `data/input/connecticut-latest.gpkg`, `data/output/a_06/osm_driving_network_northeast.rds`
