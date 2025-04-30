# newhaven_csi
Remake of the Community Severance Index for New Haven, Connecticut. Methodology based on *Development of a community severance index for urban areas in the United States: A case study in New York City* (Benavides et al, 2024) ([journal article](https://www.sciencedirect.com/science/article/pii/S0160412024001120?ref=pdf_download&fr=RR-2&rr=9387bb57b9c37f56), [GitHub repository](https://github.com/jaime-benavides/community_severance_nyc?tab=readme-ov-file)).

## scripts/
`a_00_initiate.R`
- Install and load required packages.
- Set home and input/output directories and Connecticut-specific coordinate reference system.
- Source at the beginning of each run script `source("scripts/a_00_initate.R")`

`a_01_preproc_smart_location_data.R`
- Preparing spatial data sets from Smart Location Database.
- Input(s): `data/input/SmartLocationDatabase.gbd`
- Output(s): `data/output/a_01/sf_sld_variables_proj.rds`, `data/output/a_01/df_sld.rds`

`a_02_barrier_factor_prep.R`
- Prepare barrier factor variables.
- Input(s): `data/input/connecticut-latest.osm.pbf`, `data/input/FAF5Network/F5FNEWHAVEN.shp`
- Output(s): 

`a_03_prep_traffic.R`
- Query US and Connecticut traffic data from ESRI respoitories.
- Output(s): `data/output/a_03/sf_aadt_proj.rds`, `data/output/a_03/sf_ct_hpms_proj.rds`

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

`a_07_road_infrastructure_dist_to_cbg.R`
- Generate road infrastructure input data.
- Input(s):
- Output(s):

`a_08_traffic_co2_emissions_to_cbg.R`
- Prepare traffic CO2 emissions.
- Input(s): `data/input/DARTE_v2.gdb`
- Output(s): `data/output/a_08/traffic_co2_emis_newhaven.rds`
