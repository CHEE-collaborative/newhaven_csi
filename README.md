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
- Interpolate traffic intensity data to census block groups.
- Input(s): `data/output/a_01/smart_location_data_subset.rds`, `data/output/a_03/traffic_counts_esri.rds`, `R/regrid_ok.R`
- Output(s): 

## data/
Folder containing raw input (`input/`) and processed output (`output/`) data.
