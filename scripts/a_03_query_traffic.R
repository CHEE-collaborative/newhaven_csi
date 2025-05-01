################################################################################
# Prepare traffic datasets (AADT and HPMS) from ESRI repositories.

################################################################################
# Source variables from a_00_initiate.R
source(file.path(here::here(), "scripts", "a_00_initiate.R"))

###########################################################################
# ESRI Annual Average Daily Traffic (AADT) data.
chr_aadt_url <- paste0(
  "https://demographics5.arcgis.com/arcgis/rest/services/",
  "USA_Traffic_Counts/MapServer/0"
)
#####
##### [BUG] needs stable ArcGIS token generation
#####
chr_aadt_token <- paste0(
  "3NKHt6i2urmWtqOuugvr9fPXBXVMiki1uZgz7QlFUaE79bj8GbAK5-M",
  "PXVw12lK9yNU9LSRLyPuJAjbbNathsnaniGazpY3GFNaESDp8GcEn7wf",
  "82X8Ltz8o5J_MUkXSTs8Zhmd29wEFdlM3b4gdDT24tGTNBLJE8C4WNSn",
  "wQYrkHkcBkuU-9JiFyL_GNgjkEswS6u3aYE03ZodBOamlAd1oFqfa8xI9",
  "_m_eSezcbMDlcuWDyLHkRZ4vI7WPMpq_"
)
sf_aadt <- esri2sf::esri2sf(chr_aadt_url, token = chr_aadt_token)
sf_aadt_proj <- sf::st_transform(sf_aadt, crs = int_crs_ct)

###########################################################################
# Connecticut Highway Performance Monitoring System (HPMS) data for 2022.
chr_ct_hpms_url <- paste0(
  "https://geo.dot.gov/server/rest/services/Hosted/HPMS_FULL_CT_2022/",
  "FeatureServer/0"
)
sf_ct_hpms <- esri2sf::esri2sf(chr_ct_hpms_url)
sf_ct_hpms_proj <- sf::st_transform(sf_ct_hpms, crs = int_crs_ct)

################################################################################
# Save output.
chr_aadt_path <- file.path(dir_output, "a_03", "sf_aadt_proj.rds")
if (!file.exists(chr_aadt_path)) saveRDS(sf_aadt_proj, chr_aadt_path)

chr_ct_hpms_path <- file.path(dir_output, "a_03", "sf_ct_hpms_proj.rds")
if (!file.exists(chr_ct_hpms_path)) saveRDS(sf_ct_hpms_proj, chr_ct_hpms_path)
