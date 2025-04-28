# Prepare traffic dataset from ESRI US Traffic Counts data.

################################################################################
# Install `remotes` and `esri2sf` packages.
install.packages("remotes")
remotes::install_github("yonghah/esri2sf")
library(esri2sf)

################################################################################
# Declare root directory, folder locations and load essential stuff
dir_home <- file.path(here::here())
dir_data <- file.path(dir_home, "data")
dir_input <- file.path(dir_data, "input")
dir_output <- file.path(dir_data, "output")

###########################################################################
# ESRI Annual Average Daily Traffic (AADT) data.
aadt_url <- paste0(
  "https://demographics5.arcgis.com/arcgis/rest/services/",
  "USA_Traffic_Counts/MapServer/0"
)
aadt_token <- paste0(
  "3NKHt6i2urmWtqOuugvr9fPXBXVMiki1uZgz7QlFUaE79bj8GbAK5-M",
  "PXVw12lK9yNU9LSRLyPuJAjbbNathsnaniGazpY3GFNaESDp8GcEn7wf",
  "82X8Ltz8o5J_MUkXSTs8Zhmd29wEFdlM3b4gdDT24tGTNBLJE8C4WNSn",
  "wQYrkHkcBkuU-9JiFyL_GNgjkEswS6u3aYE03ZodBOamlAd1oFqfa8xI9",
  "_m_eSezcbMDlcuWDyLHkRZ4vI7WPMpq_"
)
aadt_df <- esri2sf::esri2sf(aadt_url, token = aadt_token)
# [BUG] Link unaccesable outside of Yale ArcGIS

###########################################################################
# FHWA traffic segment aadt
url_ct <- "https://geo.dot.gov/server/rest/services/Hosted/HPMS_FULL_CT_2022/FeatureServer/0"
df_ct <- esri2sf::esri2sf(url_ct)
# Alex Nechaev retrieved this using the supercomputing cluster.

################################################################################
# Save output.
aadt_path <- file.path(dir_output, "a_02", "traffic_counts_esri.rds")
if (!file.exists(aadt_path)) saveRDS(aadt_df, aadt_path)

fhwa_path <- file.path(dir_output, "a_02", "aadt_ct_2022.rds")
if (!file.exists(fhwa_path)) saveRDS(fhwa_path, df_ct)
