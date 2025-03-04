#a_03_prep_traffic.R

#Packages
install.packages("remotes")
remotes::install_github("yonghah/esri2sf")
library(esri2sf)

# traffic count aadt esri
url <- "https://demographics5.arcgis.com/arcgis/rest/services/USA_Traffic_Counts/MapServer/0"
tok <- "3NKHt6i2urmWtqOuugvr9fPXBXVMiki1uZgz7QlFUaE79bj8GbAK5-MPXVw12lK9yNU9LSRLyPuJAjbbNathsnaniGazpY3GFNaESDp8GcEn7wf82X8Ltz8o5J_MUkXSTs8Zhmd29wEFdlM3b4gdDT24tGTNBLJE8C4WNSnwQYrkHkcBkuU-9JiFyL_GNgjkEswS6u3aYE03ZodBOamlAd1oFqfa8xI9_m_eSezcbMDlcuWDyLHkRZ4vI7WPMpq_"
df <- esri2sf::esri2sf(url, token = tok)
saveRDS(df, paste0(generated.data.folder, "traffic_counts_esri.rds"))
#Alex Nechaev retrieved this using the supercomputing cluster

# FHWA traffic segment aadt
url_ct <- "https://geo.dot.gov/server/rest/services/Hosted/HPMS_FULL_CT_2022/FeatureServer/0"
df_ct <- esri2sf(url_ct)
saveRDS(df, paste0(generated.data.folder, "aadt_ct_2022.rds"))
#Alex Nechaev retrieved this using the supercomputing cluster
#saved the RDS file aadt_ct_2022.rds to my generated_data
