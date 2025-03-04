# ALINE: I DID NOT CHANGE ANYTHING IN THIS SCRIPT FROM THE ORIGINAL CODE FRM BENAVIDES
# script aim: This script is preparing spatial data sets for this study from Smart Location Database
# First step to load packages etc.
# 1a Declare root directory, folder locations and load essential stuff
project.folder = paste0(print(here::here()),'/')
source(paste0(project.folder,'init_directory_structure.R'))
source(paste0(functions.folder,'script_initiate.R'))

### set resolution
crs <- 2163 #


# data from smart location database (from Benavides readme)
sld_us <- sf::read_sf(paste0(geometry.data.folder, "SmartLocationDatabase.gdb"))

#Selection of urban spatial variables, details in table 1 of manuscript (Jaime, you can add more details or delete if it's incorrect)
var_name <- c("GEOID20", "STATEFP", "COUNTYFP", "TRACTCE", "BLKGRPCE",
              "Ac_Total", "Ac_Unpr", 
              "TotPop", "CountHU", "HH", "P_WrkAge", 
              "AutoOwn0", "AutoOwn1", "AutoOwn2p", 
              "Workers", "R_LowWageWk", "R_MedWageWk", "R_HiWageWk",
              "D1A", "D1B", "D1C",
              "D2B_E8MIXA", "D2A_EPHHM", 
              "D3A", "D3AAO", "D3AMM", "D3APO",
              "D3B", "D3BAO", "D3BMM3", "D3BMM4", "D3BPO3", "D3BPO4",
              "D4A",
              "D5AR", "D5AE", "D5BR", "D5BE",
              "D2A_Ranked", "D2B_Ranked", "D3B_Ranked", "D4A_Ranked", "NatWalkInd")

desc <- c("Census block group 12-digit FIPS code (2018)", "State FIPS code", "County FIPS code", "Census tract FIPS code in which CBG resides", "Census block group FIPS code in which CBG resides",
          "Total geometric area (acres) of the CBG", "Total land area (acres) that is not protected from development (i.e., not a park, natural area or conservation area)",
          "Population, 2018", "Housing units, 2018", "Households (occupied housing units), 2018", "Percent of population that is working aged 18 to 64 years, 2018",
          "Number of households in CBG that own zero automobiles, 2018", "Number of households in CBG that own one automobile, 2018", "Number of households in CBG that own two or more automobiles, 2018",
          "Count of workers in CBG (home location), 2017", "Count of workers earning $1250/month or less (home location), 2017", "Count of workers earning more than $1250/month but less than $3333/month (home location), 2017", "Count of workers earning $3333/month or more (home location), 2017",
          "Gross residential density (HU/acre) on unprotected land", "Gross population density (people/acre) on unprotected land", "Gross employment density (jobs/acre) on unprotected land", 
          "8-tier employment entropy (denominator set to the static 8 employment types in the CBG)", "Employment and household entropy", 
          "Total road network density", "Network density in terms of facility miles of auto-oriented links per square mile", "Network density in terms of facility miles of multi-modal links per square mile", "Network density in terms of facility miles of pedestrian-oriented links per square mile",
          "Street intersection density (weighted, auto-oriented intersections eliminated)", "Intersection density in terms of auto-oriented intersections per square mile", "Intersection density in terms of multi-modal intersections having three legs per square mile", "Intersection density in terms of multi-modal intersections having four or more legs per square mile", "Intersection density in terms of pedestrian-oriented intersections having three legs per square mile", "Intersection density in terms of pedestrian-oriented intersections having four or more legs per square mile",
          "Distance from the population-weighted centroid to nearest transit stop (meters)",
          "Jobs within 45 minutes auto travel time, time- decay (network travel time) weighted)", "Working age population within 45 minutes auto travel time, time-decay (network travel time) weighted", "Jobs within 45-minute transit commute, distance decay (walk network travel time, GTFS schedules) weighted", "Working age population within 45-minute transit commute, time decay (walk network travel time, GTFS schedules) weighted",
          "Quantile ranked order (1-20) of [D2a_EpHHm] from lowest to highest", "Quantile ranked order (1-20) of [D2b_E8MixA] from lowest to highest", "Quantile ranked order (1-20) of [D3b] from lowest to highest", "Quantile ranked order (1,13-20) of [D4a] from lowest to highest", "Walkability Index")

source <- c("2019 Census TIGER/Line", "2019 Census TIGER/Line", "2019 Census TIGER/Line", "2019 Census TIGER/Line", "2019 Census TIGER/Line",
            "sld", "sld",  # if source is not unique, write sld for smart location database, where source info can be found
            "2018 Census ACS (5-Year Estimate)", "2018 Census ACS (5-Year Estimate)", "2018 Census ACS (5-Year Estimate)", "2018 Census ACS (5-Year Estimate)",
            "2018 Census ACS (5-Year Estimate)", "2018 Census ACS (5-Year Estimate)", "2018 Census ACS (5-Year Estimate)",
            "2017 Census LEHD RAC", "2017 Census LEHD RAC", "2017 Census LEHD RAC", "2017 Census LEHD RAC",
            "sld", "sld", "sld", 
            "sld", "sld", 
            "2018 HERE Maps NAVSTREETS", "2018 HERE Maps NAVSTREETS", "2018 HERE Maps NAVSTREETS", "2018 HERE Maps NAVSTREETS",
            "2018 HERE Maps NAVSTREETS", "2018 HERE Maps NAVSTREETS", "2018 HERE Maps NAVSTREETS", "2018 HERE Maps NAVSTREETS", "2018 HERE Maps NAVSTREETS", "2018 HERE Maps NAVSTREETS",
            "2020 GTFS, 2020 CTOD",
            "2020 TravelTime API, 2017 Census LEHD", "2020 TravelTime API, 2018 Census ACS", "2020 TravelTime API, 2017 Census LEHD, 2020 GTFS", "2020 TravelTime API, 2018 Census ACS, 2020 GTFS",
            "sld", "sld", "sld", "sld", "sld")

all(var_name %in% colnames(sld_us))
#TRUE
length(var_name) == length(desc) 
#TRUE
length(var_name)== length(source)
#TRUE

data_desc <- data.frame(var_name = var_name, description = desc, source = source)

# subset variables from smart location dataset
sld_us_loc <- sld_us[,which(colnames(sld_us) %in% var_name)]
saveRDS(sld_us_loc, paste0(generated.data.folder, "smart_location_data_subset.rds"))
saveRDS(data_desc, paste0(generated.data.folder, "smart_location_data_subset_desc.rds"))
