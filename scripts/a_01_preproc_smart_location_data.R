# ALINE: I DID NOT CHANGE ANYTHING IN THIS SCRIPT FROM THE ORIGINAL CODE
# FROM BENAVIDES:
# https://github.com/jaime-benavides/community_severance_nyc/blob/main/code/data_prep/a_01_preproc_smart_location_dta.R
# Prepare spatial data sets from Smart Location Database.

################################################################################
# Declare root directory, folder locations and load essential stuff
dir_home <- file.path(here::here())
dir_data <- file.path(dir_home, "data")
dir_input <- file.path(dir_data, "input")
dir_output <- file.path(dir_data, "output")

################################################################################
# Import data from smart location database (from Benavides readme)
sld_zip_path <- file.path(dir_data, "input", "SmartLocationDatabaseV3.zip")
sld_path <- file.path(dir_data, "input",  "SmartLocationDatabase.gdb")
if (!file.exists(sld_path) && file.exists(sld_zip_path)) {
  unzip(sld_zip_path, exdir = file.path(dir_data, "input"))
}
testthat::expect_true(file.exists(sld_path))
sld_us <- sf::read_sf(sld_path)
class(sld_us)
dim(sld_us)
names(sld_us)

################################################################################
# Filter to selection of urban spatial variables
# Details in table 1 of manuscript
# (Jaime, you can add more details or delete if it's incorrect)
# Unique variable names.
var_name <- c(
  "GEOID20",
  "STATEFP",
  "COUNTYFP",
  "TRACTCE",
  "BLKGRPCE",
  "Ac_Total",
  "Ac_Unpr",
  "TotPop",
  "CountHU",
  "HH",
  "P_WrkAge",
  "AutoOwn0",
  "AutoOwn1",
  "AutoOwn2p",
  "Workers",
  "R_LowWageWk",
  "R_MedWageWk",
  "R_HiWageWk",
  "D1A",
  "D1B",
  "D1C",
  "D2B_E8MIXA",
  "D2A_EPHHM",
  "D3A",
  "D3AAO",
  "D3AMM",
  "D3APO",
  "D3B",
  "D3BAO",
  "D3BMM3",
  "D3BMM4",
  "D3BPO3",
  "D3BPO4",
  "D4A",
  "D5AR",
  "D5AE",
  "D5BR",
  "D5BE",
  "D2A_Ranked",
  "D2B_Ranked",
  "D3B_Ranked",
  "D4A_Ranked",
  "NatWalkInd"
)

# Unique variable descriptoins
desc <- c(
  "Census block group 12-digit FIPS code (2018)",
  "State FIPS code", "County FIPS code",
  "Census tract FIPS code in which CBG resides",
  "Census block group FIPS code in which CBG resides",
  "Total geometric area (acres) of the CBG",
  "Total land area (acres) that is not protected from development (i.e., not a park, natural area or conservation area)",
  "Population, 2018",
  "Housing units, 2018",
  "Households (occupied housing units), 2018",
  "Percent of population that is working aged 18 to 64 years, 2018",
  "Number of households in CBG that own zero automobiles, 2018",
  "Number of households in CBG that own one automobile, 2018",
  "Number of households in CBG that own two or more automobiles, 2018",
  "Count of workers in CBG (home location), 2017",
  "Count of workers earning $1250/month or less (home location), 2017",
  "Count of workers earning more than $1250/month but less than $3333/month (home location), 2017",
  "Count of workers earning $3333/month or more (home location), 2017",
  "Gross residential density (HU/acre) on unprotected land",
  "Gross population density (people/acre) on unprotected land",
  "Gross employment density (jobs/acre) on unprotected land",
  "8-tier employment entropy (denominator set to the static 8 employment types in the CBG)",
  "Employment and household entropy", 
  "Total road network density",
  "Network density in terms of facility miles of auto-oriented links per square mile",
  "Network density in terms of facility miles of multi-modal links per square mile",
  "Network density in terms of facility miles of pedestrian-oriented links per square mile",
  "Street intersection density (weighted, auto-oriented intersections eliminated)",
  "Intersection density in terms of auto-oriented intersections per square mile",
  "Intersection density in terms of multi-modal intersections having three legs per square mile",
  "Intersection density in terms of multi-modal intersections having four or more legs per square mile",
  "Intersection density in terms of pedestrian-oriented intersections having three legs per square mile",
  "Intersection density in terms of pedestrian-oriented intersections having four or more legs per square mile",
  "Distance from the population-weighted centroid to nearest transit stop (meters)",
  "Jobs within 45 minutes auto travel time, time- decay (network travel time) weighted)",
  "Working age population within 45 minutes auto travel time, time-decay (network travel time) weighted",
  "Jobs within 45-minute transit commute, distance decay (walk network travel time, GTFS schedules) weighted",
  "Working age population within 45-minute transit commute, time decay (walk network travel time, GTFS schedules) weighted",
  "Quantile ranked order (1-20) of [D2a_EpHHm] from lowest to highest",
  "Quantile ranked order (1-20) of [D2b_E8MixA] from lowest to highest",
  "Quantile ranked order (1-20) of [D3b] from lowest to highest",
  "Quantile ranked order (1,13-20) of [D4a] from lowest to highest",
  "Walkability Index"
)

# Data sources.
source <- c(
  "2019 Census TIGER/Line",
  "2019 Census TIGER/Line",
  "2019 Census TIGER/Line",
  "2019 Census TIGER/Line",
  "2019 Census TIGER/Line",
  # if source is not unique, write sld for smart location database, where
  # source info can be found
  "sld",
  "sld",
  "2018 Census ACS (5-Year Estimate)",
  "2018 Census ACS (5-Year Estimate)",
  "2018 Census ACS (5-Year Estimate)",
  "2018 Census ACS (5-Year Estimate)",
  "2018 Census ACS (5-Year Estimate)",
  "2018 Census ACS (5-Year Estimate)",
  "2018 Census ACS (5-Year Estimate)",
  "2017 Census LEHD RAC",
  "2017 Census LEHD RAC",
  "2017 Census LEHD RAC",
  "2017 Census LEHD RAC",
  "sld",
  "sld",
  "sld",
  "sld",
  "sld",
  "2018 HERE Maps NAVSTREETS",
  "2018 HERE Maps NAVSTREETS",
  "2018 HERE Maps NAVSTREETS",
  "2018 HERE Maps NAVSTREETS",
  "2018 HERE Maps NAVSTREETS",
  "2018 HERE Maps NAVSTREETS",
  "2018 HERE Maps NAVSTREETS",
  "2018 HERE Maps NAVSTREETS",
  "2018 HERE Maps NAVSTREETS",
  "2018 HERE Maps NAVSTREETS",
  "2020 GTFS, 2020 CTOD",
  "2020 TravelTime API, 2017 Census LEHD",
  "2020 TravelTime API, 2018 Census ACS",
  "2020 TravelTime API, 2017 Census LEHD, 2020 GTFS",
  "2020 TravelTime API, 2018 Census ACS, 2020 GTFS",
  "sld",
  "sld",
  "sld",
  "sld",
  "sld"
)

################################################################################
# Data checks.
testthat::expect_true(all(var_name %in% colnames(sld_us)))
testthat::expect_true(length(var_name) == length(desc))
testthat::expect_true(length(var_name) == length(source))

data_desc <- data.frame(
  var_name = var_name,
  description = desc,
  source = source
)

################################################################################
# Subset variables from smart location dataset
sld_us_loc <- sld_us[, which(colnames(sld_us) %in% var_name)]
class(sld_us_loc)
dim(sld_us_loc)
names(sld_us_loc)

################################################################################
# Save output.
sld_us_loc_path <- file.path(
  dir_output, "a_01", "smart_location_data_subset.rds"
)
if (!file.exists(sld_us_loc_path)) saveRDS(sld_us_loc, sld_us_loc_path)
testthat::expect_true(file.exists(sld_us_loc_path))

data_desc_path <- file.path(
  dir_output, "a_01", "smart_location_data_subset_desc.rds"
)
if (!file.exists(data_desc_path)) saveRDS(data_desc, data_desc_path)
testthat::expect_true(file.exists(data_desc_path))
