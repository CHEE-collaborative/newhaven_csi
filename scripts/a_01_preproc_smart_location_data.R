################################################################################
# ALINE: I DID NOT CHANGE ANYTHING IN THIS SCRIPT FROM THE ORIGINAL CODE
# FROM BENAVIDES:
# https://github.com/jaime-benavides/community_severance_nyc/blob/main/code/data_prep/a_01_preproc_smart_location_dta.R
# Prepare spatial data sets from Smart Location Database (SLD).

################################################################################
# Source variables from a_00_initiate.R
source(file.path(here::here(), "scripts", "a_00_initiate.R"))

################################################################################
# Identify and unzip (if necessary) SLD data files.
chr_sld_zip_path <- file.path(dir_input, "SmartLocationDatabaseV3.zip")
chr_sld_path <- file.path(dir_input, "SmartLocationDatabase.gdb")
if (!file.exists(chr_sld_path) && file.exists(chr_sld_zip_path)) {
  unzip(chr_sld_zip_path, exdir = file.path(dir_input))
}
testthat::expect_true(file.exists(chr_sld_path))

################################################################################
# Import SLD data as an sf object and inspect metadata.
sf_sld <- sf::st_read(chr_sld_path)
class(sf_sld)
dim(sf_sld)
names(sf_sld)

################################################################################
# Define vectors of unique variable names, descriptions, and data sources.
chr_varnames <- c(
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

chr_descriptions <- c(
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

chr_sources <- c(
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
testthat::expect_true(all(chr_varnames %in% colnames(sf_sld)))
testthat::expect_true(length(chr_varnames) == length(chr_descriptions))
testthat::expect_true(length(chr_varnames) == length(chr_sources))

################################################################################
# Merge variable names, descriptions, and sources into a data frame.
df_sld <- data.frame(
  varname = chr_varnames,
  description = chr_descriptions,
  source = chr_sources
)

################################################################################
# Subset SLD to variables of interest
sf_sld_variables <- sf_sld[, which(colnames(sf_sld) %in% chr_varnames)]
class(sf_sld_variables)
dim(sf_sld_variables)
names(sf_sld_variables)

################################################################################
# Project to Connecticut CRS.
sf_sld_variables_proj <- sf::st_transform(sf_sld_variables, crs = int_crs_ct)

################################################################################
# Save output.
chr_sld_variables_path <- file.path(
  dir_output, "a_01", "sf_sld_variables_proj.rds"
)
if (!file.exists(chr_sld_variables_path)) {
  saveRDS(sf_sld_variables_proj, chr_sld_variables_path)
}
testthat::expect_true(file.exists(chr_sld_variables_path))

chr_sld_desc_path <- file.path(dir_output, "a_01", "df_sld.rds")
if (!file.exists(chr_sld_desc_path)) {
  saveRDS(df_sld, chr_sld_desc_path)
}
testthat::expect_true(file.exists(chr_sld_desc_path))
