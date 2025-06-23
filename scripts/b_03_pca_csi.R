################################################################################
# Calculate Community Severance Index (CSI) for New Haven.

################################################################################
# Source variables from a_00_initiate.R
source(file.path(here::here(), "scripts", "a_00_initiate.R"))

################################################################################
# Import CSI variable data.
chr_desc_path <- file.path(dir_output, "a_01", "df_sld.rds")
testthat::expect_true(file.exists(chr_desc_path))
df_desc <- readRDS(chr_desc_path)

chr_csi_scale_path <- file.path(dir_output, "a_09", "sf_csi_scale.rds")
testthat::expect_true(file.exists(chr_csi_scale_path))
sf_csi_scale <- readRDS(chr_csi_scale_path)

chr_csi_raw_path <- file.path(dir_output, "a_09", "sf_csi_raw.rds")
testthat::expect_true(file.exists(chr_csi_raw_path))
sf_csi_raw <- readRDS(chr_csi_raw_path)

################################################################################
# Transform sf_csi_scale to matrix.
mat_csi_scale <- as.matrix(sf::st_drop_geometry(sf_csi_scale[, -1]))

################################################################################
# Import New Haven boundary.
sf_nh_boundary <- generate_newhaven(path = chr_towns_path, crs = int_crs_ct)
sf_context <- sf_nh_boundary

################################################################################
# Import New Haven polygons.
sf_ct_2019 <- tigris::block_groups(
  state = "CT",
  county = "New Haven",
  cb = TRUE,
  year = 2019
) %>%
  sf::st_transform(crs = int_crs_ct)
sf_nh_2019 <- sf::st_intersection(sf_ct_2019, sf_context)
sf_nh_polygons <- sf_nh_2019[, "GEOID"]
names(sf_nh_polygons)[1] <- "GEOID20"

################################################################################
# Merge CSI data with New Haven polygons.
sf_csi_scale_polygons <- merge(
  sf_nh_polygons,
  sf::st_drop_geometry(sf_csi_scale),
  by = "GEOID20"
)

################################################################################
# Classify variables.
chr_varnames <- c(
  "autom_netw_dens",
  "autom_inters_dens",
  "barrier_factor_osm",
  "barrier_factor_faf5",
  "motorway_prox",
  "primary_prox",
  "secondary_prox",
  "trunk_prox",
  "interstate_highway_prox",
  "freeways_expressways_prox",
  "other_princ_arter_prox",
  "tertiary_prox",
  "residential_prox",
  "aadt_esri_point",
  "aadt_fhwa_segm",
  "co2_emis_per_m2",
  "pedest_netw_dens",
  "street_no_autom_inters_dens",
  "NatWalkInd"
)
chr_varcat <- c(
  "Road infrastructure",
  "Road infrastructure",
  "Road infrastructure",
  "Road infrastructure",
  "Road infrastructure",
  "Road infrastructure",
  "Road infrastructure",
  "Road infrastructure",
  "Road infrastructure",
  "Road infrastructure",
  "Road infrastructure",
  "Road infrastructure",
  "Road infrastructure",
  "Road traffic activity",
  "Road traffic activity",
  "Road traffic activity",
  "Pedestrian infrastructure",
  "Pedestrian infrastructure",
  "Pedestrian infrastructure"
)
df_var_namecat <- data.frame(
  variable = chr_varnames,
  category = chr_varcat
)

################################################################################
# Plot raw data matrix correlations.
cor(mat_csi_scale)
mat_csi_scale %>%
  GGally::ggcorr(
    .,
    method = c("pairwise.complete.obs", "pearson"),
    label = TRUE,
    label_size = 3,
    label_alpha = TRUE,
    hjust = 1,
    nbreaks = 10,
    limits = TRUE,
    size = 4,
    layout.exp = 5
  ) +
  ggplot2::ggtitle("Raw data matrix Pearson Correlation")

################################################################################
################################################################################
# Unsupervised PCA.
prcomp_csi <- stats::prcomp(mat_csi_scale)
attributes(prcomp_csi)
print(prcomp_csi)
summary(prcomp_csi)
factoextra::fviz_screeplot(prcomp_csi, addlabels = TRUE)
################################################################################
################################################################################

################################################################################
# Import Connecticut crash data.
chr_crash_paths <- list.files(
  file.path(dir_input, "ct_car_crash_data"),
  recursive = TRUE,
  full.names = TRUE,
  pattern = "\\.csv"
)
chr_crash_paths
for (c in seq_along(chr_crash_paths)) {
  assign(paste0("df_crash_", c), read.csv(chr_crash_paths[c]))
}

################################################################################
# Check for lon/lat information
grep("lon|longitude|Longitude", names(df_crash_1))
grep("lon|longitude|Longitude", names(df_crash_2))
grep("lon|longitude|Longitude", names(df_crash_3))
grep("lon|longitude|Longitude", names(df_crash_4))

################################################################################
# Convert main crash data `df_crash_1` to `sf`.
sf_crash <- sf::st_as_sf(
  df_crash_1,
  coords = c("Longitude", "Latitude"),
  crs = 4326
) |>
  sf::st_transform(crs = sf::st_crs(sf_csi_scale_polygons))

################################################################################
# Pedestrian-involved crashes.
sf_crash_ped <- sf_crash[sf_crash$Number.Of.Non.Motorist > 0, ]

################################################################################
# Count number of crashes per census block group.
list_csi_crash <- sf::st_intersects(sf_csi_scale_polygons, sf_crash)
sf_csi_scale_polygons$crashes <- lengths(list_csi_crash)
list_csi_crash_ped <- sf::st_intersects(sf_csi_scale_polygons, sf_crash_ped)
sf_csi_scale_polygons$crashes_ped <- lengths(list_csi_crash_ped)

################################################################################
# Convert to data.frame.
df_csi_scale_crashes <- sf::st_drop_geometry(sf_csi_scale_polygons)

################################################################################
################################################################################
# Supervised PCA.
library(superpc)
grep("GEOID|crashes", names(df_csi_scale_crashes))

list_pca_csi <- list(
  x = t(as.matrix(
    df_csi_scale_crashes[
      , grep("GEOID20|crashes", names(df_csi_scale_crashes), invert = TRUE)
    ]
  )),
  y = as.matrix(df_csi_scale_crashes$crashes),
  censoring.status = NULL
)

train_csi <- superpc::superpc.train(data = list_pca_csi, type = "regression")


cv_csi <- superpc::superpc.cv(train_csi, list_pca_csi)
superpc::superpc.plotcv(cv_csi)

################################################################################
################################################################################


?rep
gsub("a", NULL, rep("a", 5))
