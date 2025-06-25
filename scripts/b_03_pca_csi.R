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

# Sample for training and testing indices.
int_train <- sample(
  nrow(df_csi_scale_crashes),
  size = nrow(df_csi_scale_crashes) * 0.8
)
int_test <- setdiff(seq_len(nrow(df_csi_scale_crashes)), int_train)

# Split into training and testing data.
mat_csi_crashes_train <- t(
  as.matrix(
    df_csi_scale_crashes[
      int_train,
      grep("GEOID20|crashes", names(df_csi_scale_crashes), invert = TRUE)
    ]
  )
)
mat_csi_crashes_test <- t(
  as.matrix(
    df_csi_scale_crashes[
      int_test,
      grep("GEOID20|crashes", names(df_csi_scale_crashes), invert = TRUE)
    ]
  )
)

# Training data.
list_pca_train <- list(
  x = mat_csi_crashes_train,
  y = as.matrix(df_csi_scale_crashes$crashes[int_train]),
  censoring.status = NULL
)

# Testing data.
list_pca_test <- list(
  x = mat_csi_crashes_test,
  y = as.matrix(df_csi_scale_crashes$crashes[int_test]),
  censoring.status = NULL
)

# Train supervised pca.
superpc_train <- superpc::superpc.train(
  data = list_pca_train,
  type = "regression"
)

# Cross validation.
superpc_cv <- superpc::superpc.cv(superpc_train, list_pca_train)

# Plot cross validation curves.
superpc::superpc.plotcv(superpc_cv)

# Likelihood ratio statistic.
superpc_lrtest <- superpc::superpc.lrtest.curv(
  superpc_train,
  list_pca_train,
  list_pca_test
)

# Plot Likelihood ratio statistics.
superpc::superpc.plot.lrtest(superpc_lrtest)

# Predict.
superpc_pred <- superpc::superpc.predict(
  superpc_train,
  list_pca_train,
  list_pca_test,
  threshold = 0.65,
  n.components = 1,
  prediction.type = "continuous"
)

# Fit predictions to outcome variable.
superpc_fit <- superpc.fit.to.outcome(
  superpc_train,
  list_pca_test,
  superpc_pred$v.pred
)

# Plot predicted outcomes.
df_superpc <- data.frame(
  truth = list_pca_test$y,
  pred = as.numeric(superpc_pred$v.pred)
)

# Back transform PC predictions based on slope and intercept.
df_superpc$pred_crashes <-
  superpc_fit$coeftable[1, 1] +
  (superpc_fit$coeftable[2, 1] * superpc_pred$v.pred)

# Plot.
ggplot2::ggplot(df_superpc, aes(x = truth, y = pred_crashes)) +
  ggplot2::geom_point(color = "steelblue", size = 2) +
  ggplot2::geom_smooth(method = "lm", se = TRUE, color = "red") +
  ggplot2::geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    color = "gray"
  ) +
  ggplot2::labs(
    x = "Crashes",
    y = "Predicted Value (PC1 score)"
  ) +
  ggplot2::theme_minimal()


# Per 1SD increase in CSI.
superpc_fit$coeftable[2, 1] * sd(superpc_pred$v.pred)

# Full dataset for CSI values.
mat_csi_crashes <- t(
  as.matrix(
    df_csi_scale_crashes[
      grep("GEOID20|crashes", names(df_csi_scale_crashes), invert = TRUE)
    ]
  )
)

# Training data.
list_pca_full <- list(
  x = mat_csi_crashes,
  y = as.matrix(df_csi_scale_crashes$crashes),
  censoring.status = NULL
)

# Predict all CSI (PC1) values.
superpc_csi <- superpc::superpc.predict(
  superpc_train,
  list_pca_train,
  list_pca_full,
  threshold = 0.65,
  n.components = 1,
  prediction.type = "scores"
)

# Get the feature weights from the trained model.
mat_feat_weights <- superpc_train$feature.scores

# Identify which features are used at the chosen threshold.
int_feat_threshold <- which(abs(mat_feat_weights) > 0.65)

# Subset the full feature matrix to just those.
mat_selected <- list_pca_full$x[int_feat_threshold, ]

# Compute PC1 scores as weighted sum.
num_csi <- colSums(mat_feat_weights[int_feat_threshold] * mat_selected)

# Add to your spatial object.
sf_csi_scale_polygons$csi <- as.numeric(num_csi)
sf_csi_scale_polygons$csi_normal <- normalize(as.numeric(num_csi))

# Change name.
sf_csi_pca <- sf_csi_scale_polygons

# Save output.
chr_sf_csi_pca_path <- file.path(dir_output, "b_03", "sf_csi_pca.rds")
saveRDS(sf_csi_pca, chr_sf_csi_pca_path)

df_csi_pca <- sf::st_drop_geometry(sf_csi_pca)
write.csv(df_csi_pca, gsub("rds", "csv", chr_sf_csi_pca_path))

################################################################################
# Plot training vs testing census block groups.
sf_csi_scale_polygons$superpc <- "FILL"
sf_csi_scale_polygons$superpc[int_train] <- "train"
sf_csi_scale_polygons$superpc[int_test] <- "test"

ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = sf_ct_towns,
    col = "grey50",
    fill = NA,
    lwd = 1
  ) +
  ggplot2::geom_sf(
    data = sf_context,
    fill = "black",
    alpha = 0.1,
    color = "black",
    lwd = 1
  ) +
  ggplot2::geom_sf(
    data = sf_csi_scale_polygons,
    aes(fill = superpc),
    color = "black"
  ) +
  ggplot2::geom_sf(data = sf_faf5_123_nh, aes(color = "Roadway"), lwd = 1) +
  ggplot2::scale_color_manual(values = c("Roadway" = "black"), name = "") +
  ggplot2::coord_sf(
    xlim = sf::st_bbox(sf_csi_polygons)[c("xmin", "xmax")],
    ylim = sf::st_bbox(sf_csi_polygons)[c("ymin", "ymax")]
  ) +
  ggplot2::theme_bw()
