################################################################################
# Validate Community Severance Index (CSI) for New Haven with car crash data.

################################################################################
# Source variables from a_00_initiate.R
source(file.path(here::here(), "scripts", "a_00_initiate.R"))

################################################################################
# Import CSI data.
chr_csi_path <- file.path(dir_output, "b_01", "sf_csi_nh.rds")
testthat::expect_true(file.exists(chr_csi_path))
sf_csi_nh <- readRDS(chr_csi_path)

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
# Load Smart Location Database variables.
chr_sld_variables_path <- file.path(
  dir_output,
  "a_01",
  "sf_sld_variables_proj.rds"
)
testthat::expect_true(file.exists(chr_sld_variables_path))
sf_sld_variables_proj <- readRDS(chr_sld_variables_path)

################################################################################
# Add population data from Smart location Database.
sf_csi_nh_pop <- merge(
  sf_csi_nh,
  sf::st_drop_geometry(sf_sld_variables_proj[, c("GEOID20", "TotPop")]),
  by = "GEOID20"
)

################################################################################
# Merge CSI data with New Haven polygons.
sf_csi_polygons <- merge(
  sf_nh_polygons,
  sf::st_drop_geometry(sf_csi_nh_pop),
  by = "GEOID20"
)

################################################################################
# Calculate population density.
sf_csi_polygons$popdens <- sf_csi_polygons$TotPop / sf::st_area(sf_csi_polygons)

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
  sf::st_transform(crs = sf::st_crs(sf_csi_polygons))

################################################################################
# Pedestrian-involved crashes.
sf_crash_ped <- sf_crash[sf_crash$Number.Of.Non.Motorist > 0, ]

################################################################################
# Count number of crashes per census block group.
list_csi_crash <- sf::st_intersects(sf_csi_polygons, sf_crash)
sf_csi_polygons$crashes <- lengths(list_csi_crash)
list_csi_crash_ped <- sf::st_intersects(sf_csi_polygons, sf_crash_ped)
sf_csi_polygons$crashes_ped <- lengths(list_csi_crash_ped)

################################################################################
# Import Area Deprivation Index (ADI) data.
chr_adi_path <- file.path(
  dir_input,
  "adi",
  "CT_2020_ADI_Census_Block_Group_v4_0_1.csv"
)
testthat::expect_true(file.exists(chr_adi_path))
df_adi <- read.csv(chr_adi_path)
df_adi$GEOID20 <- stringr::str_pad(df_adi$FIPS, 12, "0", side = "left")

################################################################################
# Merge CSI and ADI data.
sf_csi_polygons1 <- dplyr::left_join(
  sf_csi_polygons,
  df_adi[, c("GEOID20", "ADI_NATRANK", "ADI_STATERNK")],
  by = "GEOID20"
)
sf_csi_polygons1$ADI_NATRANK <- as.numeric(sf_csi_polygons1$ADI_NATRANK)
sf_csi_polygons1$ADI_STATE <- as.numeric(sf_csi_polygons1$ADI_STATERNK)

################################################################################
# Yeo-Johnson Transformation for MR1 and MR2
sf_csi_polygons1$MR1_yj <- stats::predict(
  bestNormalize::yeojohnson(sf_csi_polygons1$MR1)
)
sf_csi_polygons1$MR2_yj <- stats::predict(
  bestNormalize::yeojohnson(sf_csi_polygons1$MR2)
)
sf_csi_polygons1$MR1_normal <- normalize(sf_csi_polygons1$MR1)
sf_csi_polygons1$MR2_normal <- normalize(sf_csi_polygons1$MR2)

################################################################################
# Simple linear models.
################################################################################
# Assess relationship between MR1 and crash count.
lm_mr1_crashes <- lm(MR1_normal ~ crashes, data = sf_csi_polygons1)
summary(lm_mr1_crashes)
confint(lm_mr1_crashes)

# Test for normally distributed residuals.
shapiro.test(lm_mr1_crashes$residuals)

# Visualize
hist(lm_mr1_crashes$residuals)
qqnorm(lm_mr1_crashes$residuals)
qqline(lm_mr1_crashes$residuals, col = "blue")

# Plot
ggplot2::ggplot(sf_csi_polygons1, aes(x = crashes_ped, y = MR1)) +
  ggplot2::geom_point(alpha = 0.6) +
  ggplot2::geom_smooth(
    method = "lm",
    se = TRUE,
    color = "blue",
    fill = "lightgrey"
  ) +
  ggplot2::labs(
    x = "Number of Crashes in 2019)",
    y = "Community Severance Index (CSI) Score"
  ) +
  ggpubr::theme_pubr()

################################################################################
# Assess relationship between MR2 and crash count.
lm_mr2_crashes <- lm(MR2_normal ~ crashes, data = sf_csi_polygons1)
summary(lm_mr2_crashes)
confint(lm_mr2_crashes)

# Test for normally distributed residuals.
shapiro.test(lm_mr2_crashes$residuals)

# Visualize
hist(lm_mr2_crashes$residuals)
qqnorm(lm_mr2_crashes$residuals)
qqline(lm_mr2_crashes$residuals, col = "blue")

# Plot
ggplot2::ggplot(sf_csi_polygons1, aes(x = crashes, y = MR2)) +
  ggplot2::geom_point(alpha = 0.6) +
  ggplot2::geom_smooth(
    method = "lm",
    se = TRUE,
    color = "red",
    fill = "lightgrey"
  ) +
  ggplot2::labs(
    x = "Number of Crashes in 2019)",
    y = "Community Severance Index (CSI) Score"
  ) +
  ggpubr::theme_pubr()

################################################################################
# Generalized Additive Model (GAM) with `mgcv`.

################################################################################
# Assess relationship between MR1 and crashes (gam).
gam_mr1_crash <- mgcv::gam(
  crashes ~
    s(MR1_normal, bs = "cr", k = 3) +
      as.numeric(popdens) +
      ADI_STATE,
  data = sf_csi_polygons1,
  family = nb()
)
summary(gam_mr1_crash)

# Test for normally distributed residuals.
shapiro.test(gam_mr1_crash$residuals)

# Visualize
hist(gam_mr1_crash$residuals)
qqnorm(gam_mr1_crash$residuals)
qqline(gam_mr1_crash$residuals, col = "blue")

library(gratia)
smooth_gam_mr1 <- gratia::smooth_estimates(
  gam_mr1_crash,
  smooth = "s(MR1_normal)"
)

ggplot2::ggplot(smooth_gam_mr1, aes(x = MR1_normal, y = .estimate)) +
  ggplot2::geom_line(color = "blue", size = 1) +
  ggplot2::geom_ribbon(
    aes(ymin = .estimate - 2 * .se, ymax = .estimate + 2 * .se),
    alpha = 0.2
  ) +
  ggplot2::geom_point(
    data = sf_csi_polygons1,
    aes(x = MR1_normal, y = crashes),
    alpha = 0.6
  ) +
  ggplot2::labs(
    x = "Effect on MR1 (partial)",
    y = "Number of Crashes"
  ) +
  ggplot2::theme_minimal()

################################################################################
# Assess relationship between MR2 and crashes (gam).
gam_mr2_crash <- mgcv::gam(
  crashes ~
    s(MR2_normal, bs = "cr", k = 3) +
      as.numeric(popdens) +
      ADI_STATE,
  data = sf_csi_polygons1,
  family = nb()
)
summary(gam_mr2_crash)

# Test for normally distributed residuals.
shapiro.test(gam_mr2_crash$residuals)

# Visualize
hist(gam_mr2_crash$residuals)
qqnorm(gam_mr2_crash$residuals)
qqline(gam_mr2_crash$residuals, col = "blue")

smooth_gam_mr2 <- gratia::smooth_estimates(
  gam_mr2_crash,
  smooth = "s(MR2_normal)"
)

ggplot2::ggplot(smooth_gam_mr2, aes(x = MR2_normal, y = .estimate)) +
  ggplot2::geom_line(color = "red", size = 1) +
  ggplot2::geom_ribbon(
    aes(ymin = .estimate - 2 * .se, ymax = .estimate + 2 * .se),
    alpha = 0.2
  ) +
  ggplot2::geom_point(
    data = sf_csi_polygons1,
    aes(x = MR2_normal, y = crashes),
    alpha = 0.6
  ) +
  ggplot2::labs(
    x = "Effect on MR2 (partial)",
    y = "Number of Crashes"
  ) +
  ggplot2::theme_minimal()
