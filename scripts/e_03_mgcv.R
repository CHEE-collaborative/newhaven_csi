################################################################################
# Analyze Community Severance Index and environmental/demographic conditions
# with `mgcv` regression accounting for spatial autocorrelation.

################################################################################
# Source variables from a_00_initiate.R
source(file.path(here::here(), "scripts", "a_00_initiate.R"))

################################################################################
# Import CSI and conditions data.
chr_csi_cond_path <- file.path(dir_output, "d_06", "sf_csi_cond.rds")
testthat::expect_true(file.exists(chr_csi_cond_path))
sf_csi_cond <- readRDS(chr_csi_cond_path)

################################################################################
# Drop census block group with no neighbors.
sf_csi_cond_nn <- sf_csi_cond[
  grep("090091428004", sf_csi_cond$GEOID20, invert = TRUE),
]

################################################################################
# Yeo-Johnson transformation.
sf_csi_cond_nn$csi_yj <- stats::predict(
  bestNormalize::yeojohnson(sf_csi_cond_nn$csi)
)

################################################################################
# GEOID as a multi-level factor.
sf_csi_cond_nn$GEOID20F <- as.factor(sf_csi_cond_nn$GEOID20)

################################################################################
# Define nearest neighbors.
nb_csi_cond <- spdep::poly2nb(sf_csi_cond_nn)
names(nb_csi_cond) <- levels(sf_csi_cond_nn$GEOID20F)

################################################################################
# Analyze CSI x tree cover (csi_100).
gam_tree <- mgcv::gam(
  csi ~
    tree_cover_perc +
      s(
        GEOID20F,
        bs = "mrf",
        xt = list(nb = nb_csi_cond)
      ),
  data = sf_csi_cond_nn
)
summary(gam_tree)

# Test for normally distributed residuals.
shapiro.test(gam_tree$residuals)

# Visualize
hist(gam_tree$residuals)
qqnorm(gam_tree$residuals)
qqline(gam_tree$residuals, col = "forestgreen")

# Analyze CSI x tree cover (csi_yj).
gam_tree_yj <- mgcv::gam(
  csi_yj ~
    tree_cover_perc +
      s(
        GEOID20F,
        bs = "mrf",
        xt = list(nb = nb_csi_cond)
      ),
  data = sf_csi_cond_nn
)
summary(gam_tree_yj)

# Test for normally distributed residuals.
shapiro.test(gam_tree_yj$residuals)

# Visualize
hist(gam_tree_yj$residuals)
qqnorm(gam_tree_yj$residuals)
qqline(gam_tree_yj$residuals, col = "red")

################################################################################
# Analyze CSI x NO2 (csi_100).
gam_no2 <- mgcv::gam(
  csi ~
    no2_2019 +
      s(
        GEOID20F,
        bs = "mrf",
        xt = list(nb = nb_csi_cond)
      ),
  data = sf_csi_cond_nn
)
summary(gam_no2)

# Test for normally distributed residuals.
shapiro.test(gam_no2$residuals)

# Visualize
hist(gam_no2$residuals)
qqnorm(gam_no2$residuals)
qqline(gam_no2$residuals, col = "red")

# Analyze CSI x NO2 (csi_yj).
gam_no2_yj <- mgcv::gam(
  csi_yj ~
    no2_2019 +
      s(
        GEOID20F,
        bs = "mrf",
        xt = list(nb = nb_csi_cond)
      ),
  data = sf_csi_cond_nn
)
summary(gam_no2_yj)

# Test for normally distributed residuals.
shapiro.test(gam_no2_yj$residuals)

# Visualize
hist(gam_no2_yj$residuals)
qqnorm(gam_no2_yj$residuals)
qqline(gam_no2_yj$residuals, col = "red")

################################################################################
# Analyze CSI x PM2.5 (csi_100).
gam_pm25 <- mgcv::gam(
  csi ~
    pm25_2019 +
      s(
        GEOID20F,
        bs = "mrf",
        xt = list(nb = nb_csi_cond)
      ),
  data = sf_csi_cond_nn
)
summary(gam_pm25)

# Test for normally distributed residuals.
shapiro.test(gam_pm25$residuals)

# Visualize
hist(gam_pm25$residuals)
qqnorm(gam_pm25$residuals)
qqline(gam_pm25$residuals, col = "red")

# Analyze CSI x PMf2.5 (csi_yj).
gam_pm25_yj <- mgcv::gam(
  csi_yj ~
    pm25_2019 +
      s(
        GEOID20F,
        bs = "mrf",
        xt = list(nb = nb_csi_cond)
      ),
  data = sf_csi_cond_nn
)
summary(gam_pm25_yj)

# Test for normally distributed residuals.
shapiro.test(gam_pm25_yj$residuals)

# Visualize
hist(gam_pm25_yj$residuals)
qqnorm(gam_pm25_yj$residuals)
qqline(gam_pm25_yj$residuals, col = "red")

################################################################################
# Analyze CSI x CDD (csi_100).
gam_cdd <- mgcv::gam(
  csi ~
    CDD +
      s(
        GEOID20F,
        bs = "mrf",
        xt = list(nb = nb_csi_cond)
      ),
  data = sf_csi_cond_nn
)
summary(gam_cdd)

# Test for normally distributed residuals.
shapiro.test(gam_cdd$residuals)

# Visualize
hist(gam_cdd$residuals)
qqnorm(gam_cdd$residuals)
qqline(gam_cdd$residuals, col = "red")

# Analyze CSI x PMf2.5 (csi_yj).
gam_cdd_yj <- mgcv::gam(
  csi_yj ~
    CDD +
      s(
        GEOID20F,
        bs = "mrf",
        xt = list(nb = nb_csi_cond)
      ),
  data = sf_csi_cond_nn
)
summary(gam_cdd_yj)

# Test for normally distributed residuals.
shapiro.test(gam_cdd_yj$residuals)

# Visualize
hist(gam_cdd_yj$residuals)
qqnorm(gam_cdd_yj$residuals)
qqline(gam_cdd_yj$residuals, col = "red")


################################################################################
# Analyze CSI x % Black population (csi_100).
gam_perc_black <- mgcv::gam(
  csi ~
    perc_black +
      s(
        GEOID20F,
        bs = "mrf",
        xt = list(nb = nb_csi_cond)
      ),
  data = sf_csi_cond_nn
)
summary(gam_perc_black)

# Test for normally distributed residuals.
shapiro.test(gam_perc_black$residuals)

# Visualize
hist(gam_perc_black$residuals)
qqnorm(gam_perc_black$residuals)
qqline(gam_perc_black$residuals, col = "red")

# Analyze CSI x PMf2.5 (csi_yj).
gam_perc_black_yj <- mgcv::gam(
  csi_yj ~
    perc_black +
      s(
        GEOID20F,
        bs = "mrf",
        xt = list(nb = nb_csi_cond)
      ),
  data = sf_csi_cond_nn
)
summary(gam_perc_black_yj)

# Test for normally distributed residuals.
shapiro.test(gam_perc_black_yj$residuals)

# Visualize
hist(gam_perc_black_yj$residuals)
qqnorm(gam_perc_black_yj$residuals)
qqline(gam_perc_black_yj$residuals, col = "red")


################################################################################
# Analyze CSI x % Hispanic/Latino population (csi_100).
gam_perc_hispanic <- mgcv::gam(
  csi ~
    perc_hispanic +
      s(
        GEOID20F,
        bs = "mrf",
        xt = list(nb = nb_csi_cond)
      ),
  data = sf_csi_cond_nn
)
summary(gam_perc_hispanic)

# Test for normally distributed residuals.
shapiro.test(gam_perc_hispanic$residuals)

# Visualize
hist(gam_perc_hispanic$residuals)
qqnorm(gam_perc_hispanic$residuals)
qqline(gam_perc_hispanic$residuals, col = "red")

# Analyze CSI x PMf2.5 (csi_yj).
gam_perc_hispanic_yj <- mgcv::gam(
  csi_yj ~
    perc_hispanic +
      s(
        GEOID20F,
        bs = "mrf",
        xt = list(nb = nb_csi_cond)
      ),
  data = sf_csi_cond_nn
)
summary(gam_perc_hispanic_yj)

# Test for normally distributed residuals.
shapiro.test(gam_perc_hispanic_yj$residuals)

# Visualize
hist(gam_perc_hispanic_yj$residuals)
qqnorm(gam_perc_hispanic_yj$residuals)
qqline(gam_perc_hispanic_yj$residuals, col = "red")
