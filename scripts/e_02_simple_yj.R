################################################################################
# Analyze Community Severance Index and environmental/demographic conditions
# with Yeo-Johnson transformation and linear regression methods.

################################################################################
# Source variables from a_00_initiate.R
source(file.path(here::here(), "scripts", "a_00_initiate.R"))

################################################################################
# Import CSI and conditions data.
chr_csi_cond_path <- file.path(dir_output, "d_06", "sf_csi_cond.rds")
testthat::expect_true(file.exists(chr_csi_cond_path))
sf_csi_cond <- readRDS(chr_csi_cond_path)

################################################################################
# Yeo-Johnson transformation for normalized data.
sf_csi_cond$csi_yj <- stats::predict(
  bestNormalize::yeojohnson(sf_csi_cond$csi)
)
hist(sf_csi_cond$csi_yj)
shapiro.test(sf_csi_cond$csi_yj)

################################################################################
# Analyze CSI x tree cover.
lm_tree_yj <- lm(csi_yj ~ tree_cover_perc, data = sf_csi_cond)
summary(lm_tree_yj)
confint(lm_tree_yj)

# Test for normally distributed residuals.
shapiro.test(lm_tree_yj$residuals)

# Visualize
hist(lm_tree_yj$residuals)
qqnorm(lm_tree_yj$residuals)
qqline(lm_tree_yj$residuals, col = "forestgreen")

# Plot
ggplot_csi_yj_tree <-
  ggplot2::ggplot(sf_csi_cond, aes(x = tree_cover_perc, y = csi_yj)) +
  ggplot2::geom_point(alpha = 0.6) +
  ggplot2::geom_smooth(
    method = "lm",
    se = TRUE,
    color = "forestgreen",
    fill = "lightgrey"
  ) +
  ggplot2::labs(
    x = "Tree Cover (%)",
    y = "Yeo-Johnson Transform CSI Score"
  ) +
  ggpubr::theme_pubr()
ggplot_csi_yj_tree

chr_csi_tree <- paste0(
  "figures/",
  "ggplot_csi_yj_tree_",
  format(Sys.time(), "%m%d_%H%M"),
  ".png"
)
png(chr_csi_tree)
ggplot_csi_yj_tree
dev.off()

################################################################################
# Analyze CSI x NO2 concentration (2019)
lm_no2_yj <- lm(csi_yj ~ no2_2019, data = sf_csi_cond)
summary(lm_no2_yj)
confint(lm_no2_yj)

# Test for normally distributed residuals.
shapiro.test(lm_no2_yj$residuals)

# Visualize
hist(lm_no2_yj$residuals)
qqnorm(lm_no2_yj$residuals)
qqline(lm_no2_yj$residuals, col = "purple")

# Plot
ggplot_csi_yj_no22019 <-
  ggplot2::ggplot(sf_csi_cond, aes(x = no2_2019, y = csi_yj)) +
  ggplot2::geom_point(alpha = 0.6) +
  ggplot2::geom_smooth(
    method = "lm",
    se = TRUE,
    color = "purple",
    fill = "lightgrey"
  ) +
  ggplot2::labs(
    x = "NO2 Concentration (molec/cm2)",
    y = "Yeo-Johnson Transform CSI Score"
  ) +
  ggpubr::theme_pubr()
ggplot_csi_yj_no22019

chr_csi_no22019 <- paste0(
  "figures/",
  "ggplot_csi_yj_no22019_",
  format(Sys.time(), "%m%d_%H%M"),
  ".png"
)
png(chr_csi_no22019)
ggplot_csi_yj_no22019
dev.off()

################################################################################
# Analyze CSI x PM2.5 concentration (2019)
lm_pm25_yj <- lm(csi_yj ~ pm25_2019, data = sf_csi_cond)
summary(lm_pm25_yj)
confint(lm_pm25_yj)

# Test for normally distributed residuals.
shapiro.test(lm_pm25_yj$residuals)

# Visualize
hist(lm_pm25_yj$residuals)
qqnorm(lm_pm25_yj$residuals)
qqline(lm_pm25_yj$residuals, col = "orange")

# Plot
ggplot_csi_yj_pm252019 <-
  ggplot2::ggplot(sf_csi_cond, aes(x = pm25_2019, y = csi_yj)) +
  ggplot2::geom_point(alpha = 0.6) +
  ggplot2::geom_smooth(
    method = "lm",
    se = TRUE,
    color = "orange",
    fill = "lightgrey"
  ) +
  ggplot2::labs(
    x = "PM2.5 Concentration (µg/m^3)",
    y = "Yeo-Johnson Transform CSI Score"
  ) +
  ggpubr::theme_pubr()
ggplot_csi_yj_pm252019

chr_csi_pm252019 <- paste0(
  "figures/",
  "ggplot_csi_yj_pm252019_",
  format(Sys.time(), "%m%d_%H%M"),
  ".png"
)
png(chr_csi_pm252019)
ggplot_csi_yj_pm252019
dev.off()

################################################################################
# Analyze CSI x cooling degree days.
lm_cdd_yj <- lm(csi_yj ~ CDD, data = sf_csi_cond)
summary(lm_cdd_yj)
confint(lm_cdd_yj)

# Test for normally distributed residuals.
shapiro.test(lm_cdd_yj$residuals)

# Visualize
hist(lm_cdd_yj$residuals)
qqnorm(lm_cdd_yj$residuals)
qqline(lm_cdd_yj$residuals, col = "red")

# Plot
ggplot_csi_yj_cdd <-
  ggplot2::ggplot(sf_csi_cond, aes(x = CDD, y = csi_yj)) +
  ggplot2::geom_point(alpha = 0.6) +
  ggplot2::geom_smooth(
    method = "lm",
    se = TRUE,
    color = "red",
    fill = "lightgrey"
  ) +
  ggplot2::labs(
    x = "Cooling Degree Days (CDD)",
    y = "Yeo-Johnson Transform CSI Score"
  ) +
  ggpubr::theme_pubr()
ggplot_csi_yj_cdd

chr_csi_cdd <- paste0(
  "figures/",
  "ggplot_csi_yj_cdd_",
  format(Sys.time(), "%m%d_%H%M"),
  ".png"
)
png(chr_csi_cdd)
ggplot_csi_yj_cdd
dev.off()

################################################################################
# Analyze CSI x % white population (linear regression)
lm_perc_white_yj <- lm(csi_yj ~ perc_white, data = sf_csi_cond)
summary(lm_perc_white_yj)
confint(lm_perc_white_yj)

# Test for normally distributed residuals.
shapiro.test(lm_perc_white_yj$residuals)

# Visualize
hist(lm_perc_white_yj$residuals)
qqnorm(lm_perc_white_yj$residuals)
qqline(lm_perc_white_yj$residuals, col = "red")

# Plot
ggplot_csi_yj_white <-
  ggplot2::ggplot(sf_csi_cond, aes(x = perc_white, y = csi_yj)) +
  ggplot2::geom_point(alpha = 0.6) +
  ggplot2::geom_smooth(
    method = "lm",
    se = TRUE,
    color = "blue",
    fill = "lightgrey"
  ) +
  ggplot2::labs(
    x = "Non-Hispanic White Population (%)",
    y = "Yeo-Johnson Transform CSI Score"
  ) +
  ggpubr::theme_pubr()
ggplot_csi_yj_white

chr_csi_white <- paste0(
  "figures/",
  "ggplot_csi_yj_white_",
  format(Sys.time(), "%m%d_%H%M"),
  ".png"
)
png(chr_csi_white)
ggplot_csi_yj_white
dev.off()

################################################################################
# Analyze CSI x % Black population (linear regression)
lm_perc_black_yj <- lm(csi_yj ~ perc_black, data = sf_csi_cond)
summary(lm_perc_black_yj)
confint(lm_perc_black_yj)

# Test for normally distributed residuals.
shapiro.test(lm_perc_black_yj$residuals)

# Visualize
hist(lm_perc_black_yj$residuals)
qqnorm(lm_perc_black_yj$residuals)
qqline(lm_perc_black_yj$residuals, col = "blue")

# Plot
ggplot_csi_yj_black <-
  ggplot2::ggplot(sf_csi_cond, aes(x = perc_black, y = csi_yj)) +
  ggplot2::geom_point(alpha = 0.6) +
  ggplot2::geom_smooth(
    method = "lm",
    se = TRUE,
    color = "blue",
    fill = "lightgrey"
  ) +
  ggplot2::labs(
    x = "Non-Hispanic Black Population (%)",
    y = "Yeo-Johnson Transform CSI Score"
  ) +
  ggpubr::theme_pubr()
ggplot_csi_yj_black

chr_csi_black <- paste0(
  "figures/",
  "ggplot_csi_yj_black_",
  format(Sys.time(), "%m%d_%H%M"),
  ".png"
)
png(chr_csi_black)
ggplot_csi_yj_black
dev.off()

################################################################################
# Analyze CSI x % Hispanic/Latino population (linear regression)
lm_perc_hispanic_yj <- lm(csi_yj ~ perc_hispanic, data = sf_csi_cond)
summary(lm_perc_hispanic_yj)
confint(lm_perc_hispanic_yj)

# Test for normally distributed residuals.
shapiro.test(lm_perc_hispanic_yj$residuals)

# Visualize
hist(lm_perc_hispanic_yj$residuals)
qqnorm(lm_perc_hispanic_yj$residuals)
qqline(lm_perc_hispanic_yj$residuals, col = "blue")

# Plot
ggplot_csi_yj_hispanic <-
  ggplot2::ggplot(sf_csi_cond, aes(x = perc_hispanic, y = csi_yj)) +
  ggplot2::geom_point(alpha = 0.6) +
  ggplot2::geom_smooth(
    method = "lm",
    se = TRUE,
    color = "blue",
    fill = "lightgrey"
  ) +
  ggplot2::labs(
    x = "Hispanic Population (%)",
    y = "Yeo-Johnson Transform CSI Score"
  ) +
  ggpubr::theme_pubr()
ggplot_csi_yj_hispanic

chr_csi_hispanic <- paste0(
  "figures/",
  "ggplot_csi_yj_hispanic_",
  format(Sys.time(), "%m%d_%H%M"),
  ".png"
)
png(chr_csi_hispanic)
ggplot_csi_yj_hispanic
dev.off()
