################################################################################
# Analyze Community Severance Index and environmental/demographic conditions.

################################################################################
# Source variables from a_00_initiate.R
source(file.path(here::here(), "scripts", "a_00_initiate.R"))

################################################################################
# Import CSI and conditions data.
chr_csi_cond_path <- file.path(dir_output, "d_06", "sf_csi_cond.rds")
testthat::expect_true(file.exists(chr_csi_cond_path))
sf_csi_cond <- readRDS(chr_csi_cond_path)

################################################################################
# Analyze CSI x tree cover.
lm_tree <- lm(csi_100 ~ tree_cover_perc, data = sf_csi_cond)
summary(lm_tree)
confint(lm_tree)

ggplot_csi_tree <-
  ggplot2::ggplot(sf_csi_cond, aes(x = tree_cover_perc, y = csi_100)) +
  ggplot2::geom_point(alpha = 0.6) +
  ggplot2::geom_smooth(
    method = "lm", se = TRUE, color = "forestgreen", fill = "lightgrey"
  ) +
  ggplot2::labs(
    x = "Tree Cover (%)",
    y = "Community Severance Index (CSI) Score"
  ) +
  ggpubr::theme_pubr()
ggplot_csi_tree

chr_csi_tree <- paste0(
  "figures/",
  "ggplot_csi_tree_",
  format(Sys.time(), "%m%d_%H%M"),
  ".png"
)
png(chr_csi_tree)
ggplot_csi_tree
dev.off()

################################################################################
# Analyze CSI x NO2 concentration (2019)
lm_no2 <- lm(csi_100 ~ no2_2019, data = sf_csi_cond)
summary(lm_no2)
confint(lm_no2)

ggplot_csi_no22019 <-
  ggplot2::ggplot(sf_csi_cond, aes(x = no2_2019, y = csi_100)) +
  ggplot2::geom_point(alpha = 0.6) +
  ggplot2::geom_smooth(
    method = "lm", se = TRUE, color = "purple", fill = "lightgrey"
  ) +
  ggplot2::labs(
    x = "NO2 Concentration (molec/cm2)",
    y = "Community Severance Index (CSI) Score"
  ) +
  ggpubr::theme_pubr()
ggplot_csi_no22019

chr_csi_no22019 <- paste0(
  "figures/",
  "ggplot_csi_no22019_",
  format(Sys.time(), "%m%d_%H%M"),
  ".png"
)
png(chr_csi_no22019)
ggplot_csi_no22019
dev.off()

################################################################################
# Analyze CSI x PM2.5 concentration (2019)
lm_pm25_2019 <- lm(csi_100 ~ pm25_2019, data = sf_csi_cond)
summary(lm_pm25_2019)
confint(lm_pm25_2019)

ggplot_csi_pm252019 <-
  ggplot2::ggplot(sf_csi_cond, aes(x = pm25_2019, y = csi_100)) +
  ggplot2::geom_point(alpha = 0.6) +
  ggplot2::geom_smooth(
    method = "lm", se = TRUE, color = "orange", fill = "lightgrey"
  ) +
  ggplot2::labs(
    x = "PM2.5 Concentration (µg/m^3)",
    y = "Community Severance Index (CSI) Score"
  ) +
  ggpubr::theme_pubr()
ggplot_csi_pm252019

chr_csi_pm252019 <- paste0(
  "figures/",
  "ggplot_csi_pm252019_",
  format(Sys.time(), "%m%d_%H%M"),
  ".png"
)
png(chr_csi_pm252019)
ggplot_csi_pm252019
dev.off()

################################################################################
# Analyze CSI x temperature.
# plot(csi_100 ~ temp, data = sf_csi_cond)
# lm_temp <- lm(csi_100 ~ temp, data = sf_csi_cond)
# summary(lm_temp)

# ggplot2::ggplot(sf_csi_cond, aes(x = temp, y = csi_100)) +
#   ggplot2::geom_point(alpha = 0.6) +
#   ggplot2::geom_smooth(
#     method = "lm", se = TRUE, color = "red", fill = "lightgrey"
#   ) +
#   ggplot2::labs(
#     x = "Temperature Statistic",
#     y = "Community Severance Index (CSI) Score"
#   ) +
#   ggpubr::theme_pubr()

################################################################################
# Analyze CSI x % white population (linear regression)
lm_perc_white <- lm(csi_100 ~ perc_white, data = sf_csi_cond)
summary(lm_perc_white)
confint(lm_perc_white)

ggplot_csi_white <-
  ggplot2::ggplot(sf_csi_cond, aes(x = perc_white, y = csi_100)) +
  ggplot2::geom_point(alpha = 0.6) +
  ggplot2::geom_smooth(
    method = "lm", se = TRUE, color = "blue", fill = "lightgrey"
  ) +
  ggplot2::labs(
    x = "Non-Hispanic White Population (%)",
    y = "Community Severance Index (CSI) Score"
  ) +
  ggpubr::theme_pubr()
ggplot_csi_white

chr_csi_white <- paste0(
  "figures/",
  "ggplot_csi_white_",
  format(Sys.time(), "%m%d_%H%M"),
  ".png"
)
png(chr_csi_white)
ggplot_csi_white
dev.off()

################################################################################
# Analyze CSI x % black population (linear regression)
lm_perc_black <- lm(csi_100 ~ perc_black, data = sf_csi_cond)
summary(lm_perc_black)
confint(lm_perc_black)

ggplot_csi_black <-
  ggplot2::ggplot(sf_csi_cond, aes(x = perc_black, y = csi_100)) +
  ggplot2::geom_point(alpha = 0.6) +
  ggplot2::geom_smooth(
    method = "lm", se = TRUE, color = "blue", fill = "lightgrey"
  ) +
  ggplot2::labs(
    x = "Non-Hispanic Black Population (%)",
    y = "Community Severance Index (CSI) Score"
  ) +
  ggpubr::theme_pubr()
ggplot_csi_black

chr_csi_black <- paste0(
  "figures/",
  "ggplot_csi_black_",
  format(Sys.time(), "%m%d_%H%M"),
  ".png"
)
png(chr_csi_black)
ggplot_csi_black
dev.off()

################################################################################
# Analyze CSI x % hispanic population (linear regression)
lm_perc_hispanic <- lm(csi_100 ~ perc_hispanic, data = sf_csi_cond)
summary(lm_perc_hispanic)
confint(lm_perc_hispanic)

ggplot_csi_hispanic <-
  ggplot2::ggplot(sf_csi_cond, aes(x = perc_hispanic, y = csi_100)) +
  ggplot2::geom_point(alpha = 0.6) +
  ggplot2::geom_smooth(
    method = "lm", se = TRUE, color = "blue", fill = "lightgrey"
  ) +
  ggplot2::labs(
    x = "Hispanic Population (%)",
    y = "Community Severance Index (CSI) Score"
  ) +
  ggpubr::theme_pubr()
ggplot_csi_hispanic

chr_csi_hispanic <- paste0(
  "figures/",
  "ggplot_csi_hispanic_",
  format(Sys.time(), "%m%d_%H%M"),
  ".png"
)
png(chr_csi_hispanic)
ggplot_csi_hispanic
dev.off()

################################################################################
# Analyze CSI x predominant population (anova)
chr_pred <- c()
for (r in seq_len(nrow(sf_csi_cond))) {
  sf_csi_r <- sf::st_drop_geometry(
    sf_csi_cond[r, grep("perc_", names(sf_csi_cond))]
  )
  chr_max <- names(sf_csi_r)[which.max(sf_csi_r[1, ])]
  chr_pred <- c(chr_pred, gsub("perc_", "", chr_max))
}
sf_csi_cond$pred <- chr_pred

lm_pred <- lm(csi_100 ~ pred_minority, data = sf_csi_cond)
summary(lm_pred)
anova(lm_pred)

ggplot_csi_box <- ggplot2::ggplot(
  sf_csi_cond,
  aes(
    x = factor(pred_minority),
    y = csi_100,
    fill = factor(pred_minority)
  )
) +
  ggplot2::geom_boxplot(color = "black") +
  ggplot2::labs(
    x = "Majority Population Demographic",
    y = "Community Severance Index (CSI) Score"
  ) +
  ggplot2::scale_fill_manual(
    values = c("#60886d", "#906c9e", "#545da0")
  ) +
  ggpubr::theme_pubr() +
  ggplot2::theme(legend.position = "none")
ggplot_csi_box

chr_csi_box <- paste0(
  "figures/",
  "ggplot_csi_box_",
  format(Sys.time(), "%m%d_%H%M"),
  ".png"
)
png(chr_csi_box)
ggplot_csi_box
dev.off()
