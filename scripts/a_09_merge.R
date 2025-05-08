################################################################################
# Merge all Community Serverance Index (CSI) variables at SmartLocationsDatabase
# census block groups for New Haven, Connecticut.

################################################################################
# Source variables from a_00_initiate.R
source(file.path(here::here(), "scripts", "a_00_initiate.R"))

################################################################################
# List output files.
chr_output_files <- list.files(dir_output, full.names = TRUE, recursive = TRUE)
chr_output_files <- grep("a_\\d{2}", chr_output_files, value = TRUE)

################################################################################
# Import all output (`.rds`) files.
for (c in seq_along(chr_output_files)) {
  chr_split <- strsplit(chr_output_files[c], "/")[[1]]
  chr_object <- gsub(".rds", "", chr_split[length(chr_split)])
  assign(chr_object, readRDS(chr_output_files[c]))
}

################################################################################
# Subset SmartLocationDatabase data to CSI variables.
chr_sld_variables <- c(
  "GEOID20", "D3AAO", "D3APO", "D3B", "D3BAO", "NatWalkInd"
)
testthat::expect_true(all(chr_sld_variables %in% names(sf_sld_variables_proj)))
sf_sld_csi <- sf_sld_variables_proj[, chr_sld_variables]
colnames(sf_sld_csi)[2:5] <- c(
  "autom_netw_dens", "pedest_netw_dens",
  "street_no_autom_inters_dens", "autom_inters_dens"
)

################################################################################
# Check for missing data and -99999 values.
testthat::expect_equal(unique(as.character(unique(is.na(sf_sld_csi)))), "FALSE")
df_sld_99999 <- as.data.frame(sf_sld_csi) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::where(is.numeric), ~ sum(. == -99999, na.rm = TRUE)
    )
  ) %>%
  tidyr::pivot_longer(
    cols = dplyr::everything(), names_to = "Column", values_to = "Count"
  ) %>%
  dplyr::filter(Count > 0)
testthat::expect_equal(dim(df_sld_99999)[1], 0)

################################################################################
# Merge all CSI variables.
sf_csi <- dplyr::left_join(
  sf_grid_nh, sf::st_drop_geometry(sf_sld_csi), by = "GEOID20"
) %>%
  dplyr::left_join(sf::st_drop_geometry(sf_regrid_aadt_cbg), by = "GEOID20") %>%
  dplyr::left_join(sf::st_drop_geometry(sf_regrid_hpms_cbg), by = "GEOID20") %>%
  dplyr::left_join(
    sf::st_drop_geometry(sf_barrier_factors_nh), by = "GEOID20"
  ) %>%
  dplyr::left_join(sf::st_drop_geometry(sf_grid_prox), by = "GEOID20") %>%
  dplyr::left_join(sf::st_drop_geometry(sf_co2_emis), by = "GEOID20")

################################################################################
# CSI data descriptions.
df_csi_summary <- vtable::sumtable(sf_csi, out = "return")
names(df_sld)[1] <- "Variable"
# dplyr::left_join(df_sld, df_csi_summary, by = "Variable")

################################################################################
# Scale CSI variables.
sf_csi_geoid <- sf_csi[, c("GEOID20")]
df_csi_variables <- sf::st_drop_geometry(
  sf_csi[, grep("GEOID20", names(sf_csi), invert = TRUE)]
)
df_csi_scale <- data.frame(sapply(df_csi_variables, scale))
sf_csi_scale <- cbind(sf_csi_geoid, df_csi_scale)
df_csi_geoid_scale <- cbind(
  GEOID20 = sf_csi_geoid$GEOID20,
  df_csi_scale
)

################################################################################
# Build data.frame for distributional characteristics (paper table).
vtable::st(
  df_csi_variables,
  add.median = TRUE,
  fit.page = "\\textwidth",
  digits = 2,
  out = "latex"
)

################################################################################
# Save output.
chr_csi_scale <- file.path(dir_output, "a_09", "sf_csi_scale.rds")
if (!file.exists(chr_csi_scale)) saveRDS(sf_csi_scale, chr_csi_scale)

chr_csi_geoid_scale <- file.path(dir_output, "a_09", "df_csi_geoid_scale.rds")
if (!file.exists(chr_csi_geoid_scale)) {
  saveRDS(df_csi_geoid_scale, chr_csi_geoid_scale)
}

chr_csi_raw_path <- file.path(dir_output, "a_09", "sf_csi_raw.rds")
if (!file.exists(chr_csi_raw_path)) saveRDS(sf_csi, chr_csi_raw_path)
