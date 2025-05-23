################################################################################
# Merge all Community Serverance Index (CSI) variables at SmartLocationsDatabase
# census block groups for New Haven, Connecticut.

################################################################################
# Source variables from a_00_initiate.R
source(file.path(here::here(), "scripts", "a_00_initiate.R"))

################################################################################
# List output files.
chr_output_files <- list.files(
  dir_output, full.names = TRUE, recursive = TRUE, pattern = ".rds"
)
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
# Order CSI variables.
chr_csi_order <- c(
  "GEOID20", "interstate_highway_prox", "freeways_expressways_prox",
  "other_princ_arter_prox", "tertiary_prox", "residential_prox",
  "motorway_prox", "primary_prox", "secondary_prox", "trunk_prox",
  "autom_netw_dens", "autom_inters_dens", "barrier_factor_osm",
  "barrier_factor_faf5", "aadt_esri_point", "aadt_fhwa_segm",
  "co2_emis_per_m2", "pedest_netw_dens", "street_no_autom_inters_dens",
  "NatWalkInd"
)
sf_csi_order <- sf_csi[, chr_csi_order]

################################################################################
# CSI data descriptions.
df_csi_summary <- vtable::sumtable(sf_csi_order[, -1], out = "return")
names(df_sld)[1] <- "Variable"

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
# Save output.
chr_csi_scale <- file.path(dir_output, "a_09", "sf_csi_scale.rds")
saveRDS(sf_csi_scale, chr_csi_scale)

chr_csi_geoid_scale <- file.path(dir_output, "a_09", "df_csi_geoid_scale.rds")
saveRDS(df_csi_geoid_scale, chr_csi_geoid_scale)

chr_csi_raw_path <- file.path(dir_output, "a_09", "sf_csi_raw.rds")
saveRDS(sf_csi, chr_csi_raw_path)

chr_csi_summary_path <- file.path(dir_output, "a_09", "df_csi_summary.csv")
write.csv(df_csi_summary, chr_csi_summary_path)
