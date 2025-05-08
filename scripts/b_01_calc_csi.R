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
mat_csi_scale <- as.matrix(sf::st_drop_geometry(sf_csi_scale[,-1]))
dim(mat_csi_scale)
colnames(mat_csi_scale)

################################################################################
# Import New Haven boundary.
sf_nh_boundary <- generate_newhaven(path = chr_towns_path, crs = int_crs_ct)
sf_context <- sf_nh_boundary

################################################################################
# Classify variables.
chr_varnames <- c(
  "autom_netw_dens", "autom_inters_dens", "barrier_factor_osm",
  "barrier_factor_faf5", "motorway_prox", "primary_prox",
  "secondary_prox", "trunk_prox", "interstate_highway_prox",
  "freeways_expressways_prox", "other_princ_arter_prox", "tertiary_prox",
  "residential_prox", "aadt_esri_point", "aadt_fhwa_segm", "co2_emis_per_m2",
  "pedest_netw_dens", "street_no_autom_inters_dens", "NatWalkInd"
)
chr_varcat <- c(
  "Road infrastructure", "Road infrastructure", "Road infrastructure",
  "Road infrastructure", "Road infrastructure", "Road infrastructure",
  "Road infrastructure", "Road infrastructure", "Road infrastructure",
  "Road infrastructure", "Road infrastructure", "Road infrastructure",
  "Road infrastructure", "Road traffic activity", "Road traffic activity",
  "Road traffic activity", "Pedestrian infrastructure",
  "Pedestrian infrastructure", "Pedestrian infrastructure"
)
df_var_namecat <- data.frame(
  variable = chr_varnames,
  category = chr_varcat
)

################################################################################
# Define PCP parameters input.
num_etas <- seq(0.01, 0.07, length.out = 11)
int_rank <- 5L
df_rrmc_grid <- expand.grid(eta = num_etas, r = int_rank)
int_runs <- 22L
num_perc_test <- 0.15
num_lod <- rep(0, ncol(mat_csi_scale))

################################################################################
# Run PCP grid search.
list_rrmc_grid_result <- with_progress(
  expr = {
    pcpr::grid_search_cv(
      D = mat_csi_scale,
      pcp_fn = pcpr::rrmc,
      grid = df_rrmc_grid,
      perc_test = num_perc_test,
      num_runs = int_runs,
      parallel_strategy = "sequential"
    )
  }
)

################################################################################
# Inspect parameters results.
list_rrmc_grid_result$summary_stats
list_rrmc_grid_result$summary_stats %>% dplyr::slice_min(rel_err)

################################################################################
# Visualize grid search.
# plot_ly(
#   data = rrmc_results$summary_stats,
#   x = ~eta,
#   y = ~r,
#   z = ~rel_err,
#   type = "heatmap"
# )

################################################################################
# Visualize sparsity.
# plot_ly(
#   data = rrmc_results$summary_stats,
#   x = ~eta,
#   y = ~r,
#   z = ~S_sparsity,
#   type = "heatmap"
# )

################################################################################
# Extract best parameters according to minimum relative error.
df_rrmc_opt <- list_rrmc_grid_result$summary_stats %>%
  dplyr::slice_min(rel_err)
num_eta_opt <- df_rrmc_opt$eta
num_r_opt <- df_rrmc_opt$r

################################################################################
# Run PCP with optimal parameters.
list_rrmc_opt <- pcpr::rrmc(
  D = mat_csi_scale, r = num_r_opt, eta = num_eta_opt, LOD = num_lod
)

################################################################################
# % below 0 in sparsity matrix
sum(list_rrmc_opt$L < 0) / prod(dim(list_rrmc_opt$L))
sum(list_rrmc_opt$L < (-1 / 2)) / prod(dim(list_rrmc_opt$L))

################################################################################
# Run factor analysis on low rank matrix.
chr_lrm_names <- colnames(list_rrmc_opt$S)
# data_desc <- data_desc[which(data_desc$var_name %in% chr_lrm_names), ]

################################################################################
# Reoder columns in low-rank (L) and sparsity (S) matricies.
colnames(list_rrmc_opt$L) <- colnames(list_rrmc_opt$S)
list_rrmc_opt$L <- list_rrmc_opt$L[, chr_varnames]
list_rrmc_opt$S <- list_rrmc_opt$S[, chr_varnames]

################################################################################
# Plot low rank matrix correlations.
chr_lrm_plot_path <- file.path(dir_output, "b_01", "plot_lrm_pc.png")
# png(chr_lrm_plot_path, 900, 460)
list_rrmc_opt$L %>%
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
  ggplot2::ggtitle("Low Rank Matrix (L) Pearson Correlation")
# dev.off()

################################################################################
# Plot raw data matrix correlations.
mat_csi_scale_reorder <- mat_csi_scale[, chr_varnames]
chr_rdm_plot_path <- file.path(dir_output, "b_01", "plot_rdm_pc.png")
# png(chr_rdm_plot_path, 900, 460)
mat_csi_scale_reorder %>%
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
# dev.off()

################################################################################
# Define factor analysis variables.
num_ranktol <- 1e-04
mat_lrm <- Matrix::rankMatrix(list_rrmc_opt$L, tol = num_ranktol)
log_scale_flag <- FALSE
chr_pc <- paste0("PC", 1:mat_lrm)
num_factors <- 1:mat_lrm
num_n <- nrow(list_rrmc_opt$L)

################################################################################
# Run factor analysis.
list_csi_fa <- num_factors %>%
  purrr::map(
    ~psych::fa(
      list_rrmc_opt$L,
      nfactors = .,
      n.obs = num_n,
      rotate = "varimax",
      scores = "regression"
    )
  )
list_csi_fa %>% purrr::walk(print, digits = 2, sort = TRUE)

################################################################################
# Identify best
num_ebic <- list_csi_fa %>% purrr::map_dbl(~.$EBIC)
num_best_ebic <- which.min(num_ebic)

################################################################################
# Extract factor analysis from list.
fa_csi <- list_csi_fa[[num_best_ebic]]
fa_csi$loadings[]

################################################################################
# Organize factor analysis.
data.frame("Factors" = num_factors, "EBIC" = num_ebic) %>%
  kableExtra::kbl(caption = "Orthogonal Models: Fit Indices") %>%
  kableExtra::kable_classic(
    full_width = FALSE, html_font = "Cambria", position = "center"
  ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("hover", "condensed"),
    fixed_thead = TRUE
  ) %>%
  kableExtra::row_spec(
    num_best_ebic, bold = TRUE, color = "white", background = "#D7261E"
  )

################################################################################
# Organize loadings.
df_loadings <- dplyr::as_tibble(
  cbind(rownames(fa_csi$loadings[]), fa_csi$loadings[])
)
colnames(df_loadings)[1] <- "Variable"
df_loadings <- df_loadings %>%
  dplyr::mutate_at(
    colnames(df_loadings)[grep("MR", colnames(df_loadings))], as.numeric
  )
df_loadings$max <- colnames(df_loadings[, -1])[
  max.col(df_loadings[, -1], ties.method = "first")
]

################################################################################
# Table for loadings.
df_loadings %>%
  kableExtra::kbl(caption = "Loadings") %>%
  kableExtra::kable_classic(
    full_width = FALSE, html_font = "Cambria", position = "center"
  ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("hover", "condensed"),
    fixed_thead = TRUE
  ) %>%
  kableExtra::scroll_box(width = "100%", height = "400px")

################################################################################
# Organize scores
df_scores <- dplyr::as_tibble(
  cbind(rownames(fa_csi$scores[]), fa_csi$scores[])
) %>%
  dplyr::mutate_all(as.numeric)
df_scores$max <- colnames(df_scores)[max.col(df_scores, ties.method = "first")]

################################################################################
# Calculate weighted index score based on 3 retained components and their
# proportion of variance explained
num_mr_weights <- fa_csi$Vaccounted["Proportion Var", ]
df_scores$csi <- as.numeric(
  as.matrix(df_scores[, c("MR1", "MR3", "MR2")]) %*% num_mr_weights
)
df_scores$csi_normal <- normalize(df_scores$csi)
df_scores$csi_100 <- df_scores$csi_normal * 100

################################################################################
# Table for scores.
df_scores %>%
  kableExtra::kbl(caption = "Scores") %>%
  kableExtra::kable_classic(
    full_width = FALSE, html_font = "Cambria", position = "center"
  ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("hover", "condensed"),
    fixed_thead = TRUE
  ) %>%
  kableExtra::scroll_box(width = "100%", height = "400px")

################################################################################
# Prepare loadings.
df_pats <- df_loadings %>%
  dplyr::select(-max, -Variable) %>%
  dplyr::mutate_all(as.numeric)
mat_pats <- as.matrix(df_pats)
# df_colgroups_pats <- cbind(colgroups_l, mat_pats)

################################################################################
# Plot loadings.
chr_fa_plot_path <- file.path(dir_out, "b_01", "fa_patterns.png")
# png(chr_fa_plot_path, 1250, 460)
print_patterns_loc(
  dat[, c("MR1"), drop = FALSE],
  colgroups = dat[, c("column_names", "family")],
  pat_type = "factor",
  n = p,
  title = "FA factors",
  size_line = 2,
  size_point = 3.5
)
# dev.off()

################################################################################
# Merge CSI score with variable data.
sf_csi_variables <- merge(
  sf_csi_raw, sf::st_drop_geometry(sf_csi_scale), by = "GEOID20"
)
names(sf_csi_variables) <- gsub(
  ".x", "_raw", gsub(".y", "_scale", names(sf_csi_variables))
)
sf_csi_nh <- cbind(
  sf_csi_variables,
  df_scores[, grep("MR\\d{1}|max", names(df_scores), invert = TRUE)]
)

################################################################################
# Save output.
chr_sf_csi_path <- file.path(dir_output, "b_01", "sf_csi_nh.rds")
if (!file.exists(chr_sf_csi_path)) saveRDS(sf_csi_nh, chr_sf_csi_path)
