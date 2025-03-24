#c_02_estimate_community_sev_index_newhaven_sens_anal

#Aline changed:
#1. nomenclature nyc --> new haven
#2. nybb shapefile to New Haven neighborhoods (as usual)
#3. they had MR1 and MR2, but I only have MR1
#print_patterns_loc(dat[,c("MR1", "MR2")], colgroups = dat[,c("column_names", "family")], pat_type = "factor", n = p, title = "FA factors", size_line = 2, size_point = 3.5)
#switch to:
#

# script aim: estimate community severance index when adding a variable related to sidewalk area in a census block group
# First step to load packages etc.
# 1a Declare root directory, folder locations and load essential stuff
project.folder = paste0(print(here::here()),'/')
source(paste0(project.folder,'init_directory_structure.R'))
source(paste0(functions.folder,'script_initiate.R'))

# set coordinate reference system
crs <- 2163

### load community severance input data
data_desc <- readRDS(paste0(generated.data.folder, "smart_location_data_subset_desc.rds"))
dta_cs_in <- readRDS(paste0(generated.data.folder, "community_severance_newhaven_input_data.rds"))
areal_ratio_newhaven <- readRDS(paste0(generated.data.folder, "areal_ratio_nh", ".rds"))

dta_cs_in <- dplyr::left_join(dta_cs_in, areal_ratio_nh[,c("GEOID20", "sidewalk_to_road")], by = "GEOID20") 

built_social_block_newhaven_comm_sev_m <-  as.matrix(dta_cs_in[,-1])


sld_us_loc <- readRDS(paste0(generated.data.folder, "smart_location_data_subset.rds"))


neighborhoods <- sf::st_read(paste0(demography.data.folder, "NewHaven_NeighbhorhoodBoundaries.shp")) %>%
  sf::st_transform(crs)

newhaven_boundaries <- sf::st_union(neighborhoods)
spatial_context <- newhaven_boundaries

### estimate community severance index
c("autom_netw_dens", "autom_inters_dens", "barrier_factor_osm","barrier_factor_fhwa", "motorway_prox", "primary_prox",
  "secondary_prox", "trunk_prox", "interstate_highway_prox", "freeways_expressways_prox", "other_princ_arter_prox", "tertiary_prox", "residential_prox", 
  "aadt_esri_point", "aadt_fhwa_segm", "traffic_co2_emis", "pedest_netw_dens", "street_no_autom_inters_dens", "NatWalkInd", "sidewalk_to_road")
family_vars <- c("Road infrastructure", "Road infrastructure", "Road infrastructure", 
                 "Road infrastructure", "Road infrastructure", "Road infrastructure", "Road infrastructure",
                 "Road infrastructure", "Road infrastructure", "Road infrastructure", "Road infrastructure", "Road infrastructure", "Road infrastructure", "Road traffic activity", "Road traffic activity", 
                 "Road traffic activity", "Pedestrian infrastructure", "Pedestrian infrastructure", "Pedestrian infrastructure", "Pedestrian infrastructure" )
cng <- data.frame(vars = colnames(built_social_block_newhaven_comm_sev_m), family_vars = family_vars)
cng_comm_sev_vars <- cng

## run pcp grid search
# including also rows with some na
geoids <- dta_cs_in[,
                    "GEOID20"]
dat <- built_social_block_newhaven_comm_sev_m
data <- list("M" = dat) %>% purrr::map(as.matrix)
# second vanilla search
etas <- seq(0.01,0.07, length.out=11)
rank <- 5
rrmc_grid <- expand.grid(eta = etas, r = rank) # RRMC will search up to rank 6
runs = 22
LOD = rep(0, ncol(data$M))
perc_test = 0.15
cores = parallel::detectCores(logical = F) /2
# 3b. Run gridsearch:
library(progressr)
progressr::handlers(global = TRUE)
PCPhelpers::vanilla_search
library(pcpr)
library(PCPhelpers)

with_progress(expr = {
  rrmc_results <- vanilla_search(
    cores = cores,
    mat = data$M, 
    pcp_func = rrmc, 
    grid = rrmc_grid,
    LOD = LOD,
    perc_test = perc_test,
    runs = runs,
    save_as = paste0(generated.data.folder, "rrmc_vanilla_results_community_severance_newhaven_sens_anal")
  )
})
# # read results
rrmc_results <- readRDS(paste0(generated.data.folder,"rrmc_vanilla_results_community_severance_newhaven_sens_anal", ".rds"))
# # 
# # # 
# # # # 3c. The best parameter setting according to relative error...
rrmc_results$summary_stats %>% slice_min(rel_err)
# 
# 
# # # 
# # # # 3d. Visualizing the whole gridsearch:
library(plotly)
plot_ly(data = rrmc_results$summary_stats, x = ~eta, y = ~r, z = ~rel_err, type = "heatmap")
# # # sparsities
plot_ly(data = rrmc_results$summary_stats, x = ~eta, y = ~r, z = ~S_sparsity, type = "heatmap")
# run pcp
pcp_outs <- rrmc(data$M, r = 2, eta = 0.028, LOD = LOD) 
sum(pcp_outs$L<0)/prod(dim(pcp_outs$L)) # 3 % below 0 in L matrix
#0.004285714
sum(pcp_outs$L<(-1/2))/prod(dim(pcp_outs$L)) # 0% below -1/2
#0
saveRDS(pcp_outs, file = paste0(generated.data.folder, "pcp_rrmc_csi_sens_anal.rds"))
pcp_outs <- readRDS(file = paste0(generated.data.folder, "pcp_rrmc_csi_sens_anal.rds"))
# run fa on low rank matrix
cn <- colnames(pcp_outs$S)
data_desc <- data_desc[which(data_desc$var_name %in% cn),]

cng <- data_desc[,c("var_name", "source")]
cng <- cng[which(cng$var_name %in% cn),]
colnames(pcp_outs$L) <- colnames(pcp_outs$S)
pcp_outs$L <- pcp_outs$L[,c("autom_netw_dens", "autom_inters_dens", "barrier_factor_osm","barrier_factor_fhwa", "motorway_prox", "primary_prox",
                            "secondary_prox", "trunk_prox", "interstate_highway_prox", "freeways_expressways_prox", "other_princ_arter_prox", "tertiary_prox", "residential_prox", 
                            "aadt_esri_point", "aadt_fhwa_segm", "traffic_co2_emis", "pedest_netw_dens", "street_no_autom_inters_dens", "NatWalkInd", "sidewalk_to_road")]


pcp_outs$S <- pcp_outs$S[,c("autom_netw_dens", "autom_inters_dens", "barrier_factor_osm","barrier_factor_fhwa", "motorway_prox", "primary_prox",
                            "secondary_prox", "trunk_prox", "interstate_highway_prox", "freeways_expressways_prox", "other_princ_arter_prox", "tertiary_prox", "residential_prox", 
                            "aadt_esri_point", "aadt_fhwa_segm", "traffic_co2_emis", "pedest_netw_dens", "street_no_autom_inters_dens", "NatWalkInd", "sidewalk_to_road")]

# factor analysis
ranktol <- 1e-04
L.rank <- Matrix::rankMatrix(pcp_outs$L, tol = ranktol)
scale_flag <- FALSE
pcs <- paste0("PC", 1:L.rank)
factors <- 1:L.rank
n <- nrow(pcp_outs$L)
colgroups_l <- data.frame(column_names = colnames(pcp_outs$L), 
                          family = data_desc[match(colnames(pcp_outs$L), data_desc$var_name), "source"])
colgroups_l$family <- family_vars
colgroups_m <- data.frame(column_names = colnames(data$M), 
                          family = data_desc[match(colnames(data$M), data_desc$var_name), "source"])
colgroups_m$family <- family_vars
L.eda <-PCPhelpers::eda(pcp_outs$L, pcs = pcs, cor_lbl = T, scale_flag = scale_flag, colgroups = colgroups_l, rowgroups = NULL)

orthos <- factors %>% purrr::map(~fa(pcp_outs$L, nfactors = ., n.obs = n, rotate = "varimax", scores = "regression"))

orthos %>% walk(print, digits = 2, sort = T)

ortho_ebics <- orthos %>% map_dbl(~.$EBIC)

best_fit <- which.min(ortho_ebics)

data.frame("Factors" = factors, "EBIC" = ortho_ebics) %>% kbl(caption = "Orthogonal Models: Fit Indices") %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center") %>%
  kable_styling(bootstrap_options = c("hover", "condensed"), fixed_thead = T) %>%
  row_spec(best_fit, bold = T, color = "white", background = "#D7261E")

#Orthogonal Models: Fit Indices
#Facors=1
#EBIC=-791.1708

fa_model <- orthos[[best_fit]]

print(fa_model, digits = 2)

# prepare loadings
loadings <- as_tibble(cbind(rownames(fa_model$loadings[]), fa_model$loadings[]))
colnames(loadings)[1] <- "Variable"
loadings <- loadings %>% mutate_at(colnames(loadings)[str_starts(colnames(loadings), "MR")], as.numeric)
loadings$Max <- colnames(loadings[, -1])[max.col(loadings[, -1], ties.method = "first")] # should be 2:5
loadings %>% kbl(caption = "Loadings") %>% kable_classic(full_width = F, html_font = "Cambria", position = "center") %>% 
  kable_styling(bootstrap_options = c("hover", "condensed"), fixed_thead = T) %>% scroll_box(width = "100%", height = "400px") 

# prepare scores
scores <- as_tibble(cbind(rownames(fa_model$scores[]), fa_model$scores[])) %>%
  mutate_all(as.numeric)

scores$Max <- colnames(scores)[max.col(scores, ties.method = "first")]
scores %>% kbl(caption = "Scores") %>% kable_classic(full_width = F, html_font = "Cambria", position = "center") %>% 
  kable_styling(bootstrap_options = c("hover", "condensed"), fixed_thead = T) %>% scroll_box(width = "100%", height = "400px")

# prepare loadings for plotting
fa_pats <- loadings %>% 
  dplyr::select(-Max, -Variable) %>% 
  mutate_all(as.numeric)
fa_pats <- fa_pats %>% dplyr::select(sort(colnames(.))) %>% as.matrix()
dat <- cbind(colgroups_l, fa_pats)

p <- 1 # 1 is for community severance index and 2 for the other pattern (manuscript Figure S2)
png(paste0(output.folder, "_l_fa_", p, "_patterns_csi_sens_anal.png"), 1250, 460)
#they had MR1 and MR2, but I only have MR1
#print_patterns_loc(dat[,c("MR1", "MR2")], colgroups = dat[,c("column_names", "family")], pat_type = "factor", n = p, title = "FA factors", size_line = 2, size_point = 3.5)
print_patterns_loc(
  dat[, "MR1", drop = FALSE],
  colgroups = dat[, c("column_names", "family")],
  pat_type = "factor",
  n = p,
  title = "FA factor",
  size_line = 2,
  size_point = 3.5)

dev.off()

# save normalized scores
dat_scores <- cbind(built_social_block_newhaven_comm_sev_m, scores)
dat_scores$GEOID20 <- geoids
normalize <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}
dat_scores$MR1_norm <- normalize(dat_scores$MR1)
saveRDS(dat_scores, paste0(generated.data.folder, "comm_sev_fa_scores_newhaven_dta_us_sens_anal.rds"))
