#c_01_estimate_community_sev_index_newhaven.R

#Aline changed:
#1. Nomenclature new york / nyc --> newhaven
#2. New Haven shapefile
#3. RRMC is actually lowercase rrmc
#4. a couple tweaks to the line of code:
#print_patterns_loc(dat[,c("MR1"), drop = FALSE],
#                   colgroups = dat[,c("column_names", "family")],
#                   pat_type = "factor",
#                   n = p,
#                   title = "FA factors",
#                   size_line = 2,
#                   size_point = 3.5)


# script aim: estimate community severance index for New Haven
# First step to load packages etc.
# 1a Declare root directory, folder locations and load essential stuff
project.folder = paste0(print(here::here()),'/')
source(paste0(project.folder,'init_directory_structure.R'))
source(paste0(functions.folder,'script_initiate.R'))

#Trying to find the rrmc function because Columbia-PRIME won't run
remotes::install_github("Columbia-PRIME/PCPhelpers")
#Using GitHub PAT from the git credential store.
#Error: Failed to install 'PCPhelpers' from GitHub:
#  HTTP error 404.
#Not Found

#Did you spell the repo owner (`Columbia-PRIME`) and repo name (`PCPhelpers`) correctly?
#  - If spelling is correct, check that you have the required permissions to access the repo.
find.package("pcpr")
#"/Library/Frameworks/R.framework/Versions/4.3-x86_64/Resources/library/pcpr"
list.files(file.path(find.package("pcpr"), "R"), full.names = TRUE)
library(pcpr)
ls("package:pcpr")

exists("RRMC")
#fALSE
exists("rrmc")
#TRUE


# set coordinate reference system
crs <- 2163

### load community severance input data
data_desc <- readRDS(paste0(generated.data.folder, "smart_location_data_subset_desc.rds"))
dta_cs_in <- readRDS(paste0(generated.data.folder, "community_severance_newhaven_input_data.rds"))

built_social_block_newhaven_comm_sev_m <-  as.matrix(dta_cs_in[,-1])


sld_us_loc <- readRDS(paste0(generated.data.folder, "smart_location_data_subset.rds"))

neighborhoods <- sf::st_read(paste0("data/raw/demography/NewHaven_NeighbhorhoodBoundaries.shp")) %>%
  sf::st_transform(crs)

newhaven_boundaries <- sf::st_union(neighborhoods)
spatial_context <- newhaven_boundaries

### estimate community severance index
c("autom_netw_dens", "autom_inters_dens", "barrier_factor_osm","barrier_factor_fhwa", "motorway_prox", "primary_prox",
  "secondary_prox", "trunk_prox", "interstate_highway_prox", "freeways_expressways_prox", "other_princ_arter_prox", "tertiary_prox", "residential_prox", 
  "aadt_esri_point", "aadt_fhwa_segm", "traffic_co2_emis", "pedest_netw_dens", "street_no_autom_inters_dens", "NatWalkInd")
family_vars <- c("Road infrastructure", "Road infrastructure", "Road infrastructure", 
                 "Road infrastructure", "Road infrastructure", "Road infrastructure", "Road infrastructure",
                 "Road infrastructure", "Road infrastructure", "Road infrastructure", "Road infrastructure", "Road infrastructure", "Road infrastructure", "Road traffic activity", "Road traffic activity", 
                 "Road traffic activity", "Pedestrian infrastructure", "Pedestrian infrastructure", "Pedestrian infrastructure" )
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
rrmc_grid <- expand.grid(eta = etas, r = rank) # rrmc will search up to rank 6
runs = 22
LOD = rep(0, ncol(data$M))
perc_test = 0.15
cores = parallel::detectCores(logical = F) /2
# 3b. Run gridsearch:
with_progress(expr = {
  rrmc_results <- vanilla_search(
    cores = cores,
    mat = data$M, 
    pcp_func = rrmc, 
    grid = rrmc_grid,
    LOD = LOD,
    perc_test = perc_test,
    runs = runs,
    save_as = paste0(generated.data.folder, "rrmc_vanilla_results_community_severance_newhaven")
  )
})
# # read results
rrmc_results <- readRDS(paste0(generated.data.folder,"rrmc_vanilla_results_community_severance_newhaven", ".rds"))
# # 
# # # 
# # # # 3c. The best parameter setting according to relative error...
# # # saveRDS(rrmc_results$summary_stats, paste0(generated.data.folder, "vanilla_search_res_rrmc.RDS"))
rrmc_results$summary_stats %>% slice_min(rel_err)
# 
# 
# # # 
# # # # 3d. Visualizing the whole gridsearch:
plot_ly(data = rrmc_results$summary_stats, x = ~eta, y = ~r, z = ~rel_err, type = "heatmap")
# # # sparsities
plot_ly(data = rrmc_results$summary_stats, x = ~eta, y = ~r, z = ~S_sparsity, type = "heatmap")

# run pcp for optimal result
pcp_outs <- rrmc(data$M, r = 2, eta = 0.028, LOD = LOD) 
# % below 0 in sparsity matrix
sum(pcp_outs$L<0)/prod(dim(pcp_outs$L)) # 3 % below 0 in L matrix
#0.0481203
sum(pcp_outs$L<(-1/2))/prod(dim(pcp_outs$L)) # 0% below -1/2
#0
# save pcp result
saveRDS(pcp_outs, file = paste0(generated.data.folder, "pcp_rrmc.rds"))
pcp_outs <- readRDS(file = paste0(generated.data.folder, "pcp_rrmc.rds"))

# run factor analysis on low rank matrix
cn <- colnames(pcp_outs$S)
data_desc <- data_desc[which(data_desc$var_name %in% cn),]

#re-order columns in low-rank matrix
cng <- data_desc[,c("var_name", "source")]
cng <- cng[which(cng$var_name %in% cn),]
colnames(pcp_outs$L) <- colnames(pcp_outs$S)
pcp_outs$L <- pcp_outs$L[,c("autom_netw_dens", "autom_inters_dens", "barrier_factor_osm","barrier_factor_fhwa", "motorway_prox", "primary_prox",
                            "secondary_prox", "trunk_prox", "interstate_highway_prox", "freeways_expressways_prox", "other_princ_arter_prox", "tertiary_prox", "residential_prox", 
                            "aadt_esri_point", "aadt_fhwa_segm", "traffic_co2_emis", "pedest_netw_dens", "street_no_autom_inters_dens", "NatWalkInd")]

#re-order columns in sparsity matrix
pcp_outs$S <- pcp_outs$S[,c("autom_netw_dens", "autom_inters_dens", "barrier_factor_osm","barrier_factor_fhwa", "motorway_prox", "primary_prox",
                            "secondary_prox", "trunk_prox", "interstate_highway_prox", "freeways_expressways_prox", "other_princ_arter_prox", "tertiary_prox", "residential_prox", 
                            "aadt_esri_point", "aadt_fhwa_segm", "traffic_co2_emis", "pedest_netw_dens", "street_no_autom_inters_dens", "NatWalkInd")]

# manuscript Figure 3b
# L matrix correlations:
graph_title <- paste0("pcp_rrmc", ": L Pearson correlation")
png(paste0(output.folder, "pcp_rrmc", "_l_matrix_correlations.png"), 900, 460)
pcp_outs$L %>% GGally::ggcorr(., method = c("pairwise.complete.obs", "pearson"),
                              label = T, label_size = 3, label_alpha = T,
                              hjust = 1, nbreaks = 10, limits = TRUE,
                              size = 4, layout.exp = 5) + ggtitle(graph_title)
dev.off()

# raw data matrix correlations
data$M <- data$M[,c("autom_netw_dens", "autom_inters_dens", "barrier_factor_osm","barrier_factor_fhwa", "motorway_prox", "primary_prox",
                    "secondary_prox", "trunk_prox", "interstate_highway_prox", "freeways_expressways_prox", "other_princ_arter_prox", "tertiary_prox", "residential_prox", 
                    "aadt_esri_point", "aadt_fhwa_segm", "traffic_co2_emis", "pedest_netw_dens", "street_no_autom_inters_dens", "NatWalkInd")]

# manuscript Figure 3a
png(paste0(output.folder, "raw_mat_corr.png"), 900, 460)
data$M %>% GGally::ggcorr(., method = c("pairwise.complete.obs", "pearson"),
                          label = T, label_size = 3, label_alpha = T,
                          hjust = 1, nbreaks = 10, limits = TRUE,
                          size = 4, layout.exp = 5) + ggtitle(graph_title)
dev.off()

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

# run factor analysis
orthos <- factors %>% purrr::map(~fa(pcp_outs$L, nfactors = ., n.obs = n, rotate = "varimax", scores = "regression"))
# explore results
orthos %>% walk(print, digits = 2, sort = T)
#Factor Analysis using method =  minres
#Call: fa(r = pcp_outs$L, nfactors = ., n.obs = n, rotate = "varimax", 
         #         scores = "regression")
#Standardized loadings (pattern matrix) based upon correlation matrix
#V  MR1     h2      u2 com
#barrier_factor_osm           3 1.00 0.9997 0.00028   1
#barrier_factor_fhwa          4 1.00 0.9997 0.00028   1
#primary_prox                 6 1.00 0.9997 0.00028   1
#motorway_prox                5 1.00 0.9997 0.00028   1
#trunk_prox                   8 1.00 0.9997 0.00028   1
#interstate_highway_prox      9 1.00 0.9997 0.00028   1
#freeways_expressways_prox   10 1.00 0.9997 0.00028   1
#other_princ_arter_prox      11 1.00 0.9997 0.00028   1
#tertiary_prox               12 1.00 0.9997 0.00028   1
#aadt_esri_point             14 1.00 0.9997 0.00028   1
#pedest_netw_dens            17 1.00 0.9997 0.00028   1
#aadt_fhwa_segm              15 1.00 0.9997 0.00028   1
#secondary_prox               7 1.00 0.9997 0.00028   1
#traffic_co2_emis            16 1.00 0.9997 0.00028   1
#street_no_autom_inters_dens 18 1.00 0.9997 0.00028   1
#NatWalkInd                  19 1.00 0.9997 0.00028   1
#residential_prox            13 1.00 0.9997 0.00028   1
#autom_netw_dens              1 1.00 0.9997 0.00028   1
#autom_inters_dens            2 0.03 0.0011 0.99894   1

#MR1
#SS loadings    18.00
#Proportion Var  0.95

#Mean item complexity =  1
#Test of the hypothesis that 1 factor is sufficient.

#df null model =  171  with the objective function =  388.55 with Chi Square =  37624.61
#df of  the model are 152  and the objective function was  235.23 

#The root mean square of the residuals (RMSR) is  0 
#The df corrected root mean square of the residuals is  0 

#The harmonic n.obs is  105 with the empirical chi square  0  with prob <  1 
#The total n.obs was  105  with Likelihood Chi Square =  22621.45  with prob <  0 

#Tucker Lewis Index of factoring reliability =  0.32
#RMSEA index =  1.186  and the 90 % confidence intervals are  1.179 NA
#BIC =  21914.05
#Fit based upon off diagonal values = 1
#Measures of factor score adequacy             
#MR1
#Correlation of (regression) scores with factors     1
#Multiple R square of scores with factors            1
#Minimum correlation of possible factor scores       1


ortho_ebics <- orthos %>% map_dbl(~.$EBIC)
best_fit <- which.min(ortho_ebics)
# visualize in table
data.frame("Factors" = factors, "EBIC" = ortho_ebics) %>% kbl(caption = "Orthogonal Models: Fit Indices") %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center") %>%
  kable_styling(bootstrap_options = c("hover", "condensed"), fixed_thead = T) %>%
  row_spec(best_fit, bold = T, color = "white", background = "#D7261E")
#orthogonal models - fit indices
# factors=1
# EBIC = -707.3995

fa_model <- orthos[[best_fit]]

print(fa_model, digits = 2)
#Factor Analysis using method =  minres
#Call: fa(r = pcp_outs$L, nfactors = ., n.obs = n, rotate = "varimax", 
#         scores = "regression")
#Standardized loadings (pattern matrix) based upon correlation matrix
#MR1     h2      u2 com
#autom_netw_dens             1.00 0.9997 0.00028   1
#autom_inters_dens           0.03 0.0011 0.99894   1
#barrier_factor_osm          1.00 0.9997 0.00028   1
#barrier_factor_fhwa         1.00 0.9997 0.00028   1
#motorway_prox               1.00 0.9997 0.00028   1
#primary_prox                1.00 0.9997 0.00028   1
#secondary_prox              1.00 0.9997 0.00028   1
#trunk_prox                  1.00 0.9997 0.00028   1
#interstate_highway_prox     1.00 0.9997 0.00028   1
#freeways_expressways_prox   1.00 0.9997 0.00028   1
#other_princ_arter_prox      1.00 0.9997 0.00028   1
#tertiary_prox               1.00 0.9997 0.00028   1
#residential_prox            1.00 0.9997 0.00028   1
#aadt_esri_point             1.00 0.9997 0.00028   1
#aadt_fhwa_segm              1.00 0.9997 0.00028   1
#traffic_co2_emis            1.00 0.9997 0.00028   1
#pedest_netw_dens            1.00 0.9997 0.00028   1
#street_no_autom_inters_dens 1.00 0.9997 0.00028   1
#NatWalkInd                  1.00 0.9997 0.00028   1

#MR1
#SS loadings    18.00
#Proportion Var  0.95

#Mean item complexity =  1
#Test of the hypothesis that 1 factor is sufficient.

#df null model =  171  with the objective function =  388.55 with Chi Square =  37624.61
#df of  the model are 152  and the objective function was  235.23 

#The root mean square of the residuals (RMSR) is  0 
#The df corrected root mean square of the residuals is  0 

#The harmonic n.obs is  105 with the empirical chi square  0  with prob <  1 
#The total n.obs was  105  with Likelihood Chi Square =  22621.45  with prob <  0 

#Tucker Lewis Index of factoring reliability =  0.32
#RMSEA index =  1.186  and the 90 % confidence intervals are  1.179 NA
#BIC =  21914.05
#Fit based upon off diagonal values = 1
#Measures of factor score adequacy             
#MR1
#Correlation of (regression) scores with factors     1
#Multiple R square of scores with factors            1
#Minimum correlation of possible factor scores       1

#STOPPED HERE
# organize loadings
loadings <- as_tibble(cbind(rownames(fa_model$loadings[]), fa_model$loadings[]))
colnames(loadings)[1] <- "Variable"
loadings <- loadings %>% mutate_at(colnames(loadings)[str_starts(colnames(loadings), "MR")], as.numeric)
loadings$Max <- colnames(loadings[, -1])[max.col(loadings[, -1], ties.method = "first")] # should be 2:5
# loadings table
# manuscript Table S1
loadings %>% kbl(caption = "Loadings") %>% kable_classic(full_width = F, html_font = "Cambria", position = "center") %>% 
  kable_styling(bootstrap_options = c("hover", "condensed"), fixed_thead = T) %>% scroll_box(width = "100%", height = "400px") 
# organize scores
scores <- as.tibble(cbind(rownames(fa_model$scores[]), fa_model$scores[])) %>% mutate_all(as.numeric)
scores$Max <- colnames(scores)[max.col(scores, ties.method = "first")]
# scores table
scores %>% kbl(caption = "Scores") %>% kable_classic(full_width = F, html_font = "Cambria", position = "center") %>% 
  kable_styling(bootstrap_options = c("hover", "condensed"), fixed_thead = T) %>% scroll_box(width = "100%", height = "400px")

# prepare loadings for plotting
fa_pats <- loadings %>% 
  dplyr::select(-Max, -Variable) %>% 
  mutate_all(as.numeric)
fa_pats <- fa_pats %>% dplyr::select(sort(colnames(.))) %>% as.matrix()
# build dataframe for plotting
dat <- cbind(colgroups_l, fa_pats)
# plot loadings
p <- 1 # 1 is for community severance index (manuscript Figure 4) and 2 for the other pattern (manuscript Figure S1)
png(paste0(output.folder, "_l_fa_", p, "_patterns.png"), 1250, 460)
#I don't have an MR2 column, only MR1
#If I want MR2, I need to force a 2-factor solution...
#print_patterns_loc(dat[,c("MR1", "MR2")], colgroups = dat[,c("column_names", "family")], pat_type = "factor", n = p, title = "FA factors", size_line = 2, size_point = 3.5)
#changed this line a lil cause it wasn't running:
#print_patterns_loc(dat[,c("MR1")], colgroups = dat[,c("column_names", "family")], pat_type = "factor", n = p, title = "FA factors", size_line = 2, size_point = 3.5)
print_patterns_loc(dat[,c("MR1"), drop = FALSE],
                   colgroups = dat[,c("column_names", "family")],
                   pat_type = "factor",
                   n = p,
                   title = "FA factors",
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
saveRDS(dat_scores, paste0(generated.data.folder, "comm_sev_fa_scores_newhaven_dta_us.rds"))
