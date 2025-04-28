################################################################################
# Sourced from https://github.com/jaime-benavides/community_severance_nyc/blob/main/code/functions/functions.R

################################################################################
# 1. kriging from mid point
# adapted from Criado et al. (2022) https://earth.bsc.es/gitlab/es/universalkriging/-/blob/production/general/UK_mean.R
regrid_ok <- function(non_uniform_data, target_grid, crs_sim = "+proj=utm +zone=31 +ellps=intl +units=m +no_defs") {
  if (isTRUE(class(non_uniform_data) != "SpatialPointsDataFrame")) {
    non_uniform_data <- sp::SpatialPointsDataFrame(non_uniform_data[, c("X", "Y")], non_uniform_data)
    sp::proj4string(non_uniform_data) <- crs_sim
  }
  vf_ok <- automap::autofitVariogram(aadt ~ 1, non_uniform_data)
  ok_regular <- gstat(formula = aadt ~ 1, data = non_uniform_data, model = vf_ok$var_model, nmax = 20)
  regular <- predict(ok_regular, target_grid)
  regular_sf <- sf::st_as_sf(regular)
  # regular <- sp::spTransform(regular, sp::CRS("+proj=longlat"))
  # pixels <- sp::SpatialPixelsDataFrame(regular,tolerance = 0.99, as.data.frame(regular[,"var1.pred"]))
  # mean_raster <- raster::raster(pixels[,'var1.pred'])
  # return(mean_raster)}
  return(regular_sf)
}
