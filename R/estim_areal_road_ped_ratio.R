################################################################################
# Sourced from https://github.com/jaime-benavides/community_severance_nyc/blob/main/code/functions/functions.R

################################################################################
estim_areal_road_ped_ratio <- function(sp_unit_pos) {
  sp_unit <- grid_contxt[sp_unit_pos[1], ]
  influence_area <- sf::st_buffer(sp_unit, dist = 804.672)
  roads_buffer_id <- sapply(sf::st_intersects(roads_contxt, influence_area), function(x) {
    length(x) > 0
  })
  roads_local <- roads_contxt[roads_buffer_id, ]
  sidewalk_buffer_id <- sapply(sf::st_intersects(edges_nyc, influence_area), function(x) {
    length(x) > 0
  })
  sidewalk_local <- edges_nyc[sidewalk_buffer_id, ]
  if (nrow(roads_local) > 0) {
    roads_local_area <- sum(roads_local$st_area, na.rm = T)
    bikes_local_area <- sum(roads_local$bike_lane_area, na.rm = T)
    sidewalk_local_area <- sum(sidewalk_local$sidewalk_area_m, na.rm = T)
    roads_local_area <- roads_local_area - bikes_local_area
    streets_local_area <- roads_local_area + bikes_local_area + sidewalk_local_area
    sidewalk_to_road <- sidewalk_local_area / roads_local_area
    sidewalk_to_street <- sidewalk_local_area / streets_local_area
    road_to_street <- roads_local_area / streets_local_area
  } else {
    streets_local_area <- NA
    roads_local_area <- NA
    bikes_local_area <- NA
    sidewalk_local_area <- NA
    sidewalk_to_road <- NA
    sidewalk_to_street <- NA
    road_to_street <- NA
  }
  areal_ratio <- c(sp_unit_pos[2], streets_local_area, roads_local_area, bikes_local_area, sidewalk_local_area, sidewalk_to_road, sidewalk_to_street, road_to_street)
  return(areal_ratio)
}
