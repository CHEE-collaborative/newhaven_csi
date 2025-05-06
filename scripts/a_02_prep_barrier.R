################################################################################
# Prepare barrier factor variables.

################################################################################
# Source variables from a_00_initiate.R
source(file.path(here::here(), "scripts", "a_00_initiate.R"))

################################################################################
# Import New Haven boundary.
sf_nh_boundary <- generate_newhaven(path = chr_towns_path, crs = int_crs_ct)
sf_context <- sf_nh_boundary

################################################################################
# Import grid.
chr_grid_nh <- file.path(dir_output, "a_04", "sf_grid_nh.rds")
testthat::expect_true(file.exists(chr_grid_nh))
sf_grid_nh <- readRDS(chr_grid_nh)
sf_grid_nh$id_local <- seq_len(nrow(sf_grid_nh))

################################################################################
# Import OpenStreetMap data.
chr_osm_path <- file.path(dir_input, "geofabrik_connecticut-latest.osm.pbf")
testthat::expect_true(file.exists(chr_osm_path))
sf_osm_network <- osmextract::oe_read(chr_osm_path, layer = "lines") %>%
  sf::st_transform(crs = int_crs_ct)
testthat::expect_identical("EPSG:2234", sf::st_crs(sf_osm_network)$input)

################################################################################
# Extract driving network following Larkin et al., (2017)
chr_highway_roads <- c(
  "motorway", "motorway_link", "trunk", "trunk_link",
  "primary", "secondary", "primary_link", "secondary_link"
)
sf_osm_roads <- sf_osm_network[sf_osm_network$highway %in% chr_highway_roads, ]
sf_osm_roads_nh <- sf_osm_network[sf_context, ]
testthat::expect_identical("EPSG:2234", sf::st_crs(sf_osm_roads_nh)$input)

################################################################################
# Import Freight Analysis Framework 5.0 Model Network Database data.
chr_faf5_path <- file.path(dir_input, "FAF5Network", "F5F_NEWHAVEN.shp")
testthat::expect_true(file.exists(chr_faf5_path))
sf_faf5_network <- sf::read_sf(chr_faf5_path) %>%
  sf::st_transform(crs = int_crs_ct)
testthat::expect_identical("EPSG:2234", sf::st_crs(sf_faf5_network)$input)

################################################################################
# Filter to 'Interstate', 'Principal Arterial - Other Freeways and Expressways',
# and 'Principal Arterial - Other'
sf_faf5_123 <- sf_faf5_network[sf_faf5_network$F_Class %in% c(1, 2, 3), ]
sf_faf5_123_proj <- sf::st_transform(sf_faf5_123, crs = int_crs_ct)
sf_faf5_123_nh <- sf_faf5_123_proj[sf_context, ]
testthat::expect_identical("EPSG:2234", sf::st_crs(sf_faf5_123_nh)$input)

################################################################################
# Define estimate barrier spatial units function.
estimate_barrier_spatial_units <- function(
  position,
  grid,
  dist,
  roads
) {
  # expect array to be length 2
  stopifnot(is.integer(position))
  stopifnot(length(position) == 2)

  # assign arguments
  sf_grid_context <- grid
  num_dist <- dist
  sf_roads <- roads

  # active census block group
  sf_active_cbg <- sf_grid_context[position[1], ]

  # buffer active census block group by ~0.5 miles
  sf_active_buffer <- sf::st_buffer(sf_active_cbg, dist = num_dist)

  # identify roads within 0.5 mile buffer of active CBG
  bin_roads_buffer <- sapply(
    sf::st_intersects(sf_roads, sf_active_buffer),
    function(x) length(x) > 0
  )

  # subset to roads within 0.5 mile buffer
  sf_roads_buffer <- sf_roads[bin_roads_buffer, ]

  # identify census block groups within 0.5 mile buffer of active CBG
  bin_grid_buffer <- sapply(
    sf::st_intersects(sf_grid_context, sf_active_buffer),
    function(x) length(x) > 0
  )

  # subset to CBG within 0.5 mile buffer
  sf_grid_buffer <- sf_grid_context[bin_grid_buffer, ]

  # drop active census block group (self intersection)
  sf_grid_buffer <- sf_grid_buffer[
    -which(sf_grid_buffer$id_local == sf_active_cbg$id_local),
  ]

  if (nrow(sf_grid_buffer) > 0) {
    # find visible centroids from the sf_active_cbg segment
    # obtain blocked centroids from sf_active_cbg segment and their view factor
    int_barrier_s <- sapply(
      seq_along(sf_grid_buffer$id_local),
      function(r) {
        sf_ray <- sf::st_cast(
          sf::st_union(sf_active_cbg, sf_grid_buffer[r, ]), "LINESTRING"
        )
        int_inters_loc <- lengths(
          sf::st_intersects(sf_ray, sf_roads_buffer, sparse = TRUE)
        ) > 0
        if (any(int_inters_loc == TRUE)) {
          # if there is a road between the two centroids, barrier exists
          int_factor_r <- 1L
        } else {
          # if there are no roads between the two centroids, no barrier
          int_factor_r <- 0L
        }
        return(int_factor_r)
      }
    )

    # local_id for those centroids which have a barrier with active CBG
    int_severed_cbg  <- sf_grid_buffer$id_local[which(int_barrier_s != 0)]

    if (length(int_severed_cbg) > 0) {
      int_barrier_units <- length(
        sf_grid_buffer$id_local[which(int_barrier_s != 0)]
      )
      # calculate barrier factor by dividing the total number of census block
      # groups within the 0.5 mile buffer by the number of those tracts which
      # have a road barrier between centroid and centroid of the active CBG
      # and multiplying by 100
      num_active_bf <-
        100 * length(int_severed_cbg) / length(sf_grid_buffer$id_local)

    } else {
      # if no centroids have a barrier, barrier factor is 0
      int_barrier_units <- 0L
      num_active_bf <- 0
    }

  } else {
    # if no centroids are within the 0.5 mile buffer, barrier factor is 0
    int_barrier_units <- 0L
    num_active_bf <- 0

  }

  # return local_id, number of centroids within the 0.5 mile buffer, and the
  # calculated barrier factor score
  return(
    c(
      position[2],
      int_barrier_units,
      num_active_bf
    )
  )

}

################################################################################
# Prepare grid as array for use in `estimate_barrier_spatial_units` function.
df_grid_nh <- as.data.frame(sf_grid_nh)
df_grid_nh$position <- df_grid_nh$id_local
arr_grid_nh <- as.array(
  unlist(df_grid_nh[, c("position", "id_local")])
)
dim(arr_grid_nh) <- c(arr_grid_nh = nrow(df_grid_nh), ncol = 2)

################################################################################
# Calculate barrier factor index scores from OpenStreetMap data.
list_barrier_osm <- lapply(
  seq_len(nrow(df_grid_nh)),
  function(i) {
    estimate_barrier_spatial_units(
      position = arr_grid_nh[i, ],
      grid = sf_grid_nh,
      dist = 2640L,
      roads = sf_osm_roads_nh
    )
  }
)
df_barrier_osm <- data.frame(do.call(rbind, list_barrier_osm))
colnames(df_barrier_osm) <- c(
  "id_local", "n_neighbors_osm", "barrier_factor_osm"
)
df_barrier_osm

################################################################################
# Calculate barrier factor index scores from FAF5 data.
list_barrier_faf5 <- lapply(
  seq_len(nrow(df_grid_nh)),
  function(i) {
    estimate_barrier_spatial_units(
      position = arr_grid_nh[i, ],
      grid = sf_grid_nh,
      dist = 2640L,
      roads = sf_faf5_123_nh
    )
  }
)
df_barrier_faf5 <- data.frame(do.call(rbind, list_barrier_faf5))
colnames(df_barrier_faf5) <- c(
  "id_local", "n_neighbors_faf5", "barrier_factor_faf5"
)
df_barrier_faf5

################################################################################
# Merge OSM and FAF5 barrier factor indicies and with New Haven grid.
sf_barrier_nh <- merge(
  sf_grid_nh,
  merge(df_barrier_osm, df_barrier_faf5, by = "id_local"),
  by = "id_local"
)
sf_barrier_factors_nh <- sf_barrier_nh[
  , c("GEOID20", "barrier_factor_osm", "barrier_factor_faf5")
]

################################################################################
# Save output.
chr_barrier_nh_path <- file.path(
  dir_output, "a_02", "sf_barrier_factors_nh.rds"
)
if (!file.exists(chr_barrier_nh_path)) {
  saveRDS(sf_barrier_factors_nh, chr_barrier_nh_path)
}

################################################################################
# `tmap` settings.
tmap::tmap_mode(mode = "view")
tmap::tmap_options(check.and.fix = TRUE)

################################################################################
# OpenStreetMap barrier factor map.
tmap::tm_shape(sf::st_make_valid(sf_context)) +
  tmap::tm_borders(alpha = 0.1, col = "black") +
  tmap::tm_shape(sf_barrier_nh) +
  tmap::tm_dots(
    "barrier_factor_osm", palette = "Oranges", scale = 2, style = "cont"
  ) +
  tmap::tm_shape(sf_osm_roads_nh) +
  tmap::tm_lines()

################################################################################
# FAF5 barrier factor map.
tmap::tm_shape(sf::st_make_valid(spatial_context)) +
  tmap::tm_borders(alpha = 0.1, col = "black") +
  tmap::tm_shape(sf_barrier_nh) +
  tmap::tm_dots(
    "barrier_factor_fhwa", palette = "Oranges", scale = 2, style = "cont"
  ) +
  tmap::tm_shape(sf_faf5_123_nh) +
  tmap::tm_lines()
