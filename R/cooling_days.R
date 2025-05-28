################################################################################
#' Calculate "cooling degree day" binary indicator per for point or polygon
#' spatial units.
#' @param years integer. Year or years.
#' @param tmax_path chr(1). Directory with maximum temperature netCDF files.
#' @param vp_path chr(1). Directory with vapor pressure netCDF files.
#' @param threshold integer(1). Heat index temperature threshold. Default is
#' 65.
#' @param locs sf(1). Extraction locations as an `sf` object. Can be points
#' or polygons. Polygons will be used to calculate weighted mean.
#' @param locs_id chr(1). Column with unique identifier for each location
#' (e.g. "GEOID" or "GEOID20").
#' @importFrom terra rast values longnames units varnames lapp nlyr crs time
#' @importFrom exactextractr exact_extract
#' @importFrom sf st_transform
#' @export
cooling_degree_days <- function(
  years,
  tmax_path,
  vp_path,
  threshold = 65,
  locs,
  locs_id
) {
  # Check inputs.
  stopifnot(!is.null(years))
  stopifnot(!is.null(tmax_path))
  stopifnot(!is.null(vp_path))
  stopifnot(!is.null(locs))
  stopifnot(!is.null(locs_id))
  stopifnot(is.numeric(years))

  # Set empty data.frame for storing output.
  df_cdd <- data.frame()

  # Iterate for each year.
  for (y in seq_along(years)) {
    # Index to year of interest.
    int_year <- years[y]

    # List all dates in year[y] (flexible to accomodate leap years).
    dates <- seq(
      lubridate::ymd(paste0(int_year, "-01-01")),
      lubridate::ymd(paste0(int_year, "-12-31")),
      by = "1 day"
    )

    # Define start/end of cooling period (May 01 to September 31).
    chr_start_cool <- paste0(int_year, "-05-01")
    chr_end_cool <- paste0(int_year, "-09-30")

    # Index value for May 01.
    int_start_cool_index <- grep(chr_start_cool, dates)
    # Index value for September 30.
    int_end_cool_index <- grep(chr_end_cool, dates)

    # Import `tmax` data from file.
    chr_tmax_path <- list.files(
      tmax_path,
      pattern = paste0(int_year),
      full.names = TRUE
    )
    stopifnot(length(chr_tmax_path) == 1)
    rast_tmax <- terra::rast(chr_tmax_path)

    # Subset to May 01 to September 31.
    rast_tmax_cooling <- rast_tmax[[int_start_cool_index:int_end_cool_index]]

    # Calculate saturated vapor pressure from `tmax` and store as `terra::rast`.
    rast_satvp <- rast_tmax_cooling
    terra::values(rast_satvp) <- GetSatVP(terra::values(rast_tmax_cooling))
    terra::varnames(rast_satvp) <- "satVP"
    terra::longnames(rast_satvp) <- "Saturated Vapor Pressure"
    terra::units(rast_satvp) <- "kPa"
    names(rast_satvp) <- gsub("tmax", "satVP", names(rast_satvp))

    # Convert `tmax` data to Fahrenheit.
    rast_tmax_cooling_f <- C_to_F(rast_tmax_cooling)

    # Import `vp` data from file.
    chr_vp_path <- list.files(
      vp_path,
      pattern = paste0(int_year),
      full.names = TRUE
    )
    stopifnot(length(chr_vp_path) == 1)
    rast_vp <- terra::rast(chr_vp_path)

    # Subset to May 01 to September 30.
    rast_vp_cooling <- rast_vp[[int_start_cool_index:int_end_cool_index]]

    # Calculate relative humidity from `satVP` and `vp`.
    rast_rh <- (rast_vp_cooling / rast_satvp) * 100
    terra::varnames(rast_rh) <- "rh"
    terra::longnames(rast_rh) <- "relative humidity"
    terra::units(rast_rh) <- "percent (%)"
    names(rast_rh) <- gsub("vp", "rh", names(rast_rh))

    # Calculate heat index by combining tmax_f[n] with rh[n].
    rast_hi <- terra::rast()
    for (n in seq_len(terra::nlyr(rast_tmax_cooling_f))) {
      rast_tmax_rh <- c(rast_tmax_cooling_f[[n]], rast_rh[[n]])
      rast_hi_lapp <- terra::lapp(rast_tmax_rh, GetHeatIndex)
      rast_hi <- c(rast_hi, rast_hi_lapp)
    }
    terra::time(rast_hi) <- terra::time(rast_tmax_cooling_f)
    names(rast_hi) <- gsub("tmax", "hi", names(rast_tmax_cooling_f))
    terra::varnames(rast_hi) <- "heat index"
    terra::longnames(rast_hi) <- "heat index"

    # Binary indicator for cooling degree day (> `threshold`).
    rast_cdd <- (rast_hi > 65) * 1
    terra::varnames(rast_cdd) <- "cooling degree day (binary)"
    names(rast_cdd) <- paste0("cdd_", gsub("-", "", terra::time(rast_cdd)))

    # Extract cooling degree day indicator for locations.
    for (c in seq_len(terra::nlyr(rast_cdd))) {
      if ("POLYGON" %in% as.character(unique(sf::st_geometry_type(locs)))) {
        num_cdd <- exactextractr::exact_extract(
          rast_cdd[[c]],
          locs,
          weights = "area",
          fun = "sum",
          progress = FALSE
        )
      } else {
        num_cdd <- terra::extract(
          rast_cdd[[c]],
          terra::vect(locs),
          method = "simple",
          ID = FALSE,
          bind = FALSE,
          na.rm = TRUE
        )
      }

      # Merge with location ID and time values.
      df_cdd_c <- data.frame(
        locs_id = locs[[locs_id]],
        time = as.Date(
          gsub("cdd_", "", names(rast_cdd[[c]])),
          format = "%Y%m%d"
        ),
        CDD = num_cdd
      )
      names(df_cdd_c) <- c(locs_id, "time", "CDD")

      # Merge with other years' data.
      df_cdd <- rbind(df_cdd, df_cdd_c)
    }
  }
  return(df_cdd)
}

################################################################################
#' Function to calculate saturated vapor pressure.
GetSatVP <- function(t) {
  ifelse(
    t > 0,
    exp(34.494 - (4924.99 / (t + 237.1))) / (t + 105)^1.57,
    exp(43.494 - (6545.8 / (t + 278))) / (t + 868)^2
  )
}

################################################################################
#' Function for converting celcius to farenheit.
C_to_F <- function(T.celsius, round = 2) {
  T.fahrenheit <- (9 / 5) * T.celsius + 32
  T.fahrenheit <- round(T.fahrenheit, digits = round)

  return(T.fahrenheit)
}

################################################################################
#' Calculate heat index based on temperature and relative humidity.
GetHeatIndex <- function(t = NA, rh = NA) {
  ifelse(
    is.na(rh) | is.na(t) | is.nan(rh) | is.nan(t),
    NA,
    ifelse(
      t <= 40,
      t,
      ifelse(
        -10.3 + 1.1 * t + 0.047 * rh < 79,
        -10.3 + 1.1 * t + 0.047 * rh,
        ifelse(
          rh <= 13 & t >= 80 & t <= 112,
          -42.379 +
            2.04901523 * t +
            10.14333127 * rh -
            0.22475541 * t * rh -
            6.83783 * 10^-3 * t^2 -
            5.481717 * 10^-2 * rh^2 +
            1.22874 * 10^-3 * t^2 * rh +
            8.5282 * 10^-4 * t * rh^2 -
            1.99 * 10^-6 * t^2 * rh^2 -
            (13 - rh) / 4 * ((17 - abs(t - 95)) / 17)^0.5,
          ifelse(
            rh > 85 & t >= 80 & t <= 87,
            -42.379 +
              2.04901523 * t +
              10.14333127 * rh -
              0.22475541 * t * rh -
              6.83783 * 10^-3 * t^2 -
              5.481717 * 10^-2 * rh^2 +
              1.22874 * 10^-3 * t^2 * rh +
              8.5282 * 10^-4 * t * rh^2 -
              1.99 * 10^-6 * t^2 * rh^2 +
              0.02 * (rh - 85) * (87 - t),
            -42.379 +
              2.04901523 * t +
              10.14333127 * rh -
              0.22475541 * t * rh -
              6.83783 * 10^-3 * t^2 -
              5.481717 * 10^-2 * rh^2 +
              1.22874 * 10^-3 * t^2 * rh +
              8.5282 * 10^-4 * t * rh^2 -
              1.99 * 10^-6 * t^2 * rh^2
          )
        )
      )
    )
  )
}
