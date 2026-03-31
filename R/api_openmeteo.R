# =============================================================================
# api_openmeteo.R
# Open-Meteo API Integration Layer
#
# Fetches hourly weather forecast and recent history for a given coordinate
# and trip date, then extracts and unit-converts the variables needed by
# compute_weather_score() and compute_terrain_open_score().
#
# API details:
#   Base URL : https://api.open-meteo.com/v1/forecast
#   Auth     : None required (free, open access)
#   Docs     : https://open-meteo.com/en/docs
#
# Coordinate note:
#   Weather is queried at the resort's primary coordinate (base lodge lat/lon).
#   Future iterations could improve accuracy by querying at mid-mountain
#   elevation.
#
# Unit conversions (all performed here; scoring functions receive US units):
#   temperature_2m  : °C  → °F      (temp_c * 9/5 + 32)
#   windspeed_10m   : km/h → mph    (* 0.621371)
#   snow_depth      : m   → inches  (* 39.3701)
#   snowfall        : cm  → inches  (* 0.393701)
#   snowfall rate   : cm/hr → mm/hr (* 10)       [for precip type classifier]
#   precipitation   : mm/hr — kept as-is         [precip multiplier uses mm/hr]
#
# Dependencies: httr2, dplyr, lubridate
# =============================================================================

library(httr2)
library(dplyr)
library(lubridate)


# -----------------------------------------------------------------------------
# CONSTANTS
# -----------------------------------------------------------------------------

OPENMETEO_BASE_URL <- "https://api.open-meteo.com/v1/forecast"

# Hourly variables to request from Open-Meteo
HOURLY_VARS <- paste(c(
  "temperature_2m",   # °C
  "windspeed_10m",    # km/h
  "snow_depth",       # m (snowpack total)
  "snowfall",         # cm (hourly increment)
  "precipitation",    # mm (total: rain + snow water equivalent)
  "rain"              # mm (liquid rain component)
), collapse = ",")

# Ski day window: hours considered "on-mountain" time
SKI_HOUR_START <- 8   # 8am
SKI_HOUR_END   <- 16  # 4pm


# -----------------------------------------------------------------------------
# INTERNAL: Build and execute the Open-Meteo request
# -----------------------------------------------------------------------------
# Fetches raw hourly data for the given coordinate.
# Requests 3 days of history (for 72hr snowfall lookback) plus up to 10 days
# of forecast.
#
# Returns: a data frame with one row per hour, columns for each weather var
#          plus a parsed `datetime` column (UTC)
# -----------------------------------------------------------------------------

.fetch_raw_openmeteo <- function(lat, lon, forecast_days = 10) {

  resp <- request(OPENMETEO_BASE_URL) |>
    req_url_query(
      latitude       = round(lat, 6),
      longitude      = round(lon, 6),
      hourly         = HOURLY_VARS,
      past_days      = 3,            # 72hr lookback for snowfall accumulation
      forecast_days  = forecast_days,
      timezone       = "auto"        # returns times in local timezone
    ) |>
    req_error(is_error = \(resp) FALSE) |>  # handle errors manually below
    req_perform()

  if (resp_status(resp) != 200) {
    stop(paste0(
      "Open-Meteo API error (HTTP ", resp_status(resp), "): ",
      resp_body_string(resp)
    ))
  }

  body <- resp_body_json(resp, simplifyVector = TRUE)

  # Parse hourly data into a data frame
  hourly <- as.data.frame(body$hourly)
  hourly$datetime <- ymd_hm(hourly$time, tz = body$timezone)
  hourly$date     <- as.Date(hourly$datetime, tz = body$timezone)
  hourly$hour     <- hour(hourly$datetime)

  hourly
}


# -----------------------------------------------------------------------------
# INTERNAL: Unit conversions
# -----------------------------------------------------------------------------

.convert_units <- function(hourly_df) {
  hourly_df |>
    mutate(
      temp_f        = temperature_2m * 9 / 5 + 32,
      wind_mph      = windspeed_10m  * 0.621371,
      depth_in      = snow_depth     * 39.3701,
      snowfall_in   = snowfall       * 0.393701,  # inches per hour
      snowfall_mmhr = snowfall       * 10,         # cm/hr → mm/hr (for precip type)
      precip_mmhr   = precipitation                # mm/hr — already correct units
    )
}


# -----------------------------------------------------------------------------
# MAIN: Fetch and extract weather inputs for scoring
# -----------------------------------------------------------------------------
# For a given resort coordinate and first ski day, returns all variables needed
# by compute_weather_score() and compute_terrain_open_score().
#
# Date handling decision:
#   When a user enters a trip date range (arrival → departure), scoring is
#   anchored to the FIRST SKI DAY — the first day they will actually be on
#   the mountain. This is the decision-relevant moment: the user is choosing
#   where to go, and conditions on day 1 drive that choice. Snow depth and
#   the 72hr snowfall window are also naturally anchored to arrival.
#   The caller is responsible for deriving ski_date from user inputs
#   (e.g., arrival_date if arriving in the morning, arrival_date + 1 if
#   arriving in the evening).
#
# Inputs:
#   lat           — resort latitude
#   lon           — resort longitude
#   ski_date      — Date object (or "YYYY-MM-DD") for the first ski day
#   forecast_days — how many days ahead to fetch (default 10, max 16)
#
# Aggregation rules over ski hours (SKI_HOUR_START – SKI_HOUR_END):
#   snow_depth_in    — value at midday (12:00) on ski_date
#   snowfall_72hr_in — sum of hourly snowfall over 72hrs before ski_date 00:00
#   temp_f           — mean temperature during ski hours
#   wind_mph         — max wind speed during ski hours (worst-case for scoring)
#   precip_mmhr      — max precipitation rate during ski hours
#   snowfall_mmhr    — mean snowfall rate during ski hours (for precip classifier)
#
# Returns:
#   Named list of weather inputs ready for scoring functions, plus metadata.
#   On failure, returns list(error = <message>) so the caller can handle it.
# -----------------------------------------------------------------------------

fetch_weather_openmeteo <- function(lat, lon, ski_date, forecast_days = 10) {

  ski_date <- as.Date(ski_date)

  # Validate ski_date is within fetchable range
  today <- Sys.Date()
  if (ski_date < today - 3 | ski_date > today + forecast_days) {
    return(list(
      error = paste0(
        "Ski date ", ski_date, " is outside the fetchable range (",
        today - 3, " to ", today + forecast_days, ")."
      )
    ))
  }

  # Fetch and convert
  hourly <- tryCatch({
    .fetch_raw_openmeteo(lat, lon, forecast_days) |> .convert_units()
  }, error = function(e) {
    return(list(error = conditionMessage(e)))
  })

  if (is.list(hourly) && !is.null(hourly$error)) return(hourly)

  # --- Snow depth at midday on first ski day ---
  depth_row <- hourly |>
    filter(date == ski_date, hour == 12) |>
    slice(1)

  if (nrow(depth_row) == 0) {
    # Fallback: first available hour on ski_date
    depth_row <- hourly |>
      filter(date == ski_date) |>
      slice(1)
  }

  snow_depth_in <- if (nrow(depth_row) > 0) depth_row$depth_in else NA_real_

  # --- 72-hour snowfall accumulation before first ski day ---
  cutoff_start <- as_datetime(ski_date, tz = attr(hourly$datetime, "tzone")) - hours(72)
  cutoff_end   <- as_datetime(ski_date, tz = attr(hourly$datetime, "tzone"))

  snowfall_72hr_in <- hourly |>
    filter(datetime >= cutoff_start, datetime < cutoff_end) |>
    summarise(total = sum(snowfall_in, na.rm = TRUE)) |>
    pull(total)

  # --- Ski-hours conditions on first ski day ---
  ski_hours <- hourly |>
    filter(
      date == ski_date,
      hour >= SKI_HOUR_START,
      hour <= SKI_HOUR_END
    )

  if (nrow(ski_hours) == 0) {
    return(list(
      error = paste0("No hourly data available for ski hours on ", ski_date, ".")
    ))
  }

  temp_f        <- mean(ski_hours$temp_f,        na.rm = TRUE)
  wind_mph      <- max(ski_hours$wind_mph,        na.rm = TRUE)  # worst case
  precip_mmhr   <- max(ski_hours$precip_mmhr,     na.rm = TRUE)  # worst case
  snowfall_mmhr <- mean(ski_hours$snowfall_mmhr,  na.rm = TRUE)  # avg for type classification

  list(
    # Inputs for compute_weather_score() and compute_terrain_open_score()
    snow_depth_in    = round(snow_depth_in,    1),
    snowfall_72hr_in = round(snowfall_72hr_in, 1),
    temp_f           = round(temp_f,           1),
    wind_mph         = round(wind_mph,         1),
    precip_mmhr      = round(precip_mmhr,      2),
    snowfall_mmhr    = round(snowfall_mmhr,    2),

    # Metadata (useful for UI and debugging)
    resort_lat       = lat,
    resort_lon       = lon,
    ski_date         = ski_date,
    timezone         = attr(hourly$datetime, "tzone"),
    fetched_at       = Sys.time()
  )
}


# -----------------------------------------------------------------------------
# CONVENIENCE: Fetch weather for all resorts in one call
# -----------------------------------------------------------------------------
# Iterates over a resort data frame (must have `resort_id`, `resort_name`,
# `latitude`, `longitude`) and returns a list of weather inputs per resort.
#
# Inputs:
#   resorts_df    — data frame from resorts.csv (or filtered subset)
#   ski_date      — Date or "YYYY-MM-DD" for the first ski day
#   forecast_days — passed through to fetch_weather_openmeteo()
#
# Returns:
#   Named list keyed by resort_id. Each entry is either a weather inputs list
#   or list(error = ...) if the fetch failed for that resort.
# -----------------------------------------------------------------------------

fetch_weather_all_resorts <- function(resorts_df, ski_date, forecast_days = 10) {

  results <- vector("list", nrow(resorts_df))
  names(results) <- as.character(resorts_df$resort_id)

  for (i in seq_len(nrow(resorts_df))) {
    resort  <- resorts_df[i, ]
    message("Fetching weather for ", resort$resort_name, "...")

    results[[i]] <- fetch_weather_openmeteo(
      lat           = resort$latitude,
      lon           = resort$longitude,
      ski_date      = ski_date,
      forecast_days = forecast_days
    )

    # Brief pause to be respectful to the free API
    Sys.sleep(0.2)
  }

  results
}
