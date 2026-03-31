# =============================================================================
# api_routing.R
# OpenRouteService (ORS) Routing Integration
#
# Fetches a driving route between a user's start point and the top-ranked
# ski resort. Returns route geometry (as a data frame of lat/lon waypoints)
# plus summary metadata (distance, duration).
#
# The route geometry is used by route_conditions.R to spatially filter CDOT
# events that fall along the route corridor.
#
# API details:
#   Provider : GraphHopper Routing API
#   Endpoint : GET /api/1/route?point=lat,lon&point=lat,lon&vehicle=car
#   Auth     : API key as query param (?key=...)
#   Response : JSON with GeoJSON geometry (points_encoded=false)
#   Docs     : https://docs.graphhopper.com/#operation/getRoute
#   Sign up  : https://www.graphhopper.com/ (free: 500 req/day, no card)
#
# Coordinate convention: lat,lon in query params (note: opposite of GeoJSON)
# Response geometry coordinates are [lon, lat] (GeoJSON order)
#
# Dependencies: httr2, dplyr
# =============================================================================

library(httr2)
library(dplyr)
library(purrr)


# -----------------------------------------------------------------------------
# CONSTANTS
# -----------------------------------------------------------------------------

GRAPHHOPPER_BASE_URL <- "https://graphhopper.com/api/1/route"


# -----------------------------------------------------------------------------
# MAIN FUNCTION
# -----------------------------------------------------------------------------

#' Fetch a driving route from start to resort via OpenRouteService
#'
#' @param start_lat   Numeric. User's starting latitude.
#' @param start_lon   Numeric. User's starting longitude.
#' @param resort_lat  Numeric. Resort latitude (from resorts.csv).
#' @param resort_lon  Numeric. Resort longitude (from resorts.csv).
#' @param gh_api_key Character. GraphHopper API key.
#'
#' @return A named list with:
#'   $waypoints      — data frame: seq, lat, lon (ordered along route)
#'   $distance_miles — total route distance in miles
#'   $duration_mins  — estimated drive time in minutes
#'   $bbox           — numeric vector: min_lon, min_lat, max_lon, max_lat
#'   $raw            — full parsed API response (for debugging)
#'   Returns NULL on failure.
fetch_route <- function(start_lat, start_lon, resort_lat, resort_lon, gh_api_key) {

  # GraphHopper: two point params as "lat,lon" (note: lat first, unlike GeoJSON)
  req <- request(GRAPHHOPPER_BASE_URL) |>
    req_url_query(
      point          = c(paste(start_lat,  start_lon,  sep = ","),
                         paste(resort_lat, resort_lon, sep = ",")),
      vehicle        = "car",
      points_encoded = "false",
      key            = gh_api_key,
      .multi         = "explode"
    ) |>
    req_timeout(30) |>
    req_error(is_error = function(resp) FALSE)

  resp <- req_perform(req)

  if (resp_status(resp) != 200) {
    warning(sprintf(
      "GraphHopper API returned status %d. Body: %s",
      resp_status(resp),
      tryCatch(resp_body_string(resp), error = function(e) "<unreadable>")
    ))
    return(NULL)
  }

  raw <- tryCatch(
    resp_body_json(resp, simplifyVector = FALSE),
    error = function(e) {
      warning(paste("Failed to parse ORS response:", e$message))
      NULL
    }
  )

  if (is.null(raw)) return(NULL)

  .parse_ors_response(raw)
}


# -----------------------------------------------------------------------------
# INTERNAL PARSER
# -----------------------------------------------------------------------------

#' Parse the GraphHopper JSON response into a tidy list
#'
#' GraphHopper response structure (points_encoded=false):
#'   paths[[1]]$distance                    — meters
#'   paths[[1]]$time                        — milliseconds
#'   paths[[1]]$points$coordinates          — list of [lon, lat] pairs
#'
#' @param raw List. Parsed JSON from GraphHopper.
#' @return Named list (see fetch_route return docs), or NULL on parse failure.
.parse_ors_response <- function(raw) {
  tryCatch({
    path <- raw$paths[[1]]
    if (is.null(path)) stop("No paths in GraphHopper response.")

    distance_m  <- path$distance %||% NA_real_
    duration_ms <- path$time     %||% NA_real_

    coords <- path$points$coordinates
    if (is.null(coords) || length(coords) == 0) stop("No coordinates in GraphHopper response.")

    lons <- sapply(coords, `[[`, 1)
    lats <- sapply(coords, `[[`, 2)

    waypoints <- data.frame(
      seq = seq_along(lats),
      lat = lats,
      lon = lons
    )

    bbox <- c(
      min_lon = min(lons),
      min_lat = min(lats),
      max_lon = max(lons),
      max_lat = max(lats)
    )

    list(
      waypoints      = waypoints,
      distance_miles = as.numeric(distance_m) / 1609.34,
      duration_mins  = as.numeric(duration_ms) / 60000,  # ms → minutes
      bbox           = bbox,
      raw            = raw
    )
  }, error = function(e) {
    warning(paste(".parse_graphhopper_response failed:", e$message))
    NULL
  })
}


# -----------------------------------------------------------------------------
# NULL COALESCING (base R doesn't have %||%)
# -----------------------------------------------------------------------------

`%||%` <- function(a, b) if (!is.null(a)) a else b
