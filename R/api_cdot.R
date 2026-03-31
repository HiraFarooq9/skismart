# =============================================================================
# api_cdot.R
# Colorado Department of Transportation (CDOT) API Integration
#
# Fetches road condition data from the CDOT / CoTrip REST API.
# Only one endpoint is used: /roadConditions
#
# API details:
#   Base URL : https://data.cotrip.org/api/v1
#   Auth     : API key passed as query param (?apiKey=...)
#
# Response structure (confirmed from live API):
#   Each feature = one road segment with:
#     properties.id                  — segment id
#     properties.name                — full segment description
#     properties.nameId              — short pass/area name (e.g. "Monarch Pass")
#     properties.routeName           — road name (e.g. "US 50")
#     properties.primaryLatitude     — start lat
#     properties.primaryLongitude    — start lon
#     properties.secondaryLatitude   — end lat
#     properties.secondaryLongitude  — end lon
#     properties.currentConditions   — list of condition entries, each with:
#       conditionId          — numeric type code
#       conditionDescription — label for the condition type
#       additionalData       — free-text detail (forecast, chain law text, etc.)
#       updateTime           — Unix timestamp in milliseconds
#       startTime / endTime  — validity window in Unix ms
#       sourceType           — e.g. "NDFD" (forecast) or "MANUAL"
#
# Output: one row per condition entry (a segment with 3 conditions → 3 rows).
# This makes spatial filtering and LLM formatting straightforward.
#
# Dependencies: httr2, dplyr, purrr
# =============================================================================

library(httr2)
library(dplyr)
library(purrr)


# -----------------------------------------------------------------------------
# CONSTANTS
# -----------------------------------------------------------------------------

CDOT_BASE_URL <- "https://data.cotrip.org/api/v1"


# -----------------------------------------------------------------------------
# INTERNAL HELPERS
# -----------------------------------------------------------------------------

#' Build and perform a CDOT API GET request
.cdot_get <- function(endpoint, api_key, params = list()) {
  url <- paste0(CDOT_BASE_URL, endpoint)

  req <- request(url) |>
    req_url_query(apiKey = api_key, !!!params) |>
    req_timeout(30) |>
    req_error(is_error = function(resp) FALSE)

  resp <- req_perform(req)

  if (resp_status(resp) != 200) {
    warning(sprintf(
      "CDOT API returned status %d for '%s'. Body: %s",
      resp_status(resp), endpoint,
      tryCatch(resp_body_string(resp), error = function(e) "<unreadable>")
    ))
    return(NULL)
  }

  tryCatch(
    resp_body_json(resp, simplifyVector = FALSE),
    error = function(e) {
      warning(sprintf("Failed to parse CDOT response for '%s': %s", endpoint, e$message))
      NULL
    }
  )
}


#' Convert a Unix millisecond timestamp to a human-readable string
.ms_to_datetime <- function(ms) {
  if (is.null(ms) || is.na(ms)) return(NA_character_)
  tryCatch(
    format(as.POSIXct(as.numeric(ms) / 1000, origin = "1970-01-01", tz = "America/Denver"),
           "%Y-%m-%d %H:%M %Z"),
    error = function(e) NA_character_
  )
}


#' Safely pull a scalar field from a list, returning NA if missing
.sf <- function(x, key) {
  val <- x[[key]]
  if (is.null(val) || length(val) == 0) NA_character_ else as.character(val[[1]])
}


# -----------------------------------------------------------------------------
# MAIN FETCH
# -----------------------------------------------------------------------------

#' Fetch road conditions from CDOT
#'
#' Returns one row per condition entry per segment. A single road segment
#' typically has multiple condition entries (e.g. one forecast, one surface
#' condition, one chain-law notice).
#'
#' @param api_key Character. CDOT API key.
#'
#' @return Data frame with columns:
#'   segment_id, segment_name, pass_name, route,
#'   condition_id, condition_type, additional_data,
#'   source_type, valid_from, valid_to, updated_at,
#'   start_lat, start_lon, end_lat, end_lon
#'   Returns an empty data frame on failure.
fetch_road_conditions <- function(api_key) {
  raw <- .cdot_get("/roadConditions", api_key)

  if (is.null(raw) || length(raw) == 0) {
    warning("fetch_road_conditions: no data returned.")
    return(.empty_df())
  }

  features <- if (!is.null(raw$features)) raw$features else raw

  map_dfr(features, function(f) {
    props <- f$properties
    if (is.null(props)) return(NULL)

    # Segment-level fields
    segment_id   <- as.character(props$id %||% NA)
    segment_name <- as.character(props$name %||% NA)
    pass_name    <- as.character(props$nameId %||% NA)
    route        <- as.character(props$routeName %||% NA)
    start_lat    <- as.numeric(props$primaryLatitude %||% NA)
    start_lon    <- as.numeric(props$primaryLongitude %||% NA)
    end_lat      <- as.numeric(props$secondaryLatitude %||% NA)
    end_lon      <- as.numeric(props$secondaryLongitude %||% NA)

    conditions <- props$currentConditions
    if (is.null(conditions) || length(conditions) == 0) {
      # Segment has no active conditions — still include it with NAs
      return(tibble(
        segment_id, segment_name, pass_name, route,
        condition_id   = NA_character_,
        condition_type = NA_character_,
        additional_data = NA_character_,
        source_type    = NA_character_,
        valid_from     = NA_character_,
        valid_to       = NA_character_,
        updated_at     = NA_character_,
        start_lat, start_lon, end_lat, end_lon
      ))
    }

    map_dfr(conditions, function(cond) {
      tibble(
        segment_id      = segment_id,
        segment_name    = segment_name,
        pass_name       = pass_name,
        route           = route,
        condition_id    = as.character(cond$conditionId %||% NA),
        condition_type  = as.character(cond$conditionDescription %||% NA),
        additional_data = as.character(cond$additionalData %||% NA),
        source_type     = as.character(cond$sourceType %||% NA),
        valid_from      = .ms_to_datetime(cond$startTime),
        valid_to        = .ms_to_datetime(cond$endTime),
        updated_at      = .ms_to_datetime(cond$updateTime),
        start_lat       = start_lat,
        start_lon       = start_lon,
        end_lat         = end_lat,
        end_lon         = end_lon
      )
    })
  })
}

.empty_df <- function() {
  tibble(
    segment_id = character(), segment_name = character(),
    pass_name = character(), route = character(),
    condition_id = character(), condition_type = character(),
    additional_data = character(), source_type = character(),
    valid_from = character(), valid_to = character(), updated_at = character(),
    start_lat = double(), start_lon = double(),
    end_lat = double(), end_lon = double()
  )
}


# -----------------------------------------------------------------------------
# PROBE (interactive debugging)
# -----------------------------------------------------------------------------

#' Inspect the raw CDOT response — use this to verify field names
#'
#' @param api_key Character. CDOT API key.
#' @param n       Integer. Number of features to print (default 2).
probe_cdot_endpoint <- function(api_key, n = 2) {
  raw <- .cdot_get("/roadConditions", api_key)
  if (is.null(raw)) {
    message("No response received.")
    return(invisible(NULL))
  }
  features <- if (!is.null(raw$features)) raw$features else raw
  message(sprintf("Total segments returned: %d", length(features)))
  message(sprintf("\nFirst segment properties:"))
  print(utils::str(features[[1]]$properties, max.level = 1))
  message(sprintf("\nFirst condition entry on first segment:"))
  print(utils::str(features[[1]]$properties$currentConditions[[1]], max.level = 1))
  invisible(raw)
}


# -----------------------------------------------------------------------------
# NULL COALESCING
# -----------------------------------------------------------------------------

`%||%` <- function(a, b) if (!is.null(a)) a else b
