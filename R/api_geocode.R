# =============================================================================
# api_geocode.R
# Address Geocoding via OpenStreetMap Nominatim
#
# Converts a user-entered location string (city, address, zip code) into
# a lat/lon coordinate pair for use as the route start point.
#
# API details:
#   Base URL : https://nominatim.openstreetmap.org/search
#   Auth     : None required (free, open access)
#   Rate limit: 1 request/second — fine for interactive use
#   Docs     : https://nominatim.org/release-docs/develop/api/Search/
#
# Usage policy: Nominatim requires a descriptive User-Agent header.
# We set this to "skismart-app" to comply.
#
# Dependencies: httr2, dplyr
# =============================================================================

library(httr2)
library(dplyr)


# -----------------------------------------------------------------------------
# CONSTANTS
# -----------------------------------------------------------------------------

NOMINATIM_URL <- "https://nominatim.openstreetmap.org/search"

# Bias results toward Colorado + neighboring ski states
GEOCODE_COUNTRIES <- "us"
GEOCODE_VIEWBOX   <- "-109.5,36.5,-101.5,41.5"  # Colorado bounding box (loose)
GEOCODE_BOUNDED   <- 0  # 0 = prefer viewbox but don't restrict to it


# -----------------------------------------------------------------------------
# MAIN FUNCTION
# -----------------------------------------------------------------------------

#' Geocode a user-entered location string to lat/lon
#'
#' Accepts a city name, full address, or zip code. Returns the top result
#' with a human-readable display name for UI confirmation.
#'
#' @param location  Character. User input, e.g. "Denver", "Boulder, CO", "80302".
#' @param n_results Integer. Number of candidates to return (default 1).
#'                  Pass n_results > 1 to let the UI offer disambiguation.
#'
#' @return Data frame with columns:
#'   rank, display_name, short_name, lat, lon, result_type
#'   Returns an empty data frame if geocoding fails or returns no results.
geocode_location <- function(location, n_results = 1) {
  if (is.null(location) || nchar(trimws(location)) == 0) {
    warning("geocode_location: empty input.")
    return(.empty_geocode_df())
  }

  req <- request(NOMINATIM_URL) |>
    req_url_query(
      q          = location,
      format     = "json",
      limit      = n_results,
      countrycodes = GEOCODE_COUNTRIES,
      viewbox    = GEOCODE_VIEWBOX,
      bounded    = GEOCODE_BOUNDED,
      addressdetails = 1
    ) |>
    req_headers(`User-Agent` = "skismart-app") |>
    req_timeout(10) |>
    req_error(is_error = function(resp) FALSE)

  resp <- req_perform(req)

  if (resp_status(resp) != 200) {
    warning(sprintf("Nominatim returned status %d for query '%s'.",
                    resp_status(resp), location))
    return(.empty_geocode_df())
  }

  results <- tryCatch(
    resp_body_json(resp, simplifyVector = TRUE),
    error = function(e) {
      warning(paste("Failed to parse Nominatim response:", e$message))
      NULL
    }
  )

  if (is.null(results) || length(results) == 0) {
    warning(sprintf("geocode_location: no results for '%s'.", location))
    return(.empty_geocode_df())
  }

  .parse_nominatim_results(results)
}


# -----------------------------------------------------------------------------
# INTERNAL PARSER
# -----------------------------------------------------------------------------

.parse_nominatim_results <- function(results) {
  # results is a list of hit objects when simplifyVector = TRUE
  if (is.data.frame(results)) {
    hits <- results
  } else {
    hits <- as.data.frame(do.call(rbind, lapply(results, function(r) {
      data.frame(
        display_name = r$display_name %||% NA_character_,
        lat          = r$lat %||% NA_character_,
        lon          = r$lon %||% NA_character_,
        type         = r$type %||% NA_character_,
        stringsAsFactors = FALSE
      )
    })))
  }

  tibble(
    rank         = seq_len(nrow(hits)),
    display_name = as.character(hits$display_name),
    short_name   = .shorten_display_name(as.character(hits$display_name)),
    lat          = as.numeric(hits$lat),
    lon          = as.numeric(hits$lon),
    result_type  = as.character(hits$type)
  )
}


#' Trim a Nominatim display_name to city + state for clean UI display
#' e.g. "Denver, Denver County, Colorado, United States" → "Denver, Colorado"
.shorten_display_name <- function(names) {
  sapply(names, function(nm) {
    if (is.na(nm)) return(NA_character_)
    parts <- trimws(strsplit(nm, ",")[[1]])
    # Keep first part (city/place) + find state-like part (2 words or known abbrev)
    if (length(parts) >= 3) {
      paste(parts[1], parts[length(parts) - 1], sep = ", ")
    } else {
      nm
    }
  }, USE.NAMES = FALSE)
}

.empty_geocode_df <- function() {
  tibble(
    rank = integer(), display_name = character(), short_name = character(),
    lat = double(), lon = double(), result_type = character()
  )
}


# -----------------------------------------------------------------------------
# NULL COALESCING
# -----------------------------------------------------------------------------

`%||%` <- function(a, b) if (!is.null(a)) a else b
