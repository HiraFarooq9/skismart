# =============================================================================
# fetch_caic.R
# =============================================================================
#
# PURPOSE
#   Fetches current avalanche danger forecasts from the Colorado Avalanche
#   Information Center (CAIC) API and returns a tidy, resort-level dataframe
#   for use in the SkiSmart resort-scoring pipeline.
#
# HOW IT WORKS
#   The CAIC AVID API does not return human-readable zone names. Instead:
#   1. /products/all          -> all forecast objects (danger ratings, areaId)
#   2. /products/all/area     -> GeoJSON zone polygons (geometry, id = areaId)
#   Resort lat/lon coordinates are spatially matched to zone polygons (point-in-
#   polygon via the sf package), giving each resort an areaId that links to its
#   forecast. Human-readable zone names come from the hardcoded lookup table.
#
# EXPORTED FUNCTIONS
#   get_avalanche_data_for_resorts(resort_ids)
#     Primary entry point. Returns one row per resort with danger ratings.
#
#   clear_caic_cache()
#     Clears the session cache. Call between pipeline runs for fresh data.
#
# OUTPUT SCHEMA
#   resort_id             <int>
#   resort_name           <chr>
#   caic_zone             <chr>    human-readable zone name (from lookup)
#   forecast_date         <date>
#   danger_overall        <chr>    Low/Moderate/Considerable/High/Extreme/No Rating
#   danger_numeric        <int>    1-5 (NA for No Rating)
#   danger_alpine         <chr>
#   danger_treeline       <chr>
#   danger_below_treeline <chr>
#   problem_types         <chr>    comma-separated avalanche problem names
#
# API
#   Base    : https://avalanche.state.co.us/api-proxy/avid
#   Auth    : none — free, no API key required
#   Calls   : 2 total regardless of resort count (one forecast fetch,
#             one polygon fetch), plus session caching for both
#
# USAGE
#   source("fetch_caic.R")
#   avid <- get_avalanche_data_for_resorts(c(1, 5, 8, 31))
# =============================================================================

library(httr2)
library(dplyr)
library(purrr)
library(sf)          # point-in-polygon spatial matching

# ── Constants ─────────────────────────────────────────────────────────────────

.AVID_BASE <- "https://avalanche.state.co.us/api-proxy/avid?_api_proxy_uri="

.CAIC_OUTPUT_COLS <- c(
  "resort_id", "resort_name", "caic_zone", "forecast_date",
  "danger_overall", "danger_numeric",
  "danger_alpine", "danger_treeline", "danger_below_treeline",
  "danger_tomorrow_overall", "danger_tomorrow_numeric",
  "danger_tomorrow_alpine", "danger_tomorrow_treeline", "danger_tomorrow_below_treeline",
  "problem_types"
)

# Text rating -> numeric score for the scoring pipeline
danger_levels <- c(
  "Low"          = 1L,
  "Moderate"     = 2L,
  "Considerable" = 3L,
  "High"         = 4L,
  "Extreme"      = 5L
)

# ── Resort lookup table ───────────────────────────────────────────────────────
# lat/lon used for spatial matching to CAIC zone polygons.
# caic_zone is the human-readable name shown in the UI.

caic_zone_lookup <- tribble(
  ~resort_id, ~resort_name,                    ~caic_zone,              ~lat,      ~lon,
  1,  "Arapahoe Basin",                        "Vail & Summit County",  39.641667, -105.871667,
  2,  "Aspen Highlands",                       "Aspen",                 39.181111, -106.856389,
  3,  "Aspen Mountain",                        "Aspen",                 39.186389, -106.818611,
  4,  "Beaver Creek Resort",                   "Vail & Summit County",  39.631389, -106.521667,
  5,  "Breckenridge Ski Resort",               "Vail & Summit County",  39.480000, -106.067000,
  6,  "Buttermilk Ski Area",                   "Aspen",                 39.205000, -106.860556,
  7,  "Chapman Hill Ski Area",                 "North San Juan",        37.282939, -107.868480,
  8,  "Copper Mountain",                       "Vail & Summit County",  39.501667, -106.156389,
  10, "Crested Butte Mountain Resort",         "Gunnison",              38.899722, -106.965000,
  11, "Echo Mountain",                         "Front Range",           39.685000, -105.519444,
  12, "Eldora Mountain Resort",                "Front Range",           39.937500, -105.583611,
  13, "Granby Ranch",                          "Vail & Summit County",  40.080980, -105.889020,
  14, "Hesperus Ski Area",                     "North San Juan",        37.297778, -108.055000,
  15, "Hoedown Hill",                          "Front Range",           40.452064, -104.931336,
  16, "Howelsen Hill Ski Area",                "Steamboat & Flat Tops", 40.483000, -106.839000,
  17, "Kendall Mountain",                      "North San Juan",        37.792990, -107.627760,
  18, "Keystone Resort",                       "Vail & Summit County",  39.581889, -105.943712,
  19, "Lake City Ski Hill",                    "Gunnison",              38.010971, -107.311343,
  20, "Lee's Ski Hill",                        "Gunnison",              38.019584, -107.669179,
  21, "Loveland Ski Area",                     "Front Range",           39.687144, -105.883137,
  22, "Monarch Mountain",                      "Sawatch Range",         38.512139, -106.331436,
  23, "Powderhorn Resort",                     "Grand Mesa",            39.069516, -108.150153,
  24, "Purgatory Resort",                      "North San Juan",        37.629101, -107.835070,
  25, "Silverton Mountain",                    "South San Juan",        38.397891, -107.663000,
  26, "Ski Cooper",                            "Sawatch Range",         39.360353, -106.300791,
  27, "Snowmass",                              "Aspen",                 39.193962, -106.933573,
  28, "Steamboat Ski Resort",                  "Steamboat & Flat Tops", 40.453994, -106.770398,
  29, "Sunlight Ski Area",                     "Aspen",                 39.399948, -107.338336,
  30, "Telluride Ski Resort",                  "North San Juan",        37.919502, -107.832441,
  31, "Vail Ski Resort",                       "Vail & Summit County",  39.606334, -106.354972,
  32, "Winter Park Resort",                    "Front Range",           39.899029, -105.686036,
  33, "Wolf Creek Ski Area",                   "South San Juan",        37.472374, -106.792207
)

# ── Session cache ─────────────────────────────────────────────────────────────
.caic_cache <- new.env(parent = emptyenv())

clear_caic_cache <- function() {
  rm(list = ls(.caic_cache), envir = .caic_cache)
  message("CAIC cache cleared.")
}

# ── Internal: raw API fetchers ────────────────────────────────────────────────

.fetch_raw_forecasts <- function() {
  if (exists("forecasts", envir = .caic_cache, inherits = FALSE)) {
    message("  [cache] CAIC forecasts")
    return(get("forecasts", envir = .caic_cache, inherits = FALSE))
  }
  url  <- paste0(.AVID_BASE, "%2Fproducts%2Fall")
  resp <- request(url) |> req_timeout(15) |> req_perform()
  data <- resp_body_json(resp, simplifyVector = FALSE)
  assign("forecasts", data, envir = .caic_cache)
  data
}

.fetch_zone_polygons <- function() {
  if (exists("polygons", envir = .caic_cache, inherits = FALSE)) {
    message("  [cache] CAIC zone polygons")
    return(get("polygons", envir = .caic_cache, inherits = FALSE))
  }
  url <- paste0(.AVID_BASE,
    "%2Fproducts%2Fall%2Farea%3FproductType%3Davalancheforecast%26includeExpired%3Dtrue")
  raw   <- request(url) |> req_timeout(15) |> req_perform() |> resp_body_string()
  polys <- tryCatch(
    sf::st_read(raw, quiet = TRUE),
    error = function(e) { warning("Could not parse zone polygons: ", e$message); NULL }
  )
  assign("polygons", polys, envir = .caic_cache)
  polys
}

# ── Internal: parse one forecast object ──────────────────────────────────────

.normalise_rating <- function(x) {
  if (is.null(x) || is.na(x)) return(NA_character_)
  switch(tolower(trimws(x)),
    "low"          = "Low",
    "moderate"     = "Moderate",
    "considerable" = "Considerable",
    "high"         = "High",
    "extreme"      = "Extreme",
    "norating"     = "No Rating",
    NA_character_
  )
}

.parse_forecast <- function(fc) {
  area_id <- fc[["areaId"]] %||% NA_character_

  raw_date   <- fc[["dangerRatings"]][["days"]][[1]][["date"]] %||%
                fc[["issueDateTime"]] %||% NA_character_
  fcast_date <- tryCatch(as.Date(substr(raw_date, 1, 10)), error = function(e) as.Date(NA))

  # Helper: parse one day's ratings and derive overall
  parse_day <- function(day) {
    a <- .normalise_rating(day[["alp"]])
    t <- .normalise_rating(day[["tln"]])
    b <- .normalise_rating(day[["btl"]])
    rated   <- c(a, t, b)[c(a, t, b) %in% names(danger_levels)]
    overall <- if (length(rated) > 0) {
      rated[which.max(danger_levels[rated])]
    } else if (!all(is.na(c(a, t, b)))) {
      "No Rating"
    } else {
      NA_character_
    }
    list(overall = overall, alp = a, tln = t, btl = b)
  }

  day1 <- parse_day(fc[["dangerRatings"]][["days"]][[1]] %||% list())
  day2 <- parse_day(fc[["dangerRatings"]][["days"]][[2]] %||% list())

  probs_day1    <- fc[["avalancheProblems"]][["days"]][[1]] %||% list()
  problem_types <- if (length(probs_day1) > 0) {
    paste(na.omit(map_chr(probs_day1, ~ .x[["name"]] %||% NA_character_)), collapse = ", ")
  } else {
    NA_character_
  }

  tibble(
    area_id                        = area_id,
    forecast_date                  = fcast_date,
    danger_overall                 = day1$overall,
    danger_alpine                  = day1$alp,
    danger_treeline                = day1$tln,
    danger_below_treeline          = day1$btl,
    danger_tomorrow_overall        = day2$overall,
    danger_tomorrow_alpine         = day2$alp,
    danger_tomorrow_treeline       = day2$tln,
    danger_tomorrow_below_treeline = day2$btl,
    problem_types                  = problem_types
  )
}

# ── get_avalanche_data_for_resorts() ─────────────────────────────────────────

get_avalanche_data_for_resorts <- function(resort_ids) {

  if (!is.numeric(resort_ids)) {
    stop("resort_ids must be a numeric vector (e.g. c(1, 5, 31)).")
  }
  resort_ids <- unique(resort_ids)

  unknown   <- setdiff(resort_ids, caic_zone_lookup$resort_id)
  if (length(unknown) > 0) {
    warning("resort_id(s) not found, skipping: ", paste(unknown, collapse = ", "))
  }
  known_ids <- intersect(resort_ids, caic_zone_lookup$resort_id)

  if (length(known_ids) == 0) {
    warning("No valid resort_ids — returning empty dataframe.")
    return(.empty_output())
  }

  resorts <- filter(caic_zone_lookup, resort_id %in% known_ids)

  message("Fetching CAIC forecasts and zone polygons...")

  raw_forecasts <- tryCatch(.fetch_raw_forecasts(),
                            error = function(e) { warning(e$message); NULL })
  zone_polys    <- tryCatch(.fetch_zone_polygons(),
                            error = function(e) { warning(e$message); NULL })

  if (is.null(raw_forecasts)) {
    warning("Could not fetch forecasts — returning NAs.")
    return(.na_output(resorts))
  }

  forecast_table <- map_dfr(raw_forecasts, .parse_forecast)

  # Spatial match: resort point -> zone polygon -> area_id
  if (!is.null(zone_polys) && nrow(zone_polys) > 0) {
    resorts_sf <- sf::st_as_sf(resorts, coords = c("lon", "lat"), crs = 4326)
    zone_polys <- sf::st_transform(zone_polys, crs = 4326)
    suppressWarnings(
      joined <- sf::st_join(resorts_sf, zone_polys["id"],
                            join = sf::st_within, left = TRUE)
    )
    resorts <- mutate(resorts, area_id = joined$id)
  } else {
    warning("Zone polygons unavailable — danger data will be NA.")
    resorts <- mutate(resorts, area_id = NA_character_)
  }

  result <- resorts |>
    left_join(forecast_table, by = "area_id") |>
    mutate(
      danger_numeric          = unname(danger_levels[danger_overall]),
      danger_tomorrow_numeric = unname(danger_levels[danger_tomorrow_overall])
    ) |>
    select(all_of(.CAIC_OUTPUT_COLS))

  message("Done. Returned ", nrow(result), " resort(s).")
  result
}

# ── Empty / NA output helpers ─────────────────────────────────────────────────

.empty_output <- function() {
  tibble(
    resort_id = integer(), resort_name = character(), caic_zone = character(),
    forecast_date = as.Date(character()), danger_overall = character(),
    danger_numeric = integer(), danger_alpine = character(),
    danger_treeline = character(), danger_below_treeline = character(),
    danger_tomorrow_overall = character(), danger_tomorrow_numeric = integer(),
    danger_tomorrow_alpine = character(), danger_tomorrow_treeline = character(),
    danger_tomorrow_below_treeline = character(),
    problem_types = character()
  )
}

.na_output <- function(resorts) {
  resorts |>
    mutate(
      forecast_date = as.Date(NA), danger_overall = NA_character_,
      danger_numeric = NA_integer_, danger_alpine = NA_character_,
      danger_treeline = NA_character_, danger_below_treeline = NA_character_,
      danger_tomorrow_overall = NA_character_, danger_tomorrow_numeric = NA_integer_,
      danger_tomorrow_alpine = NA_character_, danger_tomorrow_treeline = NA_character_,
      danger_tomorrow_below_treeline = NA_character_,
      problem_types = NA_character_
    ) |>
    select(all_of(.CAIC_OUTPUT_COLS))
}

# ── Test block ────────────────────────────────────────────────────────────────
if (FALSE) {
  source("fetch_caic.R")

  result <- get_avalanche_data_for_resorts(c(1, 5, 31))
  print(result)

  result2 <- get_avalanche_data_for_resorts(c(28, 10, 30))
  print(result2)

  result_all <- get_avalanche_data_for_resorts(caic_zone_lookup$resort_id)
  print(result_all)

  clear_caic_cache()
}
