# =============================================================================
# score_resorts.R
# =============================================================================
#
# PURPOSE
#   Master Stage 1 scoring script for SkiSmart.
#   Loads static resort data, fetches live avalanche conditions, and computes
#   a weighted resort score. Returns a ranked top-N dataframe ready for the
#   Shiny UI and Stage 2 (route risk scoring).
#
# PIPELINE POSITION
#   User Input -> [THIS FILE: Stage 1 Resort Scoring] -> Stage 2 Route Risk
#
# DATA SOURCES INTEGRATED HERE
#   [x] Static resort database    resorts - clean.csv
#   [x] CAIC avalanche forecast   fetch_caic.R
#   [ ] Open-Meteo weather        fetch_weather.R         (placeholder)
#   [ ] User inputs               passed as function args (placeholder)
#   [ ] Terrain match             scored inline            (placeholder)
#
# MAIN FUNCTION
#   score_resorts(user_inputs)  -> ranked tibble of top N resorts
#
# USAGE
#   source("score_resorts.R")
#   top_resorts <- score_resorts(user_inputs)
# =============================================================================

library(readr)
library(dplyr)
library(lubridate)

source("fetch_caic.R")       # provides get_avalanche_data_for_resorts()
# source("fetch_weather.R")  # TODO: uncomment when fetch_weather.R is built
#                            #       will provide get_weather_data_for_resorts()

# =============================================================================
# SECTION 1 — STATIC RESORT DATABASE
# =============================================================================

resorts_raw <- read_csv(
  "resorts - clean.csv",
  col_types = cols(
    resort_id              = col_integer(),
    resort_name            = col_character(),
    latitude               = col_double(),
    longitude              = col_double(),
    elevation_base_ft      = col_number(),
    elevation_summit_ft    = col_number(),
    vertical_drop_ft       = col_number(),
    trails_total           = col_integer(),
    trails_green_pct       = col_double(),
    trails_blue_pct        = col_double(),
    trails_black_pct       = col_double(),
    trails_double_black_pct = col_double(),
    lifts_total            = col_integer(),
    total_acres            = col_number(),
    snowmaking_acres       = col_number(),
    season_open            = col_character(),
    season_close           = col_character(),
    resort_url             = col_character()
  )
)

# =============================================================================
# SECTION 2 — USER INPUT PLACEHOLDER
# =============================================================================
# TODO: Replace this hardcoded default with the actual Shiny reactive input
#       object (e.g. input$ability_level, input$max_drive_hours, etc.)
#
# Expected fields:
#   ability_level     chr  "beginner" | "intermediate" | "advanced" | "expert"
#   max_drive_hours   num  maximum acceptable drive time in hours
#   terrain_pref      chr  "groomed" | "mixed" | "challenging"
#   trip_date         Date date of planned ski day
#   pass_type         chr  "none" | "ikon" | "epic"
#   risk_tolerance    num  0-1 (0 = low risk, 1 = high risk tolerance)

.default_user_inputs <- list(
  ability_level   = "intermediate",
  max_drive_hours = 4,
  terrain_pref    = "mixed",
  trip_date       = Sys.Date() + 1,
  pass_type       = "none",
  risk_tolerance  = 0.5
)

# =============================================================================
# SECTION 3 — LIVE DATA FETCHING
# =============================================================================

.fetch_live_data <- function(resort_ids) {

  # ── 3a. Avalanche data (CAIC) ──────────────────────────────────────────────
  message("--- Fetching avalanche data ---")
  avalanche_data <- tryCatch(
    get_avalanche_data_for_resorts(resort_ids),
    error = function(e) {
      warning("Avalanche fetch failed: ", e$message)
      NULL
    }
  )

  # ── 3b. Weather forecast placeholder (Open-Meteo) ─────────────────────────
  # TODO: Replace with get_weather_data_for_resorts(resort_ids, trip_date)
  #       from fetch_weather.R once that module is built.
  #
  # Expected output schema (one row per resort_id):
  #   resort_id            <int>
  #   snow_depth_cm        <dbl>   current base depth
  #   snowfall_72h_cm      <dbl>   snowfall in past 72 hours
  #   temp_max_c           <dbl>   forecast high on trip_date
  #   temp_min_c           <dbl>   forecast low on trip_date
  #   precip_forecast_mm   <dbl>   forecast precipitation on trip_date
  #   wind_speed_kmh       <dbl>   forecast wind speed
  #   weather_code         <int>   WMO weather code for trip_date
  message("--- Weather data: placeholder (returning NAs) ---")
  weather_data <- tibble(
    resort_id          = resort_ids,
    snow_depth_cm      = NA_real_,
    snowfall_72h_cm    = NA_real_,
    temp_max_c         = NA_real_,
    temp_min_c         = NA_real_,
    precip_forecast_mm = NA_real_,
    wind_speed_kmh     = NA_real_,
    weather_code       = NA_integer_
  )

  list(avalanche = avalanche_data, weather = weather_data)
}

# =============================================================================
# SECTION 4 — SCORING FUNCTIONS
# =============================================================================

# ── 4a. Snow score (0-100) ────────────────────────────────────────────────────
# Based on base depth and recent snowfall.
# TODO: Incorporate Open-Meteo data (snow_depth_cm, snowfall_72h_cm) once
#       weather fetch is live. Currently returns NA.
#
# Inputs:  snow_depth_cm, snowfall_72h_cm
# Returns: numeric 0-100

.score_snow <- function(snow_depth_cm, snowfall_72h_cm) {
  # PLACEHOLDER — replace with real formula once weather data is available
  # Example formula when live:
  #   depth_score    <- pmin(snow_depth_cm / 150, 1) * 60   # max at 150cm depth
  #   freshness_score <- pmin(snowfall_72h_cm / 30, 1) * 40  # max at 30cm fresh
  #   depth_score + freshness_score
  NA_real_
}

# ── 4b. Terrain match score (0-100) ──────────────────────────────────────────
# Compares resort trail difficulty distribution against user ability.
# TODO: Refine weighting once user input module is finalised.
#
# Inputs:  trails_green_pct, trails_blue_pct, trails_black_pct,
#          trails_double_black_pct, user ability_level
# Returns: numeric 0-100

.score_terrain <- function(green_pct, blue_pct, black_pct, dbl_black_pct,
                           ability_level) {
  # Target difficulty distributions per ability level
  targets <- list(
    beginner     = c(green = 0.5, blue = 0.4, black = 0.1, dbl = 0.0),
    intermediate = c(green = 0.1, blue = 0.5, black = 0.3, dbl = 0.1),
    advanced     = c(green = 0.0, blue = 0.2, black = 0.5, dbl = 0.3),
    expert       = c(green = 0.0, blue = 0.1, black = 0.3, dbl = 0.6)
  )

  target <- targets[[ability_level]] %||% targets[["intermediate"]]

  # Score = 100 minus mean absolute deviation from target distribution
  actual <- c(green = green_pct, blue = blue_pct,
              black = black_pct, dbl = dbl_black_pct)
  actual[is.na(actual)] <- 0

  deviation <- mean(abs(actual - target))
  pmax(0, 100 - deviation * 200)   # scale: 0 deviation = 100, 0.5 deviation = 0
}

# ── 4c. Conditions risk score (0-100, lower = safer) ─────────────────────────
# Based on avalanche danger. Feeds into the composite score as a penalty.
# 100 = Extreme danger (hard block candidate), 0 = No rating / no concern.
#
# Inputs:  danger_numeric (1-5), risk_tolerance (0-1)
# Returns: numeric 0-100

.score_conditions_risk <- function(danger_numeric, risk_tolerance) {
  if (is.na(danger_numeric)) return(0)
  # Base risk: scale danger (1-5) to 0-100
  base_risk <- (danger_numeric - 1) / 4 * 100
  # Adjusted by user risk tolerance: high tolerance dampens the penalty
  base_risk * (1 - risk_tolerance * 0.4)
}

# =============================================================================
# SECTION 5 — MAIN SCORING FUNCTION
# =============================================================================

score_resorts <- function(user_inputs = .default_user_inputs, top_n = 3) {

  # Merge user_inputs with defaults to fill any missing fields
  inputs <- modifyList(.default_user_inputs, user_inputs)

  # ── Step 1: Filter resorts in season ──────────────────────────────────────
  # TODO: Parse season_open / season_close from the CSV and filter properly.
  #       Currently keeping all resorts.
  active_resorts <- resorts_raw

  # ── Step 2: Fetch live data ────────────────────────────────────────────────
  live <- .fetch_live_data(active_resorts$resort_id)

  # ── Step 3: Join all data sources ─────────────────────────────────────────
  resorts_full <- active_resorts

  if (!is.null(live$avalanche)) {
    resorts_full <- resorts_full |>
      left_join(
        live$avalanche |>
          select(resort_id, caic_zone, forecast_date,
                 danger_overall, danger_numeric,
                 danger_tomorrow_overall, danger_tomorrow_numeric,
                 problem_types),
        by = "resort_id"
      )
  } else {
    resorts_full <- resorts_full |>
      mutate(
        caic_zone              = NA_character_,
        forecast_date          = as.Date(NA),
        danger_overall         = NA_character_,
        danger_numeric         = NA_integer_,
        danger_tomorrow_overall = NA_character_,
        danger_tomorrow_numeric = NA_integer_,
        problem_types          = NA_character_
      )
  }

  resorts_full <- resorts_full |>
    left_join(live$weather, by = "resort_id")

  # ── Step 4: Compute sub-scores ─────────────────────────────────────────────
  resorts_scored <- resorts_full |>
    mutate(

      # Snow quality (0-100) — NA until weather module is live
      snow_score = .score_snow(snow_depth_cm, snowfall_72h_cm),

      # Terrain match (0-100)
      terrain_score = .score_terrain(
        trails_green_pct, trails_blue_pct,
        trails_black_pct, trails_double_black_pct,
        inputs$ability_level
      ),

      # Conditions risk penalty (0-100, higher = riskier)
      risk_score = .score_conditions_risk(danger_numeric, inputs$risk_tolerance),

      # ── COMPOSITE SCORE (0-100) ───────────────────────────────────────────
      # Weights reflect Stage 1 proposal logic.
      # TODO: Adjust weights once snow_score is live (currently 0 weight
      #       to avoid NA contamination).
      #
      # Final weights (proposed once all data is live):
      #   snow_score     * 0.40
      #   terrain_score  * 0.35
      #   risk_score     * -0.25  (penalty)
      composite_score = case_when(
        is.na(snow_score) ~
          # Temporary: terrain + avalanche only while weather is placeholder
          terrain_score * 0.70 + (100 - risk_score) * 0.30,
        TRUE ~
          # Full formula once weather is live
          snow_score * 0.40 + terrain_score * 0.35 + (100 - risk_score) * 0.25
      )
    )

  # ── Step 5: Hard blocks ────────────────────────────────────────────────────
  # Resorts with Extreme or High avalanche danger are flagged.
  # TODO: Add road closure hard blocks once CDOT 511 module is integrated.
  resorts_scored <- resorts_scored |>
    mutate(
      hard_block = danger_numeric >= 4 & !is.na(danger_numeric),
      block_reason = case_when(
        danger_numeric == 5 ~ "Extreme avalanche danger",
        danger_numeric == 4 ~ "High avalanche danger",
        TRUE                ~ NA_character_
      )
    )

  # ── Step 6: Rank and return top N ─────────────────────────────────────────
  top_resorts <- resorts_scored |>
    filter(!hard_block) |>
    arrange(desc(composite_score)) |>
    slice_head(n = top_n) |>
    select(
      resort_id, resort_name, latitude, longitude,
      composite_score, terrain_score, snow_score, risk_score,
      danger_overall, danger_numeric,
      danger_tomorrow_overall, danger_tomorrow_numeric,
      problem_types, caic_zone, forecast_date,
      hard_block, block_reason
    )

  # Also return blocked resorts so the UI can display rejection reasons
  blocked_resorts <- resorts_scored |>
    filter(hard_block) |>
    select(resort_id, resort_name, block_reason, danger_overall)

  list(
    top       = top_resorts,
    blocked   = blocked_resorts,
    all       = resorts_scored,
    inputs    = inputs,
    timestamp = Sys.time()
  )
}

# =============================================================================
# SECTION 6 — TEST BLOCK
# =============================================================================
if (FALSE) {
  source("score_resorts.R")

  # Run with default inputs
  result <- score_resorts()
  print(result$top)
  print(result$blocked)

  # Run with custom user inputs
  result2 <- score_resorts(user_inputs = list(
    ability_level   = "expert",
    max_drive_hours = 3,
    terrain_pref    = "challenging",
    risk_tolerance  = 0.8
  ))
  print(result2$top)
}
