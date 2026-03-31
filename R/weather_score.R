# =============================================================================
# weather_score.R
# Stage 1 — Weather Score Component
#
# Computes a composite weather score (0–1) for a ski resort given current and
# forecast conditions. Score reflects skiing desirability, not just raw
# meteorological severity.
#
# Inputs expected in US customary units (inches, °F, mph) after unit conversion
# from Open-Meteo (which returns metric). See fetch_weather_openmeteo() in
# api_openmeteo.R for the conversion layer.
#
# Coordinate note: weather is queried at the resort's primary coordinate
# (base lodge lat/lon). Future iterations could improve accuracy by querying
# at mid-mountain elevation.
#
# Composite formula:
#   weather_score = (w_depth  * depth_score    +
#                    w_snow   * snowfall_score  +
#                    w_temp   * temp_score      +
#                    w_wind   * wind_score) * precip_multiplier
#
# The precipitation_multiplier is applied last as a penalty that can drag the
# entire score down regardless of otherwise good conditions.
# =============================================================================


# -----------------------------------------------------------------------------
# 1. SNOW DEPTH SCORE
# -----------------------------------------------------------------------------
# Inputs:
#   depth_in — current snow base depth in inches
#
# Logic (no hard exclude — resort open/closed is determined by season dates
# in the CSV, not snow depth):
#   0"–24"   → 0.0–0.1  (piecewise linear, very thin — limited groomed runs)
#   24"–30"  → 0.1–0.2  (piecewise linear, warning issued)
#   30"–36"  → 0.2–0.5  (groomed runs only)
#   36"–48"  → 0.5–0.8  (solid intermediate terrain)
#   48"–72"  → 0.8–0.9  (advanced terrain opens up)
#   72"+     → 0.9–1.0  (excellent, capped at 1.0 at ~100")
#
# Design note: the previous hard exclude (< 24" → 0.0) was removed because
# a 0" reading at base-lodge coordinates in late season does not mean the
# resort is closed — upper mountain terrain may still be fully open.
# Resort operating status is checked separately via season_open/season_close.
# The rescue clause was also removed: fresh snowfall is now captured
# independently by score_snowfall(), making it redundant.
#
# Returns: list(score = numeric, warning = character or NULL)
# -----------------------------------------------------------------------------

score_snow_depth <- function(depth_in) {

  warning_msg <- NULL

  # Piecewise linear interpolation across defined breakpoints
  breakpoints <- c(0, 24, 30, 36, 48, 72, 100)
  scores      <- c(0.0, 0.1, 0.2, 0.5, 0.8, 0.9, 1.0)

  if (depth_in >= 100) {
    score <- 1.0
  } else {
    depth_in <- max(depth_in, 0)  # floor at 0
    idx  <- findInterval(depth_in, breakpoints)
    lo_x <- breakpoints[idx];  hi_x <- breakpoints[idx + 1]
    lo_s <- scores[idx];       hi_s <- scores[idx + 1]
    score <- lo_s + (depth_in - lo_x) / (hi_x - lo_x) * (hi_s - lo_s)
  }

  # Warnings by severity
  if (depth_in < 24) {
    warning_msg <- paste0(
      "Very thin base (", round(depth_in), "\") — limited groomed terrain only. ",
      "Many runs likely closed."
    )
  } else if (depth_in < 30) {
    warning_msg <- paste0(
      "Shallow base (", round(depth_in), "\") — groomed runs only. ",
      "Off-piste not recommended."
    )
  }

  list(score = round(score, 3), warning = warning_msg)
}


# -----------------------------------------------------------------------------
# 2. SNOWFALL SCORE (72-hour accumulation)
# -----------------------------------------------------------------------------
# Inputs:
#   snowfall_in — total snowfall accumulation over the past 72 hours, inches
#
# Logic (step function with piecewise interpolation between breakpoints):
#   0"        → 0.0   (no recent snow)
#   1"–3"     → 0.1   (trace amounts)
#   3"–6"     → 0.3   (light dusting)
#   6"–12"    → 0.6   (solid session)
#   12"–18"   → 0.9   (powder day territory)
#   18"+      → 1.0   (exceptional)
#
# Returns: numeric score 0–1
# -----------------------------------------------------------------------------

score_snowfall <- function(snowfall_in) {

  breakpoints <- c(0, 1, 3, 6, 12, 18)
  scores      <- c(0.0, 0.1, 0.3, 0.6, 0.9, 1.0)

  if (snowfall_in <= 0)  return(0.0)
  if (snowfall_in >= 18) return(1.0)

  idx  <- findInterval(snowfall_in, breakpoints)
  lo_x <- breakpoints[idx];  hi_x <- breakpoints[idx + 1]
  lo_s <- scores[idx];       hi_s <- scores[idx + 1]

  score <- lo_s + (snowfall_in - lo_x) / (hi_x - lo_x) * (hi_s - lo_s)
  round(score, 3)
}


# -----------------------------------------------------------------------------
# 3. PRECIPITATION MULTIPLIER
# -----------------------------------------------------------------------------
# Applied as a multiplicative penalty to the entire composite score.
# Rain damages snowpack and makes conditions dangerous — it should be able to
# tank an otherwise good score regardless of base depth or fresh snow.
#
# Inputs:
#   temp_f           — current temperature in °F
#   precip_mm_hr     — total precipitation rate in mm/hr (rain + snow combined)
#   snowfall_mm_hr   — snowfall component in mm/hr
#                      (used to distinguish heavy snow from mixed precip)
#
# Precipitation type classification:
#   temp < 30°F  AND precip > 0             → Snow      → 1.0 (no penalty)
#   temp 30–34°F AND precip > 0:
#     snowfall_mm_hr > 5                    → Snow      → 1.0 (override)
#     else                                  → Mixed     → 0.7
#   temp > 34°F  AND precip > 0             → Rain:
#     < 2.5 mm/hr                           → Light     → 0.6
#     2.5–7.5 mm/hr                         → Moderate  → 0.4
#     > 7.5 mm/hr                           → Heavy     → 0.2
#   No precipitation                        →           → 1.0
#
# Returns: list(multiplier = numeric, type = character, warning = character or NULL)
# -----------------------------------------------------------------------------

get_precip_multiplier <- function(temp_f, precip_mm_hr, snowfall_mm_hr = 0) {

  # No precipitation
  if (precip_mm_hr <= 0) {
    return(list(multiplier = 1.0, type = "none", warning = NULL))
  }

  # Snow: cold enough that all precip falls as snow
  if (temp_f < 30) {
    return(list(multiplier = 1.0, type = "snow", warning = NULL))
  }

  # Borderline temp range (30–34°F)
  if (temp_f >= 30 && temp_f <= 34) {
    if (snowfall_mm_hr > 5) {
      # High snowfall rate overrides — treat as snow despite marginal temp
      return(list(multiplier = 1.0, type = "snow", warning = NULL))
    } else {
      return(list(
        multiplier = 0.7,
        type       = "mixed",
        warning    = "Mixed precipitation — icy conditions possible."
      ))
    }
  }

  # Above freezing: rain
  if (precip_mm_hr < 2.5) {
    return(list(
      multiplier = 0.6,
      type       = "light_rain",
      warning    = "Light rain — wet snow and slushy conditions."
    ))
  } else if (precip_mm_hr <= 7.5) {
    return(list(
      multiplier = 0.4,
      type       = "moderate_rain",
      warning    = "Moderate rain — poor conditions, snowpack damage likely."
    ))
  } else {
    return(list(
      multiplier = 0.2,
      type       = "heavy_rain",
      warning    = "Heavy rain — severe conditions, snowpack damage certain."
    ))
  }
}


# -----------------------------------------------------------------------------
# 4. TEMPERATURE SCORE
# -----------------------------------------------------------------------------
# Inputs:
#   temp_f — temperature at mid-mountain in °F
#
# Logic:
#   < 0°F      → 0.5   (good snow quality but real safety/comfort risk)
#   0–15°F     → 0.8   (cold, quality snow)
#   15–30°F    → 1.0   (sweet spot — dry powder, comfortable)
#   30–32°F    → 0.7   (snow getting heavy and wet)
#   32–35°F    → 0.3   (at/above freezing — steep cliff in quality)
#   35°F+      → 0.1   (poor conditions, spring slush)
#
# Note: piecewise linear interpolation within each band; values at band
# boundaries are exact per spec.
#
# Returns: list(score = numeric, warning = character or NULL)
# -----------------------------------------------------------------------------

score_temperature <- function(temp_f) {

  warning_msg <- NULL

  if (temp_f < 0) {
    warning_msg <- paste0(
      "Extreme cold (", round(temp_f), "\u00b0F) — dress accordingly, ",
      "check lift operating status."
    )
    return(list(score = 0.5, warning = warning_msg))
  }

  # Step function: each band has a single flat value
  score <- dplyr::case_when(
    temp_f <  15 ~ 0.8,
    temp_f <  30 ~ 1.0,
    temp_f <  32 ~ 0.7,
    temp_f <  35 ~ 0.3,
    TRUE         ~ 0.1
  )

  list(score = score, warning = warning_msg)
}


# -----------------------------------------------------------------------------
# 5. WIND SCORE
# -----------------------------------------------------------------------------
# Inputs:
#   wind_mph — wind speed at mid-mountain in mph
#
# Logic:
#   < 15 mph   → 1.0   (ideal)
#   15–25 mph  → 0.7   (noticeable, manageable)
#   25–35 mph  → 0.4   (deteriorating)
#   35–45 mph  → 0.2   (lift closures likely)
#   45+ mph    → 0.05  (widespread closures, dangerous)
#
# Returns: list(score = numeric, warning = character or NULL)
# -----------------------------------------------------------------------------

score_wind <- function(wind_mph) {

  warning_msg <- NULL

  if (wind_mph >= 45) {
    warning_msg <- paste0(
      "Extreme wind (", round(wind_mph), " mph) — widespread lift closures ",
      "likely. Check resort status before departing."
    )
    return(list(score = 0.05, warning = warning_msg))
  }

  # Step function: each band has a single flat value
  score <- dplyr::case_when(
    wind_mph <  15 ~ 1.0,
    wind_mph <  25 ~ 0.7,
    wind_mph <  35 ~ 0.4,
    wind_mph <  45 ~ 0.2,
    TRUE           ~ 0.05
  )

  if (wind_mph >= 35) {
    warning_msg <- paste0(
      "High winds (", round(wind_mph), " mph) — some lift closures likely."
    )
  }

  list(score = round(score, 3), warning = warning_msg)
}


# -----------------------------------------------------------------------------
# 6. ABILITY-BASED WEIGHTS
# -----------------------------------------------------------------------------
# Weights reflect what each ability group cares about most:
#   Beginners    — base depth matters most (need groomed runs),
#                  snowfall less critical, comfort (temp/wind) more important
#   Intermediate — more balanced; fresh snow starts to matter more
#   Advanced     — fresh snow is king; base matters less (can ski thin terrain)
#   Expert       — maximum weight on fresh snow; depth least important
#
# Returns: named numeric vector summing to 1.0
# -----------------------------------------------------------------------------

get_weather_weights <- function(ability = c("beginner", "intermediate", "advanced", "expert")) {

  ability <- match.arg(ability)

  weights <- list(
    beginner     = c(depth = 0.45, snowfall = 0.15, temp = 0.20, wind = 0.20),
    intermediate = c(depth = 0.40, snowfall = 0.25, temp = 0.15, wind = 0.20),
    advanced     = c(depth = 0.35, snowfall = 0.35, temp = 0.15, wind = 0.15),
    expert       = c(depth = 0.30, snowfall = 0.40, temp = 0.15, wind = 0.15)
  )

  weights[[ability]]
}


# -----------------------------------------------------------------------------
# 7. COMPOSITE WEATHER SCORE
# -----------------------------------------------------------------------------
# Orchestrates all sub-scores into a single 0–1 weather score.
#
# Inputs:
#   depth_in         — snow base depth, inches
#   snowfall_72hr_in — 72-hour snowfall accumulation, inches
#   temp_f           — temperature at mid-mountain, °F
#   wind_mph         — wind speed at mid-mountain, mph
#   precip_mm_hr     — total precipitation rate, mm/hr
#   snowfall_mm_hr   — snowfall component of precipitation, mm/hr
#                      (used only for precip type classification)
#   ability          — single skier ability level: "beginner", "intermediate",
#                      "advanced", or "expert" (one input per session)
#
# Output:
#   list with:
#     $score          — composite weather score, 0–1
#     $components     — named vector of individual sub-scores
#     $weights        — weights used
#     $precip         — precipitation type and multiplier applied
#     $warnings       — character vector of all active warning messages
#
# What the score means:
#   0.0–0.2   Poor          — very thin base or severe conditions
#   0.2–0.4   Below average — skiable but significant concerns
#   0.4–0.6   Average       — decent conditions, some limitations
#   0.6–0.8   Good          — solid day on the mountain
#   0.8–1.0   Excellent     — ideal conditions
# -----------------------------------------------------------------------------

compute_weather_score <- function(depth_in,
                                  snowfall_72hr_in,
                                  temp_f,
                                  wind_mph,
                                  precip_mm_hr,
                                  snowfall_mm_hr = 0,
                                  ability        = "intermediate") {

  # Compute individual component scores
  depth_result  <- score_snow_depth(depth_in)
  snowfall_sc   <- score_snowfall(snowfall_72hr_in)
  temp_result   <- score_temperature(temp_f)
  wind_result   <- score_wind(wind_mph)
  precip_result <- get_precip_multiplier(temp_f, precip_mm_hr, snowfall_mm_hr)

  # Extract scores
  depth_sc <- depth_result$score
  temp_sc  <- temp_result$score
  wind_sc  <- wind_result$score

  # Get ability-appropriate weights
  w <- get_weather_weights(ability)

  # Weighted sum
  raw_score <- (w["depth"]    * depth_sc   +
                w["snowfall"] * snowfall_sc +
                w["temp"]     * temp_sc     +
                w["wind"]     * wind_sc)

  # Apply precipitation multiplier
  final_score <- raw_score * precip_result$multiplier

  # Collect all warnings
  warnings <- c(
    depth_result$warning,
    temp_result$warning,
    wind_result$warning,
    precip_result$warning
  )
  warnings <- warnings[!is.null(warnings) & !is.na(warnings) & nchar(warnings) > 0]

  list(
    score      = round(final_score, 3),
    components = c(
      depth_score    = depth_sc,
      snowfall_score = snowfall_sc,
      temp_score     = temp_sc,
      wind_score     = wind_sc
    ),
    weights    = w,
    precip     = list(
      type       = precip_result$type,
      multiplier = precip_result$multiplier
    ),
    warnings   = if (length(warnings) > 0) warnings else NULL
  )
}
