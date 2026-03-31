# =============================================================================
# terrain_open_score.R
# Stage 1 — Terrain Open Score Component
#
# Estimates what fraction of a skier's preferred terrain is accessible given
# current snow depth, recent snowfall, and wind conditions.
#
# Distinction from weather_score.R:
#   Weather score  → quality of conditions (how good will the skiing feel)
#   Terrain score  → accessibility of your specific terrain type (how much
#                    of the mountain can you actually get on)
#
# Pipeline:
#   1. Snow depth  → base % open per trail tier (greens/blues/blacks/dbl blacks)
#   2. Snowfall    → bump each tier up (capped at tier maximum)
#   3. Weights     → weight tiers by skier ability
#   4. trail_score = weighted sum of adjusted open percentages
#   5. Lift pct    → derived from wind speed
#   6. accessibility_score = trail_score * lift_pct
#
# All percentages are stored and returned on a 0–1 scale.
# =============================================================================


# -----------------------------------------------------------------------------
# 1. BASE TERRAIN OPEN PERCENTAGES BY SNOW DEPTH
# -----------------------------------------------------------------------------
# Inputs:
#   depth_in — current snow base depth in inches
#
# Breakpoints and values (converted to 0–1):
#   Depth       Greens  Blues  Blacks  Dbl Blacks
#   0"            0%     0%      0%       0%
#   24"           60%    20%     0%       0%
#   30"           80%    50%     10%      0%
#   36"           95%    75%     40%      10%
#   48"          100%    90%     70%      40%
#   72"+         100%   100%     90%      70%
#
# Design note: the previous hard floor (< 24" → all zeros) was removed.
# Terrain now interpolates from 0% at 0" up to the 24" values, reflecting
# that some groomed terrain (mainly greens) may remain accessible on very
# thin bases — particularly at higher elevations where the base coordinate
# reading may understate actual snowpack. Resort operating status is
# checked separately via season_open/season_close dates.
#
# Piecewise linear interpolation within each band.
# Returns: named numeric vector (greens, blues, blacks, dbl_blacks), 0–1
# -----------------------------------------------------------------------------

get_terrain_pct_by_depth <- function(depth_in) {

  depth_in <- max(depth_in, 0)  # floor at 0

  # Breakpoints — now starting from 0"
  depths <- c(0, 24, 30, 36, 48, 72)

  # Values at each breakpoint, per tier (0–1 scale)
  greens_vals    <- c(0.00, 0.60, 0.80, 0.95, 1.00, 1.00)
  blues_vals     <- c(0.00, 0.20, 0.50, 0.75, 0.90, 1.00)
  blacks_vals    <- c(0.00, 0.00, 0.10, 0.40, 0.70, 0.90)
  dbl_black_vals <- c(0.00, 0.00, 0.00, 0.10, 0.40, 0.70)

  # Cap at 72"+ values (no further increase beyond top breakpoint)
  if (depth_in >= 72) {
    return(c(
      greens     = 1.00,
      blues      = 1.00,
      blacks     = 0.90,
      dbl_blacks = 0.70
    ))
  }

  # Interpolate within segment
  idx  <- findInterval(depth_in, depths)
  lo_x <- depths[idx];  hi_x <- depths[idx + 1]
  frac <- (depth_in - lo_x) / (hi_x - lo_x)

  c(
    greens     = greens_vals[idx]    + frac * (greens_vals[idx + 1]    - greens_vals[idx]),
    blues      = blues_vals[idx]     + frac * (blues_vals[idx + 1]     - blues_vals[idx]),
    blacks     = blacks_vals[idx]    + frac * (blacks_vals[idx + 1]    - blacks_vals[idx]),
    dbl_blacks = dbl_black_vals[idx] + frac * (dbl_black_vals[idx + 1] - dbl_black_vals[idx])
  )
}


# -----------------------------------------------------------------------------
# 2. SNOWFALL BUMP
# -----------------------------------------------------------------------------
# Fresh snow opens additional terrain that base depth alone wouldn't support.
# Applied as a flat additive bonus to each tier, capped at each tier's maximum
# possible open percentage (the 72"+ column values).
#
# Inputs:
#   terrain_pcts     — named vector from get_terrain_pct_by_depth(), 0–1
#   snowfall_72hr_in — 72-hour snowfall accumulation in inches
#
# Bump schedule (step function):
#   0"–3"    → no bump
#   3"–6"    → +0.05 per tier
#   6"–12"   → +0.10 per tier
#   12"+     → +0.15 per tier
#
# Tier caps (reflect that some terrain has operational limits regardless of snow):
#   Greens       → 1.00
#   Blues        → 1.00
#   Blacks       → 0.90
#   Double Blacks → 0.70
#
# Returns: named numeric vector (same structure as input), 0–1
# -----------------------------------------------------------------------------

apply_snowfall_bump <- function(terrain_pcts, snowfall_72hr_in) {

  bump <- dplyr::case_when(
    snowfall_72hr_in >= 12 ~ 0.15,
    snowfall_72hr_in >= 6  ~ 0.10,
    snowfall_72hr_in >= 3  ~ 0.05,
    TRUE                   ~ 0.00
  )

  if (bump == 0) return(terrain_pcts)

  caps <- c(greens = 1.00, blues = 1.00, blacks = 0.90, dbl_blacks = 0.70)

  pmin(terrain_pcts + bump, caps)
}


# -----------------------------------------------------------------------------
# 3. LIFT OPERATING PERCENTAGE
# -----------------------------------------------------------------------------
# Wind is the primary driver of lift closures. This returns the estimated
# fraction of lifts operating, used to scale overall mountain accessibility.
#
# Inputs:
#   wind_mph — wind speed at mid-mountain in mph
#
# Schedule:
#   < 15 mph   → 1.00  (no impact)
#   15–25 mph  → 0.90  (minor impact, exposed lifts may slow)
#   25–35 mph  → 0.70  (some high-speed quads closing)
#   35–45 mph  → 0.40  (significant closures, mainly base lifts open)
#   45+ mph    → 0.15  (near shutdown, only sheltered lifts operating)
#
# Piecewise linear interpolation within bands.
#
# Returns: list(pct = numeric 0–1, warning = character or NULL)
# -----------------------------------------------------------------------------

get_lift_pct <- function(wind_mph) {

  warning_msg <- NULL

  if (wind_mph >= 45) {
    warning_msg <- paste0(
      "Extreme wind (", round(wind_mph), " mph) — near mountain shutdown. ",
      "Only sheltered base lifts likely operating."
    )
    return(list(pct = 0.15, warning = warning_msg))
  }

  # Step function: each band has a single flat value
  pct <- dplyr::case_when(
    wind_mph <  15 ~ 1.00,
    wind_mph <  25 ~ 0.90,
    wind_mph <  35 ~ 0.70,
    wind_mph <  45 ~ 0.40,
    TRUE           ~ 0.15
  )

  if (wind_mph >= 35) {
    warning_msg <- paste0(
      "High winds (", round(wind_mph), " mph) — significant lift closures expected."
    )
  } else if (wind_mph >= 25) {
    warning_msg <- paste0(
      "Moderate winds (", round(wind_mph), " mph) — some high-speed lifts may close."
    )
  }

  list(pct = round(pct, 3), warning = warning_msg)
}


# -----------------------------------------------------------------------------
# 4. ABILITY-BASED TERRAIN WEIGHTS
# -----------------------------------------------------------------------------
# Weights reflect the terrain mix each ability group actually skis.
# A beginner cares almost entirely about greens being open; an expert
# weights double blacks most heavily.
#
#   Tier          Beginner  Intermediate  Advanced  Expert
#   Greens          0.70       0.30         0.10     0.10
#   Blues           0.30       0.60         0.30     0.10
#   Blacks          0.00       0.10         0.50     0.30
#   Double Blacks   0.00       0.00         0.10     0.50
#
# Returns: named numeric vector summing to 1.0
# -----------------------------------------------------------------------------

get_terrain_weights <- function(ability = c("beginner", "intermediate", "advanced", "expert")) {

  ability <- match.arg(ability)

  weights <- list(
    beginner     = c(greens = 0.70, blues = 0.30, blacks = 0.00, dbl_blacks = 0.00),
    intermediate = c(greens = 0.30, blues = 0.60, blacks = 0.10, dbl_blacks = 0.00),
    advanced     = c(greens = 0.10, blues = 0.30, blacks = 0.50, dbl_blacks = 0.10),
    expert       = c(greens = 0.10, blues = 0.10, blacks = 0.30, dbl_blacks = 0.50)
  )

  weights[[ability]]
}


# -----------------------------------------------------------------------------
# 5. COMPOSITE TERRAIN OPEN SCORE
# -----------------------------------------------------------------------------
# Combines depth-based terrain availability, snowfall bump, and wind-driven
# lift restrictions into a single 0–1 accessibility score.
#
# Inputs:
#   depth_in         — snow base depth in inches
#   snowfall_72hr_in — 72-hour snowfall accumulation in inches
#   wind_mph         — wind speed in mph
#   ability          — "beginner", "intermediate", "advanced", or "expert"
#
# Formula:
#   trail_score         = sum(tier_open_pct * tier_weight)   for all tiers
#   accessibility_score = trail_score * lift_pct
#
# Output:
#   list with:
#     $score              — final accessibility score, 0–1
#     $trail_score        — weighted terrain open score before lift adjustment
#     $lift_pct           — estimated fraction of lifts operating
#     $terrain_pcts       — named vector of open % per tier after snowfall bump
#     $weights            — ability weights used
#     $warnings           — character vector of active warnings (or NULL)
#
# What the score means:
#   0.0–0.2   Very limited  — few lifts, minimal terrain accessible
#   0.2–0.4   Restricted    — limited terrain, mostly beginner runs
#   0.4–0.6   Partial       — solid base terrain, some intermediate open
#   0.6–0.8   Good access   — most preferred terrain available
#   0.8–1.0   Full access   — near-complete terrain for your ability level
# -----------------------------------------------------------------------------

compute_terrain_open_score <- function(depth_in,
                                       snowfall_72hr_in,
                                       wind_mph,
                                       ability = "intermediate") {

  # Step 1: base terrain percentages from snow depth
  terrain_pcts <- get_terrain_pct_by_depth(depth_in)

  # Step 2: apply snowfall bump
  terrain_pcts <- apply_snowfall_bump(terrain_pcts, snowfall_72hr_in)

  # Step 3: ability weights
  w <- get_terrain_weights(ability)

  # Step 4: weighted trail score
  trail_score <- sum(terrain_pcts * w)

  # Step 5: lift operating percentage from wind
  lift_result <- get_lift_pct(wind_mph)

  # Step 6: final accessibility score
  accessibility_score <- trail_score * lift_result$pct

  # Collect warnings
  warnings <- lift_result$warning
  warnings <- warnings[!is.null(warnings) & !is.na(warnings) & nchar(warnings) > 0]

  list(
    score        = round(accessibility_score, 3),
    trail_score  = round(trail_score, 3),
    lift_pct     = lift_result$pct,
    terrain_pcts = round(terrain_pcts, 3),
    weights      = w,
    warnings     = if (length(warnings) > 0) warnings else NULL
  )
}
