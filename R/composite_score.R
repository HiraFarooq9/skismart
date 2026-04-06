# =============================================================================
# composite_score.R
# Stage 1 — Master Resort Scoring Function
#
# PURPOSE
#   Combines weather quality, terrain accessibility, and avalanche danger into
#   a single composite score (0–1) for every resort, then returns a ranked
#   dataframe ready for the Shiny UI.
#
# COMPOSITE SCORE LOGIC
#   Step 1 — base_score = (weather_score * 0.5) + (terrain_score * 0.5)
#     weather_score  : 0–1, from compute_weather_score()
#     terrain_score  : 0–1, normalized from compute_terrain_open_score()
#                      raw scores are ability-weighted open trail counts;
#                      normalize_terrain_scores() scales them to 0–1 across
#                      all resorts before entering the composite.
#                      NOTE: ability matching is done inside terrain functions;
#                      do NOT apply it again here.
#
#   Step 2 — avalanche multiplier (today and tomorrow only)
#     If ski_date is today or tomorrow AND danger_numeric is not NA:
#       Low (1)          → ×1.00
#       Moderate (2)     → ×0.90
#       Considerable (3) → ×0.75
#       High (4)         → ×0.50
#       Extreme (5)      → composite_score = NA, resort flagged unsafe
#     If ski_date is beyond tomorrow OR avalanche data unavailable:
#       Skip multiplier (avalanche_applied = FALSE)
#
#   Step 3 — round composite_score to 3 decimal places.
#
# TWO-PASS STRUCTURE
#   Pass 1 — fetch weather + compute raw terrain scores for all resorts
#   Pass 2 — normalize terrain scores, compute composite, apply avalanche
#   Normalization requires the full distribution, so composite cannot be
#   computed resort-by-resort in a single loop.
#
# MAIN FUNCTION
#   score_all_resorts(resorts, ski_date, ability)
#     resorts   — dataframe from data/resorts.csv
#     ski_date  — "YYYY-MM-DD", the user's first ski day
#     ability   — "beginner" | "intermediate" | "advanced" | "expert"
#   Returns a dataframe sorted by composite_score descending (NAs last).
#
# DEPENDENCIES (sourced below — working directory must be project root)
#   R/api_openmeteo.R      fetch_weather_all_resorts()
#   R/weather_score.R      compute_weather_score()
#   R/terrain_open_score.R compute_terrain_open_score(), normalize_terrain_scores()
#   R/fetch_caic.R         get_avalanche_data_for_resorts()
# =============================================================================

source("R/api_openmeteo.R")
source("R/weather_score.R")
source("R/terrain_open_score.R")
source("R/fetch_caic.R")

library(dplyr)
library(lubridate)


# =============================================================================
# INTERNAL: season open/closed filter
# =============================================================================
# Season dates in the CSV are stored as "MM-DD" (e.g. "11-27", "04-19").
# Ski seasons span two calendar years (open in fall, close in spring), so
# the correct open/close dates depend on which half of the season ski_date
# falls in.
#
# Returns a logical vector — TRUE means the resort is open on ski_date.

.resort_is_open <- function(season_open, season_close, ski_date) {

  ski_date <- as.Date(ski_date)
  ski_year <- as.integer(format(ski_date, "%Y"))

  mapply(function(s_open, s_close) {

    # Handle missing or malformed values — assume open if unknown
    if (is.na(s_open) | is.na(s_close) | nchar(s_open) < 4 | nchar(s_close) < 4) {
      return(TRUE)
    }

    open_month  <- as.integer(substr(s_open,  1, 2))
    close_month <- as.integer(substr(s_close, 1, 2))

    # Cross-year season: opens in fall (high month), closes in spring (low month)
    if (open_month > close_month) {
      if (as.integer(format(ski_date, "%m")) <= close_month) {
        # ski_date is in the closing half (Jan–Jun): season opened previous year
        open_date  <- as.Date(paste0(ski_year - 1, "-", s_open))
        close_date <- as.Date(paste0(ski_year,     "-", s_close))
      } else {
        # ski_date is in the opening half (Jul–Dec): season closes next year
        open_date  <- as.Date(paste0(ski_year,     "-", s_open))
        close_date <- as.Date(paste0(ski_year + 1, "-", s_close))
      }
    } else {
      # Same-year season (unusual but handle gracefully)
      open_date  <- as.Date(paste0(ski_year, "-", s_open))
      close_date <- as.Date(paste0(ski_year, "-", s_close))
    }

    ski_date >= open_date & ski_date <= close_date

  }, season_open, season_close, SIMPLIFY = TRUE, USE.NAMES = FALSE)
}


# =============================================================================
# INTERNAL: avalanche multiplier lookup
# =============================================================================

.avalanche_multiplier <- c(
  `1` = 1.00,
  `2` = 0.90,
  `3` = 0.75,
  `4` = 0.50,
  `5` = NA_real_   # Extreme — resort flagged unsafe
)

.avalanche_warning_text <- c(
  `2` = "Moderate avalanche danger — exercise caution in exposed terrain.",
  `3` = "Considerable avalanche danger — natural and human-triggered avalanches likely.",
  `4` = "High avalanche danger — travel in avalanche terrain not recommended.",
  `5` = "Extreme avalanche danger — resort not recommended."
)


# =============================================================================
# MAIN: score_all_resorts()
# =============================================================================

score_all_resorts <- function(resorts, ski_date, ability) {

  ski_date <- as.Date(ski_date)
  today    <- Sys.Date()

  use_avalanche <- ski_date == today | ski_date == today + 1

  # ---------------------------------------------------------------------------
  # Pre-filter — remove resorts closed on ski_date
  # ---------------------------------------------------------------------------
  open_mask <- .resort_is_open(resorts$season_open, resorts$season_close, ski_date)
  closed_resorts <- resorts[!open_mask, "resort_name", drop = FALSE]
  resorts <- resorts[open_mask, ]

  if (nrow(closed_resorts) > 0) {
    message("Excluding ", nrow(closed_resorts), " closed resort(s): ",
            paste(closed_resorts$resort_name, collapse = ", "))
  }

  if (nrow(resorts) == 0) {
    message("No resorts are open on ", ski_date, ".")
    return(data.frame())
  }

  # ---------------------------------------------------------------------------
  # Step 1 — Fetch weather for all resorts
  # ---------------------------------------------------------------------------
  message("Fetching weather data from Open-Meteo...")
  all_weather <- fetch_weather_all_resorts(resorts, ski_date = ski_date)

  # ---------------------------------------------------------------------------
  # Step 2 — Fetch avalanche data (today/tomorrow only; soft failure)
  # ---------------------------------------------------------------------------
  avalanche_df <- NULL

  if (use_avalanche) {
    message("Fetching avalanche data from CAIC...")
    avalanche_df <- tryCatch(
      get_avalanche_data_for_resorts(resorts$resort_id),
      error = function(e) {
        message("CAIC fetch failed — avalanche data will not be applied: ", e$message)
        NULL
      }
    )
  } else {
    message("Ski date is beyond tomorrow — avalanche data not applicable.")
  }

  # ---------------------------------------------------------------------------
  # Pass 1 — Weather scores + raw terrain scores for all resorts
  # ---------------------------------------------------------------------------
  intermediate <- vector("list", nrow(resorts))

  for (i in seq_len(nrow(resorts))) {

    resort    <- resorts[i, ]
    resort_id <- as.character(resort$resort_id)
    wx        <- all_weather[[resort_id]]

    row <- list(
      resort_id          = resort$resort_id,
      resort_name        = resort$resort_name,
      weather_score      = NA_real_,
      raw_terrain        = NA_real_,
      lift_pct           = NA_real_,
      pct_preferred_open = NA_real_,
      weather_warnings   = NULL,
      terrain_warnings   = NULL,
      error              = NA_character_
    )

    if (!is.null(wx$error)) {
      row$error <- wx$error
      intermediate[[i]] <- row
      next
    }

    # Weather score (0–1)
    wx_result <- compute_weather_score(
      depth_in         = wx$depth_in,
      snowfall_72hr_in = wx$snowfall_72hr_in,
      temp_f           = wx$temp_f,
      wind_mph         = wx$wind_mph,
      precip_mm_hr     = wx$precip_mm_hr,
      snowfall_mm_hr   = wx$snowfall_mm_hr,
      ability          = ability
    )
    row$weather_score    <- wx_result$score
    row$weather_warnings <- wx_result$warnings

    # Trail mix from resort CSV (proportions, must sum to ~1)
    trail_mix <- c(
      greens     = resort$trails_green_pct,
      blues      = resort$trails_blue_pct,
      blacks     = resort$trails_black_pct,
      dbl_blacks = resort$trails_double_black_pct
    )

    # Raw terrain score (ability-weighted open trail count — not yet 0–1)
    terrain_result <- compute_terrain_open_score(
      depth_in         = wx$depth_in,
      snowfall_72hr_in = wx$snowfall_72hr_in,
      wind_mph         = wx$wind_mph,
      ability          = ability,
      trails_total     = resort$trails_total,
      trail_mix        = trail_mix
    )
    row$raw_terrain        <- terrain_result$raw_score
    row$lift_pct           <- terrain_result$lift_pct
    row$pct_preferred_open <- terrain_result$pct_preferred_open
    row$terrain_warnings   <- terrain_result$warnings

    intermediate[[i]] <- row
  }

  # ---------------------------------------------------------------------------
  # Normalize terrain scores across all resorts (requires full distribution)
  # ---------------------------------------------------------------------------
  raw_terrain_vec <- sapply(intermediate, function(r) r$raw_terrain)
  names(raw_terrain_vec) <- sapply(intermediate, function(r) as.character(r$resort_id))

  valid_raw <- raw_terrain_vec[!is.na(raw_terrain_vec)]
  normalized_terrain <- if (length(valid_raw) > 0) {
    normalize_terrain_scores(valid_raw)
  } else {
    numeric(0)
  }

  # ---------------------------------------------------------------------------
  # Pass 2 — Composite scores + avalanche multiplier
  # ---------------------------------------------------------------------------
  results <- vector("list", nrow(resorts))

  for (i in seq_len(nrow(resorts))) {

    r <- intermediate[[i]]

    terrain_score_norm <- normalized_terrain[as.character(r$resort_id)]
    if (length(terrain_score_norm) == 0) terrain_score_norm <- NA_real_

    # Gate: if 0% of ability-preferred terrain is accessible, score is 0
    terrain_score_final <- unname(terrain_score_norm)
    if (!is.na(r$pct_preferred_open) && r$pct_preferred_open == 0) {
      terrain_score_final <- 0
    }

    row <- list(
      resort_id         = r$resort_id,
      resort_name       = r$resort_name,
      composite_score   = NA_real_,
      weather_score     = r$weather_score,
      terrain_score     = terrain_score_final,
      avalanche_danger  = NA_character_,
      avalanche_applied = FALSE,
      lift_pct          = r$lift_pct,
      warnings          = NA_character_,
      error             = r$error
    )

    # Skip composite calculation if weather fetch failed
    if (!is.na(r$error)) {
      results[[i]] <- row
      next
    }

    base_score   <- (r$weather_score * 0.5) + (terrain_score_final * 0.5)
    all_warnings <- c(r$weather_warnings, r$terrain_warnings)

    # Avalanche multiplier (today/tomorrow only)
    if (use_avalanche && !is.null(avalanche_df)) {

      avy_row <- avalanche_df[avalanche_df$resort_id == r$resort_id, ]

      if (nrow(avy_row) > 0 && !is.na(avy_row$danger_numeric[1])) {

        danger_num              <- avy_row$danger_numeric[1]
        row$avalanche_danger    <- avy_row$danger_overall[1]
        row$avalanche_applied   <- TRUE

        if (danger_num == 5L) {
          base_score   <- NA_real_
          all_warnings <- c(all_warnings, .avalanche_warning_text["5"])
        } else {
          multiplier   <- .avalanche_multiplier[as.character(danger_num)]
          base_score   <- base_score * multiplier
          avy_warn     <- .avalanche_warning_text[as.character(danger_num)]
          if (!is.na(avy_warn)) all_warnings <- c(all_warnings, avy_warn)
        }
      }
    }

    row$composite_score <- if (!is.na(base_score)) round(base_score, 3) else NA_real_

    all_warnings <- all_warnings[!is.null(all_warnings) & !is.na(all_warnings) & nchar(all_warnings) > 0]
    row$warnings <- if (length(all_warnings) > 0) paste(all_warnings, collapse = "; ") else NA_character_

    results[[i]] <- row
  }

  # ---------------------------------------------------------------------------
  # Assemble and sort output dataframe
  # ---------------------------------------------------------------------------
  output <- bind_rows(lapply(results, as.data.frame, stringsAsFactors = FALSE))

  output <- output[, c(
    "resort_id", "resort_name", "composite_score", "weather_score",
    "terrain_score", "avalanche_danger", "avalanche_applied",
    "lift_pct", "warnings", "error"
  )]

  output <- output[order(output$composite_score, decreasing = TRUE, na.last = TRUE), ]
  rownames(output) <- NULL

  message("Scoring complete. ",
          sum(!is.na(output$composite_score)), " resort(s) scored, ",
          sum(is.na(output$composite_score) & is.na(output$error)), " flagged unsafe, ",
          sum(!is.na(output$error)), " failed to fetch.")

  output
}


# =============================================================================
# TEST BLOCK — not executed on source()
# =============================================================================
if (FALSE) {
  resorts  <- read.csv("data/resorts.csv")
  ski_date <- Sys.Date() + 1
  ability  <- "intermediate"

  results <- score_all_resorts(resorts, ski_date, ability)
  print(results[, c("resort_name", "composite_score", "weather_score",
                    "terrain_score", "avalanche_danger", "avalanche_applied")])

  # Verify avalanche is NOT applied for future date
  results_future <- score_all_resorts(resorts, ski_date = Sys.Date() + 5, ability = "expert")
  print(results_future[, c("resort_name", "composite_score", "avalanche_applied")])
}
