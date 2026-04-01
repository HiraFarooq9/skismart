# =============================================================================
# stage2_rerank.R
# Drive Time Penalty + Resort Reranking
#
# Takes the top 3 resorts from Stage 1 (ranked by conditions score),
# fetches a driving route for each, applies a drive time penalty multiplier,
# and produces a final reranked list.
#
# Drive time penalty logic:
#   within user max              → multiplier 1.0  (no penalty)
#   up to 20% over user max      → multiplier 0.75 (soft penalty)
#   more than 20% over user max  → multiplier 0.40 (heavy penalty)
#
# If ALL resorts exceed user max (even with 20% buffer), penalty multipliers
# are dropped entirely and resorts rank by conditions score alone. The
# drive_time_flag signals this to the LLM so it can communicate the tradeoff.
#
# Dependencies: dplyr, purrr, api_routing.R
# =============================================================================

library(dplyr)
library(purrr)


# -----------------------------------------------------------------------------
# CONSTANTS
# -----------------------------------------------------------------------------

DRIVE_PENALTY_WITHIN    <- 1.00   # at or under user max
DRIVE_PENALTY_SOFT      <- 0.75   # up to 20% over
DRIVE_PENALTY_HEAVY     <- 0.40   # more than 20% over
DRIVE_SOFT_THRESHOLD    <- 1.20   # 20% buffer


# -----------------------------------------------------------------------------
# MAIN FUNCTION
# -----------------------------------------------------------------------------

#' Fetch drive times for top 3 resorts and rerank with penalty
#'
#' @param top3          Data frame with columns:
#'                        resort_name (chr), lat (dbl), lon (dbl),
#'                        conditions_score (dbl, 0–1)
#'                      Rows should be ordered by Stage 1 rank (best first).
#' @param start_lat     Numeric. User start latitude.
#' @param start_lon     Numeric. User start longitude.
#' @param user_max_mins Numeric. User's maximum acceptable drive time in minutes.
#' @param gh_api_key    Character. GraphHopper API key.
#'
#' @return Named list:
#'   $ranking        — data frame of all 3 resorts with drive time + final score,
#'                     sorted by final_score descending
#'   $winner         — single-row data frame: the top-ranked resort
#'   $drive_time_flag — character: "within_preference", "slightly_over", or
#'                      "all_exceed_preference"
rerank_with_drive_time <- function(top3, start_lat, start_lon,
                                   user_max_mins, gh_api_key) {

  # Step 1: fetch drive time for each resort
  results <- map_dfr(seq_len(nrow(top3)), function(i) {
    resort <- top3[i, ]
    route  <- tryCatch(
      fetch_route(start_lat, start_lon, resort$lat, resort$lon, gh_api_key),
      error = function(e) NULL
    )

    duration_mins  <- if (!is.null(route)) round(route$duration_mins, 1) else NA_real_
    distance_miles <- if (!is.null(route)) round(route$distance_miles, 1) else NA_real_

    tibble(
      resort_name       = resort$resort_name,
      lat               = resort$lat,
      lon               = resort$lon,
      conditions_score  = resort$conditions_score,
      duration_mins     = duration_mins,
      distance_miles    = distance_miles
    )
  })

  # Step 2: determine if any resort is within preference (including 20% buffer)
  results <- results |>
    mutate(
      within_preference = !is.na(duration_mins) &
                          duration_mins <= user_max_mins * DRIVE_SOFT_THRESHOLD
    )

  any_within <- any(results$within_preference, na.rm = TRUE)

  # Step 3: apply penalty multipliers (or drop them if all exceed)
  results <- results |>
    mutate(
      drive_multiplier = if (any_within) {
        case_when(
          is.na(duration_mins)                          ~ DRIVE_PENALTY_HEAVY,
          duration_mins <= user_max_mins                ~ DRIVE_PENALTY_WITHIN,
          duration_mins <= user_max_mins * DRIVE_SOFT_THRESHOLD ~ DRIVE_PENALTY_SOFT,
          TRUE                                          ~ DRIVE_PENALTY_HEAVY
        )
      } else {
        1.0  # all exceed — rank purely by conditions
      },
      final_score = round(conditions_score * drive_multiplier, 4)
    ) |>
    arrange(desc(final_score))

  # Step 4: drive time flag for LLM context
  drive_time_flag <- if (!any_within) {
    "all_exceed_preference"
  } else if (results$duration_mins[1] <= user_max_mins) {
    "within_preference"
  } else {
    "slightly_over"
  }

  list(
    ranking        = results,
    winner         = results[1, ],
    drive_time_flag = drive_time_flag
  )
}


# -----------------------------------------------------------------------------
# FORMATTER (for LLM context and UI)
# -----------------------------------------------------------------------------

#' Format rerank results into a readable summary string
#'
#' @param rerank_result List. Output of rerank_with_drive_time().
#' @param user_max_mins Numeric. User's max drive time.
#'
#' @return Character. Multi-line summary for passing to LLM or printing.
format_rerank_summary <- function(rerank_result, user_max_mins) {
  r <- rerank_result$ranking

  header <- sprintf(
    "User's max drive time: %d minutes\nDrive time status: %s\n\nResort Rankings:\n",
    as.integer(user_max_mins), rerank_result$drive_time_flag
  )

  rows <- pmap_chr(r, function(resort_name, conditions_score, duration_mins,
                                distance_miles, drive_multiplier, final_score, ...) {
    drive_str <- if (is.na(duration_mins)) {
      "drive time unavailable"
    } else {
      sprintf("%.0f min / %.0f miles", duration_mins, distance_miles)
    }
    sprintf("  %s | conditions: %.2f | drive: %s | penalty: %.2f | final: %.4f",
            resort_name, conditions_score, drive_str, drive_multiplier, final_score)
  })

  paste0(header, paste(rows, collapse = "\n"))
}
