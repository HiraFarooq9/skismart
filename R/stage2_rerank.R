# =============================================================================
# stage2_rerank.R
# Drive Time Hard Exclusion + Resort Reranking
#
# Takes the top resorts from Stage 1 (ranked by conditions score),
# fetches a driving route for each, hard-excludes any resort that exceeds
# the user's max drive time, and ranks the remaining by conditions score.
#
# If ALL resorts exceed the user's max drive time, winner is set to NULL
# and drive_time_flag is "no_resorts_within_limit" — the UI shows a
# "try increasing your drive time" message instead of a recommendation.
#
# Dependencies: dplyr, purrr, api_routing.R
# =============================================================================

library(dplyr)
library(purrr)


# -----------------------------------------------------------------------------
# MAIN FUNCTION
# -----------------------------------------------------------------------------

#' Fetch drive times for top resorts and hard-exclude those over the limit
#'
#' @param top_resorts   Data frame with columns:
#'                        resort_name (chr), lat (dbl), lon (dbl),
#'                        conditions_score (dbl, 0–1)
#'                      Rows should be ordered by Stage 1 rank (best first).
#' @param start_lat     Numeric. User start latitude.
#' @param start_lon     Numeric. User start longitude.
#' @param user_max_mins Numeric. User's maximum acceptable drive time in minutes.
#' @param gh_api_key    Character. GraphHopper API key.
#'
#' @return Named list:
#'   $ranking        — data frame of all resorts with drive time, sorted by
#'                     conditions_score descending (within-limit resorts first)
#'   $winner         — single-row data frame: top conditions-score resort within
#'                     the drive limit, or NULL if none qualify
#'   $drive_time_flag — character: "within_preference" or "no_resorts_within_limit"
#'   $routes         — named list of route objects, keyed by resort_name
rerank_with_drive_time <- function(top_resorts, start_lat, start_lon,
                                   user_max_mins, gh_api_key) {

  # Step 1: fetch routes for each resort
  route_list <- lapply(seq_len(nrow(top_resorts)), function(i) {
    resort <- top_resorts[i, ]
    tryCatch(
      fetch_route(start_lat, start_lon, resort$lat, resort$lon, gh_api_key),
      error = function(e) NULL
    )
  })
  names(route_list) <- top_resorts$resort_name

  results <- map_dfr(seq_along(route_list), function(i) {
    resort <- top_resorts[i, ]
    route  <- route_list[[i]]

    duration_mins  <- if (!is.null(route)) round(route$duration_mins, 1) else NA_real_
    distance_miles <- if (!is.null(route)) round(route$distance_miles, 1) else NA_real_

    tibble(
      resort_name      = resort$resort_name,
      lat              = resort$lat,
      lon              = resort$lon,
      conditions_score = resort$conditions_score,
      duration_mins    = duration_mins,
      distance_miles   = distance_miles
    )
  })

  # Step 2: hard exclude resorts that exceed the user's max drive time
  within_limit <- results |>
    filter(!is.na(duration_mins) & duration_mins <= user_max_mins)

  if (nrow(within_limit) == 0) {
    return(list(
      ranking         = results |> arrange(duration_mins),
      winner          = NULL,
      drive_time_flag = "no_resorts_within_limit",
      routes          = route_list
    ))
  }

  # Step 3: rank by conditions score — no penalty multipliers needed
  ranking <- within_limit |>
    arrange(desc(conditions_score))

  list(
    ranking         = ranking,
    winner          = ranking[1, ],
    drive_time_flag = "within_preference",
    routes          = route_list
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
                                distance_miles, ...) {
    drive_str <- if (is.na(duration_mins)) {
      "drive time unavailable"
    } else {
      sprintf("%.0f min / %.0f miles", duration_mins, distance_miles)
    }
    sprintf("  %s | conditions: %.2f | drive: %s",
            resort_name, conditions_score, drive_str)
  })

  paste0(header, paste(rows, collapse = "\n"))
}
