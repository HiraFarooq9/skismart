# =============================================================================
# route_conditions.R
# Spatial Filter + LLM Prompt Builder for Route Conditions
#
# Takes the route geometry from api_routing.R and the CDOT data frame from
# api_cdot.R, spatially filters to segments that fall along the route
# corridor, then formats the result into a structured Gemini prompt.
#
# Spatial approach:
#   Each CDOT segment has start (primary) and end (secondary) coordinates.
#   We filter to segments where either endpoint falls within a lat/lon buffer
#   around the route bounding box. Conservative by design — better to include
#   a nearby road than miss a real closure.
#
#   Buffer: CORRIDOR_BUFFER_DEG (~2.5 miles at Colorado latitudes ~40°N)
#
# Risk derivation:
#   Since all data comes through currentConditions free text, we keyword-match
#   on condition_type and additional_data. The LLM may refine this.
#
# Dependencies: dplyr, purrr
# =============================================================================

library(dplyr)
library(purrr)


# -----------------------------------------------------------------------------
# CONSTANTS
# -----------------------------------------------------------------------------

# ~0.036 degrees ≈ 2.5 miles at Colorado latitudes
CORRIDOR_BUFFER_DEG <- 0.036

# Keywords that signal elevated risk in condition text
CLOSURE_KEYWORDS     <- c("closed", "closure", "no travel")
CHAIN_LAW_KEYWORDS   <- c("chain law", "code 15", "code 16", "traction law",
                           "traction control law", "chains required")
BAD_SURFACE_KEYWORDS <- c("ice", "icy", "packed snow", "snow covered",
                          "blowing snow", "frozen mix", "glazed")


# -----------------------------------------------------------------------------
# SPATIAL FILTER
# -----------------------------------------------------------------------------

#' Filter CDOT condition rows to those along the route corridor
#'
#' @param route     List. Output of fetch_route() — must contain $bbox.
#' @param cdot_df   Data frame. Output of fetch_road_conditions().
#' @param buffer    Numeric. Corridor half-width in degrees.
#'
#' @return Filtered data frame (same columns as cdot_df).
filter_cdot_to_route <- function(route, cdot_df, buffer = CORRIDOR_BUFFER_DEG) {
  if (nrow(cdot_df) == 0) return(cdot_df)

  bbox <- route$bbox
  lon_min <- bbox["min_lon"] - buffer
  lon_max <- bbox["max_lon"] + buffer
  lat_min <- bbox["min_lat"] - buffer
  lat_max <- bbox["max_lat"] + buffer

  cdot_df |> filter(
    (start_lon >= lon_min & start_lon <= lon_max &
       start_lat >= lat_min & start_lat <= lat_max) |
      (end_lon >= lon_min & end_lon <= lon_max &
         end_lat >= lat_min & end_lat <= lat_max)
  )
}


# -----------------------------------------------------------------------------
# RISK LEVEL
# -----------------------------------------------------------------------------

#' Derive a preliminary route risk level from filtered CDOT rows
#'
#' Keyword-matches on condition_type and additional_data.
#' The LLM prompt instructs Gemini to confirm or override this.
#'
#' Risk levels (highest wins):
#'   do_not_travel — closure/no-travel keywords in text
#'   high          — chain law or traction law active
#'   moderate      — icy/packed snow/snow covered surface
#'   low           — no concerning keywords found
#'
#' @param filtered_df Data frame. Output of filter_cdot_to_route().
#' @return Character. One of: "low", "moderate", "high", "do_not_travel"
derive_risk_level <- function(filtered_df) {
  if (nrow(filtered_df) == 0) return("low")

  # Closure keywords checked against additional_data only — condition_type can
  # contain codes like "seasonal closure" that are not active road closures.
  desc_text <- tolower(replace(filtered_df$additional_data,
                               is.na(filtered_df$additional_data), ""))

  # Chain law and surface keywords checked against both fields
  full_text <- tolower(paste(
    filtered_df$condition_type,
    filtered_df$additional_data,
    sep = " "
  ))

  has_keyword <- function(keywords, text) {
    any(sapply(keywords, function(kw) any(grepl(kw, text, fixed = TRUE))))
  }

  if (has_keyword(CLOSURE_KEYWORDS,     desc_text)) return("do_not_travel")
  if (has_keyword(CHAIN_LAW_KEYWORDS,   full_text)) return("high")
  if (has_keyword(BAD_SURFACE_KEYWORDS, full_text)) return("moderate")
  "low"
}


# -----------------------------------------------------------------------------
# LLM PROMPT BUILDER
# -----------------------------------------------------------------------------

#' Build a Gemini prompt from route metadata + filtered CDOT rows
#'
#' Gemini should return exactly:
#'   RISK_LEVEL: [low | moderate | high | do_not_travel]
#'   SUMMARY: [2-3 sentences]
#'   KEY_ACTION: [single most important action, or "None"]
#'
#' @param route         List. Output of fetch_route().
#' @param filtered_df   Data frame. Output of filter_cdot_to_route().
#' @param resort_name   Character. Destination resort name.
#' @param ski_date      Date or character. First ski day.
#'
#' @return Character. Prompt string ready for Gemini.
build_route_llm_prompt <- function(route, filtered_df, resort_name, ski_date) {
  risk_level <- derive_risk_level(filtered_df)

  # Format condition rows for the prompt.
  # Use additional_data as the description when available; fall back to
  # condition_type for operator-reported rows where additional_data is NA
  # (e.g. "6 - snow", "reduced visibility") — these are real surface conditions
  # and must not be silently dropped.
  if (nrow(filtered_df) == 0) {
    conditions_block <- "  No active conditions reported along this route."
  } else {
    conditions_block <- filtered_df |>
      distinct(segment_id, condition_id, .keep_all = TRUE) |>
      pmap_chr(function(route, pass_name, condition_type, additional_data,
                        updated_at, ...) {
        label       <- if (!is.na(pass_name) && pass_name != "") pass_name else route
        description <- if (!is.na(additional_data)) additional_data else
                         paste0("[operator reported: ", condition_type, "]")
        sprintf("  [%s | %s | updated %s]\n  %s",
                label, condition_type, updated_at, description)
      }) |>
      paste(collapse = "\n\n")
  }

  sprintf(
'You are a road safety assistant helping a skier plan a drive to %s on %s.
Estimated drive: %.0f miles, %.0f minutes under normal conditions.
Preliminary risk assessment (keyword-based): %s

Below is current road condition data from CDOT for the route corridor.
Some entries may be forecasts (sourceType: NDFD) rather than live reports.
Focus on the most safety-relevant information.

CONDITIONS ALONG ROUTE:
%s

Respond with EXACTLY this structure and no other text:

RISK_LEVEL: [low | moderate | high | do_not_travel]
SUMMARY: [2-3 sentences on current driving conditions to %s. Name specific roads or passes if mentioned. Flag chain laws or closures explicitly.]
KEY_ACTION: [The single most important thing the driver should do, or "None" if conditions are clear.]',
    resort_name, as.character(ski_date),
    route$distance_miles, route$duration_mins,
    risk_level,
    conditions_block,
    resort_name
  )
}


# -----------------------------------------------------------------------------
# PER-ROW RISK CLASSIFICATION
# -----------------------------------------------------------------------------

#' Classify risk level for each row in a filtered CDOT data frame
#'
#' Adds a `row_risk` column so the UI can colour-code individual condition
#' markers on the map independently of the route-level risk summary.
#'
#' @param filtered_df Data frame. Output of filter_cdot_to_route().
#' @return Same data frame with an added character column: row_risk
#'   ("low", "moderate", "high", "do_not_travel")
classify_cdot_risks <- function(filtered_df) {
  if (nrow(filtered_df) == 0) {
    return(filtered_df |> mutate(row_risk = character(0)))
  }

  .classify_one <- function(ctype, adata) {
    text <- tolower(paste(ctype, if (is.na(adata)) "" else adata))
    if (any(sapply(CLOSURE_KEYWORDS,     grepl, x = text, fixed = TRUE))) return("do_not_travel")
    if (any(sapply(CHAIN_LAW_KEYWORDS,   grepl, x = text, fixed = TRUE))) return("high")
    if (any(sapply(BAD_SURFACE_KEYWORDS, grepl, x = text, fixed = TRUE))) return("moderate")
    "low"
  }

  filtered_df |>
    mutate(row_risk = mapply(.classify_one, condition_type, additional_data,
                             USE.NAMES = FALSE))
}


# -----------------------------------------------------------------------------
# ORCHESTRATOR
# -----------------------------------------------------------------------------

#' Full pipeline: filter CDOT to route + build Gemini prompt
#'
#' @param route       List. Output of fetch_route().
#' @param cdot_df     Data frame. Output of fetch_road_conditions().
#' @param resort_name Character. Destination resort name.
#' @param ski_date    Date or character. First ski day.
#'
#' @return Named list:
#'   $filtered_df  — CDOT rows along the route, with `row_risk` column added
#'   $risk_level   — route-level preliminary risk level (character)
#'   $llm_prompt   — ready-to-send prompt string
prepare_route_conditions <- function(route, cdot_df, resort_name, ski_date) {
  filtered   <- filter_cdot_to_route(route, cdot_df) |> classify_cdot_risks()
  risk       <- derive_risk_level(filtered)
  prompt     <- build_route_llm_prompt(route, filtered, resort_name, ski_date)

  list(
    filtered_df = filtered,
    risk_level  = risk,
    llm_prompt  = prompt
  )
}
