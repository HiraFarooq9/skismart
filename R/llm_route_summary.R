# =============================================================================
# llm_route_summary.R
# Groq LLM Call for Route Condition Interpretation
#
# Sends the prompt built by route_conditions.R to the Groq API and parses
# the structured response into a clean list for the UI.
#
# API details:
#   Provider : Groq (groq.com) — free tier, no billing required
#   Model    : llama-3.3-70b-versatile (fast, capable, free)
#   Base URL : https://api.groq.com/openai/v1/chat/completions
#   Auth     : Bearer token in Authorization header
#   Sign up  : https://console.groq.com
#
# Groq uses the OpenAI-compatible chat completions format.
#
# Expected response format (enforced by prompt in route_conditions.R):
#   RISK_LEVEL: [low | moderate | high | do_not_travel]
#   SUMMARY: [2-3 sentences]
#   KEY_ACTION: [single action or "None"]
#
# Dependencies: httr2
# =============================================================================

library(httr2)


# -----------------------------------------------------------------------------
# CONSTANTS
# -----------------------------------------------------------------------------

GROQ_MODEL    <- "llama-3.3-70b-versatile"
GROQ_BASE_URL <- "https://api.groq.com/openai/v1/chat/completions"


# -----------------------------------------------------------------------------
# MAIN FUNCTION
# -----------------------------------------------------------------------------

#' Send a route conditions prompt to Groq and return a parsed summary
#'
#' @param prompt    Character. Built by build_route_llm_prompt() in
#'                  route_conditions.R.
#' @param groq_key  Character. Groq API key.
#'
#' @return Named list:
#'   $risk_level  — character: "low", "moderate", "high", or "do_not_travel"
#'   $summary     — character: 2-3 sentence plain-language description
#'   $key_action  — character: top recommended action, or "None"
#'   $raw_text    — character: full raw response (for debugging)
#'   Returns a safe fallback list on any failure.
call_groq_route_summary <- function(prompt, groq_key) {

  body <- list(
    model    = GROQ_MODEL,
    messages = list(
      list(role = "user", content = prompt)
    ),
    temperature = 0.2,
    max_tokens  = 300
  )

  req <- request(GROQ_BASE_URL) |>
    req_headers(
      Authorization = paste("Bearer", groq_key),
      `Content-Type` = "application/json"
    ) |>
    req_body_json(body) |>
    req_timeout(30) |>
    req_error(is_error = function(resp) FALSE)

  resp <- req_perform(req)

  if (resp_status(resp) != 200) {
    warning(sprintf(
      "Groq API returned status %d. Body: %s",
      resp_status(resp),
      tryCatch(resp_body_string(resp), error = function(e) "<unreadable>")
    ))
    return(.fallback_summary())
  }

  raw <- tryCatch(
    resp_body_json(resp, simplifyVector = FALSE),
    error = function(e) {
      warning(paste("Failed to parse Groq response:", e$message))
      NULL
    }
  )

  if (is.null(raw)) return(.fallback_summary())

  raw_text <- tryCatch(
    raw$choices[[1]]$message$content,
    error = function(e) NULL
  )

  if (is.null(raw_text) || nchar(trimws(raw_text)) == 0) {
    warning("Groq returned an empty response.")
    return(.fallback_summary())
  }

  .parse_groq_response(raw_text)
}


# -----------------------------------------------------------------------------
# RESPONSE PARSER
# -----------------------------------------------------------------------------

.parse_groq_response <- function(text) {
  extract <- function(label) {
    pattern <- sprintf("(?i)^%s:\\s*(.+?)\\s*$", label)
    lines   <- strsplit(text, "\n")[[1]]
    match   <- regmatches(lines, regexpr(pattern, lines, perl = TRUE))
    if (length(match) == 0) return(NA_character_)
    sub(sprintf("(?i)^%s:\\s*", label), "", match[[1]], perl = TRUE)
  }

  risk_raw <- tolower(trimws(extract("RISK_LEVEL")))

  risk_level <- if (grepl("do_not|no.travel|do not", risk_raw)) {
    "do_not_travel"
  } else if (risk_raw %in% c("low", "moderate", "high")) {
    risk_raw
  } else {
    "moderate"
  }

  list(
    risk_level  = risk_level,
    summary     = trimws(extract("SUMMARY")),
    key_action  = trimws(extract("KEY_ACTION")),
    raw_text    = text
  )
}


.fallback_summary <- function() {
  list(
    risk_level  = "unknown",
    summary     = "Road condition data could not be retrieved. Check CDOT's CoTrip website or call 511 before driving.",
    key_action  = "Check cotrip.org or call 511 for current conditions.",
    raw_text    = NA_character_
  )
}


# =============================================================================
# SCORE INTERPRETATION
# =============================================================================

#' Ask Groq to interpret the weather, terrain, and avalanche scores in plain language
#'
#' @param resort_name         Character. Resort name.
#' @param weather_score       Numeric 0-1. From composite_score.R.
#' @param terrain_score       Numeric 0-1. Normalized relative score.
#' @param avalanche_danger    Character. CAIC rating, or NA if unavailable.
#' @param avalanche_applied   Logical. Whether avalanche data was applied.
#' @param lift_pct            Numeric 0-1. Estimated lift operating fraction.
#' @param ability_level       Character. "beginner", "intermediate", "advanced", "expert".
#' @param ski_date            Date or character. The ski day.
#' @param groq_key            Character. Groq API key.
#' @param depth_in            Numeric. Snow base depth in inches (from Open-Meteo).
#' @param snowfall_72hr_in    Numeric. 72-hour snowfall accumulation in inches.
#' @param temp_f              Numeric. Average ski-hour temperature in °F.
#' @param wind_mph            Numeric. Average ski-hour wind speed in mph.
#' @param pct_preferred_open  Numeric 0-1. Ability-weighted average % of preferred trail
#'                            tiers estimated open (e.g. 0.38 = 38%).
#' @param terrain_rank        Character. Rank context, e.g. "1st of 3 qualifying resorts".
#'
#' @return Named list: $weather, $terrain, $avalanche (character strings), or NULL on failure.
call_groq_score_interpretation <- function(resort_name, weather_score, terrain_score,
                                            avalanche_danger, avalanche_applied,
                                            lift_pct, ability_level, ski_date,
                                            groq_key,
                                            depth_in         = NA_real_,
                                            snowfall_72hr_in = NA_real_,
                                            temp_f           = NA_real_,
                                            wind_mph         = NA_real_,
                                            pct_preferred_open = NA_real_,
                                            terrain_rank     = NA_character_) {

  avy_str  <- if (isTRUE(avalanche_applied) && !is.na(avalanche_danger) && nchar(trimws(avalanche_danger)) > 0)
    avalanche_danger else "Not available (date outside 2-day forecast window)"

  lift_str <- if (is.na(lift_pct)) "Unknown" else paste0(round(lift_pct * 100), "%")

  # Build weather data block — use actual values when available, fall back to score only
  wx_lines <- c(
    if (!is.na(depth_in))         sprintf("  - Base depth: %d inches",           as.integer(depth_in)),
    if (!is.na(snowfall_72hr_in)) sprintf("  - Fresh snowfall (72h): %.1f inches", snowfall_72hr_in),
    if (!is.na(temp_f))           sprintf("  - Avg ski-hour temperature: %d°F",   as.integer(temp_f)),
    if (!is.na(wind_mph))         sprintf("  - Avg ski-hour wind: %d mph",         as.integer(wind_mph)),
    sprintf("  - Weather score: %.2f / 1.00", weather_score)
  )

  # Build terrain data block — use pct_preferred_open and rank when available
  terrain_pct_str  <- if (!is.na(pct_preferred_open))
    sprintf("  - Estimated %.0f%% of %s-preferred trail types (weighted by tier) accessible",
            pct_preferred_open * 100, ability_level) else NULL
  terrain_rank_str <- if (!is.na(terrain_rank))
    sprintf("  - Terrain rank: %s", terrain_rank) else NULL

  terrain_lines <- c(
    terrain_pct_str,
    terrain_rank_str,
    sprintf("  - Terrain score: %.2f / 1.00  (normalized across qualifying resorts)", terrain_score),
    sprintf("  - Estimated lifts operating: %s", lift_str)
  )

  prompt <- sprintf(
'You are a concise ski trip advisor. Write plain-language interpretations of the ski conditions below for %s on %s.

WEATHER DATA:
%s

TERRAIN DATA (%s skier):
%s

AVALANCHE: %s

Rules:
- One sentence per field, no filler phrases like "Great news" or "Unfortunately"
- For WEATHER: name the specific factors driving the score (depth, fresh snow, temp, wind)
- For TERRAIN: state the estimated %% of preferred terrain open and explain the score is relative to other resorts in this search
- For AVALANCHE: state the danger level and what it means practically; if unavailable say to check CAIC directly

Respond with EXACTLY this format:
WEATHER: [one sentence]
TERRAIN: [one sentence]
AVALANCHE: [one sentence]',
    resort_name, as.character(ski_date),
    paste(wx_lines, collapse = "\n"),
    ability_level,
    paste(terrain_lines, collapse = "\n"),
    avy_str
  )

  body <- list(
    model       = GROQ_MODEL,
    messages    = list(list(role = "user", content = prompt)),
    temperature = 0.3,
    max_tokens  = 220
  )

  req <- request(GROQ_BASE_URL) |>
    req_headers(
      Authorization  = paste("Bearer", groq_key),
      `Content-Type` = "application/json"
    ) |>
    req_body_json(body) |>
    req_timeout(30) |>
    req_error(is_error = function(resp) FALSE)

  resp <- req_perform(req)

  if (resp_status(resp) != 200) {
    warning(sprintf("Groq score interpretation returned status %d.", resp_status(resp)))
    return(NULL)
  }

  raw <- tryCatch(
    resp_body_json(resp, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(raw)) return(NULL)

  raw_text <- tryCatch(raw$choices[[1]]$message$content, error = function(e) NULL)
  if (is.null(raw_text) || nchar(trimws(raw_text)) == 0) return(NULL)

  .parse_score_interpretation(raw_text)
}


.parse_score_interpretation <- function(text) {
  extract <- function(label) {
    pattern <- sprintf("(?i)^%s:\\s*(.+?)\\s*$", label)
    lines   <- strsplit(text, "\n")[[1]]
    match   <- regmatches(lines, regexpr(pattern, lines, perl = TRUE))
    if (length(match) == 0) return(NA_character_)
    sub(sprintf("(?i)^%s:\\s*", label), "", match[[1]], perl = TRUE)
  }
  list(
    weather   = trimws(extract("WEATHER")),
    terrain   = trimws(extract("TERRAIN")),
    avalanche = trimws(extract("AVALANCHE"))
  )
}
