# =============================================================================
# llm_route_summary.R
# Gemini LLM Call for Route Condition Interpretation
#
# Sends the prompt built by route_conditions.R to the Gemini API and parses
# the structured response into a clean list for the UI.
#
# API details:
#   Model    : gemini-2.0-flash (fast, cheap, sufficient for this task)
#   Base URL : https://generativelanguage.googleapis.com/v1beta/models/
#   Auth     : API key as query param (?key=...)
#   Docs     : https://ai.google.dev/api/generate-content
#
# Expected Gemini response format (enforced by prompt in route_conditions.R):
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

GEMINI_BASE_URL <- "https://generativelanguage.googleapis.com/v1beta/models"
GEMINI_MODEL    <- "gemini-2.0-flash-lite"


# -----------------------------------------------------------------------------
# MAIN FUNCTION
# -----------------------------------------------------------------------------

#' Send a route conditions prompt to Gemini and return a parsed summary
#'
#' @param prompt      Character. Built by build_route_llm_prompt() in
#'                    route_conditions.R.
#' @param gemini_key  Character. Gemini API key.
#'
#' @return Named list:
#'   $risk_level  — character: "low", "moderate", "high", or "do_not_travel"
#'   $summary     — character: 2-3 sentence plain-language description
#'   $key_action  — character: top recommended action, or "None"
#'   $raw_text    — character: full raw Gemini response (for debugging)
#'   Returns a safe fallback list on any failure.
call_gemini_route_summary <- function(prompt, gemini_key) {
  url <- sprintf("%s/%s:generateContent", GEMINI_BASE_URL, GEMINI_MODEL)

  body <- list(
    contents = list(
      list(
        parts = list(
          list(text = prompt)
        )
      )
    ),
    generationConfig = list(
      temperature     = 0.2,   # low temperature — we want factual, consistent output
      maxOutputTokens = 300
    )
  )

  req <- request(url) |>
    req_url_query(key = gemini_key) |>
    req_body_json(body) |>
    req_headers(
      `Content-Type` = "application/json"
    ) |>
    req_timeout(30) |>
    req_error(is_error = function(resp) FALSE)

  resp <- req_perform(req)

  if (resp_status(resp) != 200) {
    warning(sprintf(
      "Gemini API returned status %d. Body: %s",
      resp_status(resp),
      tryCatch(resp_body_string(resp), error = function(e) "<unreadable>")
    ))
    return(.fallback_summary())
  }

  raw <- tryCatch(
    resp_body_json(resp, simplifyVector = FALSE),
    error = function(e) {
      warning(paste("Failed to parse Gemini response:", e$message))
      NULL
    }
  )

  if (is.null(raw)) return(.fallback_summary())

  # Extract text from response
  raw_text <- tryCatch(
    raw$candidates[[1]]$content$parts[[1]]$text,
    error = function(e) NULL
  )

  if (is.null(raw_text) || nchar(trimws(raw_text)) == 0) {
    warning("Gemini returned an empty response.")
    return(.fallback_summary())
  }

  .parse_gemini_response(raw_text)
}


# -----------------------------------------------------------------------------
# RESPONSE PARSER
# -----------------------------------------------------------------------------

#' Parse the structured Gemini text response into named fields
#'
#' Expects lines starting with RISK_LEVEL:, SUMMARY:, KEY_ACTION:
#' Gracefully handles minor formatting deviations.
#'
#' @param text Character. Raw text from Gemini.
#' @return Named list with: risk_level, summary, key_action, raw_text
.parse_gemini_response <- function(text) {
  extract <- function(label) {
    pattern <- sprintf("(?i)^%s:\\s*(.+?)\\s*$", label)
    lines   <- strsplit(text, "\n")[[1]]
    match   <- regmatches(lines, regexpr(pattern, lines, perl = TRUE))
    if (length(match) == 0) return(NA_character_)
    sub(sprintf("(?i)^%s:\\s*", label), "", match[[1]], perl = TRUE)
  }

  risk_raw <- tolower(trimws(extract("RISK_LEVEL")))

  # Normalise to valid values
  risk_level <- if (grepl("do_not|no.travel|do not", risk_raw)) {
    "do_not_travel"
  } else if (risk_raw %in% c("low", "moderate", "high")) {
    risk_raw
  } else {
    "moderate"  # conservative default if unrecognised
  }

  list(
    risk_level  = risk_level,
    summary     = trimws(extract("SUMMARY")),
    key_action  = trimws(extract("KEY_ACTION")),
    raw_text    = text
  )
}


#' Safe fallback when Gemini call fails
.fallback_summary <- function() {
  list(
    risk_level  = "unknown",
    summary     = "Road condition data could not be retrieved. Check CDOT's CoTrip website or call 511 before driving.",
    key_action  = "Check cotrip.org or call 511 for current conditions.",
    raw_text    = NA_character_
  )
}
