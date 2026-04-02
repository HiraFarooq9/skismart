# =============================================================================
# test_stage2.R
# Interactive probe script for Stage 2 — run sections manually in RStudio
# =============================================================================

source("R/load_env.R")
source("R/api_cdot.R")
source("R/api_routing.R")
source("R/api_geocode.R")
source("R/route_conditions.R")
source("R/llm_route_summary.R")

CDOT_KEY   <- Sys.getenv("CDOT_API_KEY")
GH_KEY     <- Sys.getenv("GRAPHHOPPER_API_KEY")
GROQ_KEY   <- Sys.getenv("GROQ_API_KEY")

RESORT     <- "Breckenridge"
RESORT_LAT <- 39.4817
RESORT_LON <- -106.0384
SKI_DATE   <- as.Date("2026-04-05")


# =============================================================================
# SECTION 1: Geocode a user-entered location (no API key needed)
# =============================================================================

geo <- geocode_location("Denver, CO")
cat("\nGeocoded result:\n")
print(geo)

# Try a few realistic user inputs
cat("\nZip code input:\n");  print(geocode_location("80302"))
cat("\nCity only:\n");       print(geocode_location("Boulder"))
cat("\nAmbiguous input:\n"); print(geocode_location("Springfield", n_results = 3))

# Use geocoded Denver as start point
START_LAT <- geo$lat[1]
START_LON <- geo$lon[1]


# =============================================================================
# SECTION 2: CDOT fetch — one row per condition entry
# =============================================================================

cdot_df <- fetch_road_conditions(CDOT_KEY)

cat("\nTotal condition rows:", nrow(cdot_df), "\n")
print(head(cdot_df, 5))


# =============================================================================
# SECTION 3: Fetch route (geocoded start → Breckenridge)
# =============================================================================

route <- fetch_route(START_LAT, START_LON, RESORT_LAT, RESORT_LON, GH_KEY)

if (!is.null(route)) {
  cat(sprintf("\nRoute: %.1f miles, %.0f minutes\n",
              route$distance_miles, route$duration_mins))
  cat(sprintf("Waypoints: %d points\n", nrow(route$waypoints)))
  cat("BBox:", route$bbox, "\n")
} else {
  cat("Route fetch failed — check ORS account activation.\n")
}


# =============================================================================
# SECTION 4: Filter CDOT to route + build Gemini prompt
# =============================================================================

if (!is.null(route) && nrow(cdot_df) > 0) {

  result <- prepare_route_conditions(route, cdot_df, RESORT, SKI_DATE)

  cat("\n--- Preliminary risk level:", result$risk_level, "---\n")
  cat("Condition rows along route:", nrow(result$filtered_df), "\n\n")
  cat("--- Gemini Prompt ---\n")
  cat(result$llm_prompt)

} else {
  cat("\nSkipping filter — route unavailable. Testing Gemini with mock prompt.\n")

  # Mock prompt so we can test the Gemini call independently of ORS
  mock_prompt <- 'You are a road safety assistant helping a skier plan a drive to Breckenridge on 2026-04-05.
Estimated drive: 80 miles, 110 minutes under normal conditions.
Preliminary risk assessment: moderate

CONDITIONS ALONG ROUTE:
  [Eisenhower Tunnel | forecast text included | updated 2026-04-05 06:00 MDT]
  Expect Light Snow. Wind gusts up to 35 mph. Chain law in effect on I-70 westbound near the tunnel.

Respond with EXACTLY this structure and no other text:

RISK_LEVEL: [low | moderate | high | do_not_travel]
SUMMARY: [2-3 sentences on current driving conditions to Breckenridge.]
KEY_ACTION: [The single most important thing the driver should do, or "None".]'

  result <- list(llm_prompt = mock_prompt)
}


# =============================================================================
# SECTION 5: Call Gemini for plain-language route summary
# =============================================================================

llm_out <- call_groq_route_summary(result$llm_prompt, GROQ_KEY)

cat("\n--- Gemini Response ---\n")
cat("Risk level: ", llm_out$risk_level, "\n")
cat("Summary:    ", llm_out$summary,    "\n")
cat("Key action: ", llm_out$key_action, "\n")
