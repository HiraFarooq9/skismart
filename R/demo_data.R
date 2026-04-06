# =============================================================================
# demo_data.R
# Mock data for SkiSmart demo mode — simulates peak January conditions
# from Denver to top Front Range / Summit County ski areas.
# =============================================================================

# Five resorts ordered by composite score (best first).
.DEMO_RESORTS <- data.frame(
  resort_id         = c(5L, 17L, 8L, 30L, 27L),
  resort_name       = c("Breckenridge Ski Resort", "Keystone Resort",
                        "Copper Mountain", "Vail Ski Resort",
                        "Steamboat Ski Resort"),
  composite_score   = c(0.84, 0.78, 0.74, 0.69, 0.63),
  weather_score     = c(0.86, 0.81, 0.76, 0.73, 0.67),
  terrain_score     = c(0.91, 0.85, 0.82, 0.79, 0.70),
  avalanche_danger  = c("Moderate", "Low", "Low", "Moderate", "Low"),
  avalanche_applied = c(TRUE, TRUE, TRUE, TRUE, TRUE),
  lift_pct          = c(0.94, 0.91, 0.88, 0.92, 0.86),
  warnings          = NA_character_,
  error             = NA_character_,
  stringsAsFactors  = FALSE
)

.DEMO_ROUTING <- data.frame(
  resort_name      = c("Breckenridge Ski Resort", "Keystone Resort",
                       "Copper Mountain", "Vail Ski Resort",
                       "Steamboat Ski Resort"),
  lat              = c(39.48,    39.582,   39.502,   39.606,   40.454),
  lon              = c(-106.067, -105.944, -106.156, -106.355, -106.770),
  conditions_score = c(0.84, 0.78, 0.74, 0.69, 0.63),
  duration_mins    = c(92,  89,  95,  115,  162),
  distance_miles   = c(79,  76,  81,   97,  157),
  stringsAsFactors = FALSE
)

.DEMO_LLM_ROUTES <- list(
  "Breckenridge Ski Resort" = list(
    risk_level = "low",
    summary    = "I-70 west and Highway 9 are clear with dry pavement. Chain laws are not in effect. Standard winter tires are sufficient.",
    key_action = "No special action needed — roads are clear.",
    raw_text   = NA_character_
  ),
  "Keystone Resort" = list(
    risk_level = "low",
    summary    = "I-70 west to US-6 is clear and well-maintained. Shortest drive of the five resorts, with light traffic expected.",
    key_action = "None.",
    raw_text   = NA_character_
  ),
  "Copper Mountain" = list(
    risk_level = "low",
    summary    = "I-70 west to Exit 195 is clear. Overnight snowfall has been plowed. Normal winter driving conditions apply.",
    key_action = "None.",
    raw_text   = NA_character_
  ),
  "Vail Ski Resort" = list(
    risk_level = "low",
    summary    = "I-70 through the Eisenhower Tunnel is open. Vail Pass is clear but can be icy at dawn — allow extra travel time.",
    key_action = "Check CDOT 511 for Vail Pass conditions before departure.",
    raw_text   = NA_character_
  ),
  "Steamboat Ski Resort" = list(
    risk_level = "moderate",
    summary    = "US-40 over Rabbit Ears Pass has light snow coverage and traction laws may be in effect through the pass.",
    key_action = "Carry chains or snow-rated tires. Check CDOT 511 before departure.",
    raw_text   = NA_character_
  )
)

.DEMO_SCORE_INTERP <- list(
  "Breckenridge Ski Resort" = list(
    weather   = "Excellent conditions — 68-inch base, 9 inches of fresh snow in the past 72 hours, and temperatures around 26°F make for ideal powder skiing.",
    terrain   = "Top terrain score across today's resorts — 94% of lifts running with strong access to blue and black runs suited for intermediate skiers.",
    avalanche = "Moderate danger at alpine elevations; stick to groomed runs and avoid off-piste cornices and wind-loaded slopes."
  ),
  "Keystone Resort" = list(
    weather   = "Solid conditions with a 58-inch base and 7 inches of recent snowfall; slightly warmer temperatures produce dense, grippy snow.",
    terrain   = "2nd-best terrain score today — 91% of lifts open with excellent blue cruisers and tree runs well-suited to intermediate ability.",
    avalanche = "Low avalanche danger — all elevations are safe for normal in-bounds skiing."
  ),
  "Copper Mountain" = list(
    weather   = "Good snow coverage at 54 inches base with 6 inches of new snow; groomed runs are firm early with softer conditions by mid-morning.",
    terrain   = "88% of lifts running and a well-balanced trail mix — a reliable pick for intermediates looking for variety.",
    avalanche = "Low avalanche danger — all terrain is accessible without restrictions today."
  ),
  "Vail Ski Resort" = list(
    weather   = "62-inch base with 8 inches of recent snow; temperatures just below freezing keep snow quality high across the mountain.",
    terrain   = "92% lift operations with a large trail network — terrain score is 4th today due to the longer drive adding a time cost.",
    avalanche = "Moderate danger above treeline; Back Bowls access may be limited — confirm with ski patrol on arrival."
  ),
  "Steamboat Ski Resort" = list(
    weather   = "48-inch base with 5 inches of Champagne Powder in the past 72 hours; colder temperatures preserve excellent dry snow quality.",
    terrain   = "Lowest terrain score today due to the 2.5-hour drive — 86% of lifts open with strong glade and tree skiing terrain.",
    avalanche = "Low avalanche danger — full mountain access without restrictions."
  )
)

.DEMO_HOURLY <- list(
  "Breckenridge Ski Resort" = data.frame(
    hour = 8:16,
    hour_label = c("8am","9am","10am","11am","12pm","1pm","2pm","3pm","4pm"),
    temp_f   = c(22, 24, 26, 27, 28, 27, 26, 25, 23),
    snow_in  = c(0.4, 0.3, 0.2, 0.1, 0.0, 0.1, 0.2, 0.3, 0.3),
    wind_mph = c(9, 10, 11, 12, 12, 11, 10, 9, 8),
    stringsAsFactors = FALSE
  ),
  "Keystone Resort" = data.frame(
    hour = 8:16,
    hour_label = c("8am","9am","10am","11am","12pm","1pm","2pm","3pm","4pm"),
    temp_f   = c(24, 26, 28, 30, 31, 30, 29, 28, 26),
    snow_in  = c(0.2, 0.1, 0.1, 0.0, 0.0, 0.0, 0.1, 0.2, 0.2),
    wind_mph = c(7, 8, 9, 10, 10, 9, 8, 7, 7),
    stringsAsFactors = FALSE
  ),
  "Copper Mountain" = data.frame(
    hour = 8:16,
    hour_label = c("8am","9am","10am","11am","12pm","1pm","2pm","3pm","4pm"),
    temp_f   = c(23, 25, 27, 28, 29, 28, 27, 26, 24),
    snow_in  = c(0.3, 0.2, 0.1, 0.0, 0.0, 0.1, 0.1, 0.2, 0.2),
    wind_mph = c(8, 9, 10, 11, 11, 10, 9, 8, 7),
    stringsAsFactors = FALSE
  ),
  "Vail Ski Resort" = data.frame(
    hour = 8:16,
    hour_label = c("8am","9am","10am","11am","12pm","1pm","2pm","3pm","4pm"),
    temp_f   = c(25, 27, 29, 31, 32, 31, 30, 28, 26),
    snow_in  = c(0.3, 0.2, 0.1, 0.0, 0.0, 0.1, 0.2, 0.2, 0.1),
    wind_mph = c(10, 11, 13, 14, 14, 13, 12, 10, 9),
    stringsAsFactors = FALSE
  ),
  "Steamboat Ski Resort" = data.frame(
    hour = 8:16,
    hour_label = c("8am","9am","10am","11am","12pm","1pm","2pm","3pm","4pm"),
    temp_f   = c(18, 20, 22, 23, 24, 23, 22, 20, 18),
    snow_in  = c(0.5, 0.4, 0.3, 0.2, 0.1, 0.1, 0.2, 0.3, 0.4),
    wind_mph = c(6, 7, 7, 8, 8, 7, 7, 6, 6),
    stringsAsFactors = FALSE
  )
)

# Approximate driving routes from Denver to each resort (key highway waypoints)
.make_route <- function(lats, lons, duration_mins, distance_miles) {
  wp <- data.frame(lat = lats, lon = lons, stringsAsFactors = FALSE)
  list(
    waypoints      = wp,
    duration_mins  = duration_mins,
    distance_miles = distance_miles,
    bbox = c(min_lat = min(lats), max_lat = max(lats),
             min_lon = min(lons), max_lon = max(lons))
  )
}

.DEMO_ROUTES <- list(
  "Breckenridge Ski Resort" = .make_route(
    lats = c(39.739, 39.748, 39.743, 39.681, 39.574, 39.480),
    lons = c(-104.990, -105.223, -105.514, -105.905, -106.098, -106.067),
    duration_mins = 92, distance_miles = 79
  ),
  "Keystone Resort" = .make_route(
    lats = c(39.739, 39.748, 39.743, 39.681, 39.574, 39.582),
    lons = c(-104.990, -105.223, -105.514, -105.905, -106.098, -105.944),
    duration_mins = 89, distance_miles = 76
  ),
  "Copper Mountain" = .make_route(
    lats = c(39.739, 39.748, 39.743, 39.681, 39.530, 39.502),
    lons = c(-104.990, -105.223, -105.514, -105.905, -106.127, -106.156),
    duration_mins = 95, distance_miles = 81
  ),
  "Vail Ski Resort" = .make_route(
    lats = c(39.739, 39.748, 39.743, 39.681, 39.530, 39.571, 39.606),
    lons = c(-104.990, -105.223, -105.514, -105.905, -106.127, -106.217, -106.355),
    duration_mins = 115, distance_miles = 97
  ),
  "Steamboat Ski Resort" = .make_route(
    lats = c(39.739, 39.745, 39.757, 39.804, 40.093, 40.154, 40.485),
    lons = c(-104.990, -105.385, -105.683, -105.775, -105.938, -106.913, -106.831),
    duration_mins = 162, distance_miles = 157
  )
)

.DEMO_SNAPSHOT <- list(
  "Breckenridge Ski Resort" = list(depth_in = 68L, snowfall_72hr_in = 9.2, avg_temp_f = 26L, avg_wind_mph = 10L),
  "Keystone Resort"         = list(depth_in = 58L, snowfall_72hr_in = 7.1, avg_temp_f = 28L, avg_wind_mph =  9L),
  "Copper Mountain"         = list(depth_in = 54L, snowfall_72hr_in = 6.3, avg_temp_f = 27L, avg_wind_mph = 10L),
  "Vail Ski Resort"         = list(depth_in = 62L, snowfall_72hr_in = 8.0, avg_temp_f = 29L, avg_wind_mph = 12L),
  "Steamboat Ski Resort"    = list(depth_in = 48L, snowfall_72hr_in = 5.4, avg_temp_f = 22L, avg_wind_mph =  7L)
)


# =============================================================================
# PUBLIC FUNCTIONS
# =============================================================================

make_demo_pipeline_result <- function() {
  ranking <- .DEMO_ROUTING

  # Fake CDOT condition points for Vail (Vail Pass) and Steamboat (Rabbit Ears Pass)
  # filtered_df needs: segment_id, condition_id, start_lat, start_lon, row_risk
  make_conditions <- function(lats, lons, risks) {
    data.frame(
      segment_id   = seq_along(lats),
      condition_id = seq_along(lats),
      start_lat    = lats,
      start_lon    = lons,
      row_risk     = risks,
      stringsAsFactors = FALSE
    )
  }

  vail_conditions <- make_conditions(
    lats  = c(39.565, 39.571, 39.578, 39.598, 39.606),
    lons  = c(-106.210, -106.217, -106.235, -106.335, -106.355),
    risks = c("moderate", "moderate", "moderate", "moderate", "moderate")
  )

  steamboat_conditions <- make_conditions(
    lats  = c(40.148, 40.154, 40.162, 40.171),
    lons  = c(-106.895, -106.913, -106.920, -106.910),
    risks = c("moderate", "moderate", "moderate", "moderate")
  )

  route_analyses <- list(
    "Breckenridge Ski Resort" = list(
      route = .DEMO_ROUTES[["Breckenridge Ski Resort"]], route_prep = NULL,
      llm   = .DEMO_LLM_ROUTES[["Breckenridge Ski Resort"]]
    ),
    "Keystone Resort" = list(
      route = .DEMO_ROUTES[["Keystone Resort"]], route_prep = NULL,
      llm   = .DEMO_LLM_ROUTES[["Keystone Resort"]]
    ),
    "Copper Mountain" = list(
      route = .DEMO_ROUTES[["Copper Mountain"]], route_prep = NULL,
      llm   = .DEMO_LLM_ROUTES[["Copper Mountain"]]
    ),
    "Vail Ski Resort" = list(
      route      = .DEMO_ROUTES[["Vail Ski Resort"]],
      route_prep = list(filtered_df = vail_conditions),
      llm        = .DEMO_LLM_ROUTES[["Vail Ski Resort"]]
    ),
    "Steamboat Ski Resort" = list(
      route      = .DEMO_ROUTES[["Steamboat Ski Resort"]],
      route_prep = list(filtered_df = steamboat_conditions),
      llm        = .DEMO_LLM_ROUTES[["Steamboat Ski Resort"]]
    )
  )

  list(
    stage1_df = .DEMO_RESORTS,
    reranked  = list(
      ranking         = ranking,
      all_resorts     = ranking[, c("resort_name", "duration_mins", "distance_miles")],
      winner          = as.list(ranking[1, ]),
      drive_time_flag = "within_preference",
      routes          = list()
    ),
    winner = list(
      resort_name = ranking$resort_name[1],
      lat         = ranking$lat[1],
      lon         = ranking$lon[1]
    ),
    all_route_analyses = route_analyses,
    start_lat          = 39.7392,
    start_lon          = -104.9903,
    start_label        = "Denver"
  )
}

make_demo_display <- function(resort_name) {
  snap <- .DEMO_SNAPSHOT[[resort_name]]
  hrly <- .DEMO_HOURLY[[resort_name]]
  if (is.null(snap)) return(NULL)
  list(
    hourly           = hrly,
    depth_in         = snap$depth_in,
    snowfall_72hr_in = snap$snowfall_72hr_in,
    avg_temp_f       = snap$avg_temp_f,
    avg_wind_mph     = snap$avg_wind_mph
  )
}

make_demo_score_interp <- function(resort_name) {
  interp <- .DEMO_SCORE_INTERP[[resort_name]]
  if (is.null(interp)) return(list(.failed = TRUE))
  interp
}
