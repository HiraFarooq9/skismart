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
    weather   = "Breckenridge has a 68-inch base with 9 inches of fresh powder in the last 72 hours, and temperatures holding around 26°F keep the snow dry and light all day. Wind is light at 10 mph with no lift closure risk.",
    terrain   = "94% of lifts are running with an estimated 88% of intermediate-preferred trail types accessible across all five peaks. The deep base puts both groomed cruisers and off-piste terrain in excellent shape.",
    avalanche = "CAIC rates avalanche danger as Moderate at alpine elevations today, meaning human-triggered slides are possible on steep wind-loaded slopes above treeline. Groomed blue and black runs below treeline are unaffected and recommended for intermediate skiers."
  ),
  "Keystone Resort" = list(
    weather   = "Keystone has a solid 58-inch base with 7 inches of snowfall in the last 72 hours, and calm winds at 9 mph pose no lift closure risk. Temperatures around 28°F produce firm, grippy snow well-suited to groomed trail skiing.",
    terrain   = "91% of lifts are operational with an estimated 84% of intermediate-preferred trail types open, including long blue cruisers and gladed tree runs. Terrain variety and lift coverage are both strong for today's conditions.",
    avalanche = "Avalanche danger is rated Low across all elevations at Keystone today, meaning natural and human-triggered slides are unlikely. All in-bounds terrain is accessible without restriction."
  ),
  "Copper Mountain" = list(
    weather   = "Copper has a 54-inch base with 6 inches of recent snowfall, and temperatures around 27°F keep snow quality good throughout the day. Wind at 10 mph is well within the threshold for full lift operations.",
    terrain   = "88% of lifts are running with an estimated 79% of intermediate-preferred trail types open. Copper's naturally separated terrain zones keep blue runs uncrowded even on busy days.",
    avalanche = "Avalanche danger is Low across all elevations at Copper Mountain today, and all in-bounds terrain is open without restrictions. Conditions are safe for standard intermediate skiing throughout the mountain."
  ),
  "Vail Ski Resort" = list(
    weather   = "Vail has a deep 62-inch base with 8 inches of fresh snow from the last 72 hours, and temperatures around 29°F keep snow quality high across the front side and Back Bowls. Wind is slightly elevated at 12 mph and may affect a small number of high-alpine lifts.",
    terrain   = "92% of lifts are operating, with an estimated 75% of intermediate-preferred trail types accessible — wind restrictions limit a few upper-mountain runs. The front side and Blue Sky Basin remain wide open with exceptional variety for intermediate skiers.",
    avalanche = "CAIC rates avalanche danger as Moderate above treeline today, with wind-loaded slopes in the Back Bowls requiring caution. Front-side groomed runs are unaffected; confirm Back Bowl access with ski patrol on the day."
  ),
  "Steamboat Ski Resort" = list(
    weather   = "Steamboat has a 48-inch base with 5 inches of Champagne Powder in the last 72 hours, and temperatures of 22°F — the coldest in today's search — preserve exceptionally dry, light snow. Wind is the calmest of all five resorts at 7 mph with all lifts fully operational.",
    terrain   = "86% of lifts are running with an estimated 71% of intermediate-preferred trail types accessible, reflecting a slightly thinner base than the Summit County resorts. Steamboat's extensive glade and tree terrain is in particularly good shape after the recent snowfall.",
    avalanche = "Avalanche danger is rated Low across all elevations at Steamboat today, and the full mountain is open without terrain restrictions. Standard in-bounds safety awareness applies."
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

# Driving routes from Denver — detailed I-70 corridor waypoints for realistic curves
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

# Shared I-70 westbound spine: Denver → Frisco
.I70_TO_FRISCO_LATS <- c(39.739, 39.716, 39.697, 39.671, 39.654, 39.651,
                          39.660, 39.685, 39.721, 39.743, 39.731, 39.718,
                          39.706, 39.696, 39.692, 39.681, 39.682, 39.647,
                          39.632, 39.608, 39.574)
.I70_TO_FRISCO_LONS <- c(-104.990, -105.102, -105.268, -105.190, -105.190, -105.311,
                          -105.368, -105.452, -105.487, -105.514, -105.565, -105.601,
                          -105.697, -105.716, -105.727, -105.893, -105.906, -106.032,
                          -106.074, -106.084, -106.098)

# I-70 spine up to Silverthorne exit only (truncated before Frisco, for Keystone branch)
.I70_TO_SILVERTHORNE_LATS <- .I70_TO_FRISCO_LATS[1:19]
.I70_TO_SILVERTHORNE_LONS <- .I70_TO_FRISCO_LONS[1:19]

.DEMO_ROUTES <- list(
  # Denver → I-70 W → Frisco → Hwy 9 S → Breckenridge
  "Breckenridge Ski Resort" = .make_route(
    lats = c(.I70_TO_FRISCO_LATS,
             39.563, 39.548, 39.531, 39.516, 39.502, 39.490, 39.480),
    lons = c(.I70_TO_FRISCO_LONS,
             -106.094, -106.089, -106.081, -106.075, -106.070, -106.067, -106.067),
    duration_mins = 92, distance_miles = 79
  ),
  # Denver → I-70 W → Silverthorne exit → US-6 SE → Keystone
  # (branches off BEFORE Frisco — no backtrack)
  "Keystone Resort" = .make_route(
    lats = c(.I70_TO_SILVERTHORNE_LATS,
             39.624, 39.619, 39.616, 39.613, 39.611,
             39.608, 39.605, 39.601, 39.596, 39.590,
             39.585, 39.582),
    lons = c(.I70_TO_SILVERTHORNE_LONS,
             -106.063, -106.051, -106.040, -106.027, -106.013,
             -105.999, -105.985, -105.973, -105.963, -105.955,
             -105.948, -105.944),
    duration_mins = 89, distance_miles = 76
  ),
  # Denver → I-70 W → past Frisco → Exit 195 → Copper Mountain
  "Copper Mountain" = .make_route(
    lats = c(.I70_TO_FRISCO_LATS,
             39.566, 39.558, 39.548, 39.535, 39.520, 39.509, 39.502),
    lons = c(.I70_TO_FRISCO_LONS,
             -106.104, -106.112, -106.124, -106.135, -106.143, -106.150, -106.156),
    duration_mins = 95, distance_miles = 81
  ),
  # Denver → I-70 W → Copper → Vail Pass → Vail
  "Vail Ski Resort" = .make_route(
    lats = c(.I70_TO_FRISCO_LATS,
             39.566, 39.558, 39.548, 39.535, 39.520, 39.509, 39.502,
             39.510, 39.519, 39.528, 39.538, 39.547, 39.558,
             39.566, 39.574, 39.581, 39.590, 39.597, 39.606),
    lons = c(.I70_TO_FRISCO_LONS,
             -106.104, -106.112, -106.124, -106.135, -106.143, -106.150, -106.156,
             -106.170, -106.187, -106.204, -106.221, -106.239, -106.256,
             -106.274, -106.293, -106.313, -106.327, -106.341, -106.355),
    duration_mins = 115, distance_miles = 97
  ),
  # Denver → I-70 W to US-40 exit (Empire) → US-40 N → Rabbit Ears Pass → Steamboat
  "Steamboat Ski Resort" = .make_route(
    lats = c(39.739, 39.727, 39.716, 39.703, 39.692, 39.680,
             39.671, 39.665, 39.660, 39.657,
             39.662, 39.671, 39.686, 39.703, 39.722,
             39.751, 39.781, 39.814, 39.845, 39.872,
             39.900, 39.922, 39.942, 39.956,
             39.972, 39.998, 40.018, 40.038, 40.052,
             40.063, 40.075, 40.093, 40.112, 40.135,
             40.158, 40.184, 40.228, 40.282, 40.347,
             40.398, 40.435, 40.454, 40.470, 40.485),
    lons = c(-104.990, -105.045, -105.102, -105.154, -105.192, -105.232,
             -105.268, -105.302, -105.332, -105.360,
             -105.390, -105.418, -105.448, -105.475, -105.502,
             -105.530, -105.565, -105.608, -105.645, -105.678,
             -105.706, -105.724, -105.741, -105.756,
             -105.773, -105.810, -105.843, -105.886, -105.926,
             -105.972, -106.027, -106.095, -106.170, -106.280,
             -106.412, -106.528, -106.650, -106.762, -106.816,
             -106.825, -106.820, -106.812, -106.793, -106.770),
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
