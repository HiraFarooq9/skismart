# =============================================================================
# app.R — SkiSmart Shiny Application
# =============================================================================
#
# PIPELINE
#   Stage 1 — R/composite_score.R: weather + terrain + avalanche → top resorts
#   Stage 2 — geocode → route → drive-time rerank → CDOT → Groq LLM summary
#
# TO RUN
#   Open in RStudio and click "Run App", or: shiny::runApp("app.R")
#   Working directory must be the project root.
#
# DEPENDENCIES
#   Stage 2 files live in R/. API keys in .env (copy from .env.example).
# =============================================================================

library(shiny)
library(dplyr)
library(leaflet)

source("R/composite_score.R")  # Stage 1: score_all_resorts()
source("R/load_env.R")         # loads .env keys into Sys.setenv()
source("R/api_geocode.R")
source("R/api_routing.R")
source("R/api_cdot.R")
source("R/route_conditions.R")
source("R/llm_route_summary.R")
source("R/stage2_rerank.R")

resorts <- read.csv("data/resorts.csv", stringsAsFactors = FALSE)

GH_KEY   <- Sys.getenv("GRAPHHOPPER_API_KEY")
CDOT_KEY <- Sys.getenv("CDOT_API_KEY")
GROQ_KEY <- Sys.getenv("GROQ_API_KEY")


# -----------------------------------------------------------------------------
# HELPERS
# -----------------------------------------------------------------------------

risk_color <- function(risk) {
  dplyr::case_when(
    risk == "do_not_travel" ~ "#7B2D2D",
    risk == "high"          ~ "#dc3545",
    risk == "moderate"      ~ "#fd7e14",
    TRUE                    ~ "#888888"
  )
}

color_route_segments <- function(waypoints, conditions, buffer_deg = 0.05) {
  priority <- c("clear" = 0, "low" = 1, "moderate" = 2,
                "high" = 3, "do_not_travel" = 4)

  point_risks <- vapply(seq_len(nrow(waypoints)), function(i) {
    dists  <- sqrt((conditions$lat - waypoints$lat[i])^2 +
                   (conditions$lon - waypoints$lon[i])^2)
    nearby <- conditions$row_risk[dists <= buffer_deg]
    if (length(nearby) == 0) return("clear")
    nearby[which.max(priority[nearby])]
  }, character(1))

  segments <- list()
  i <- 1
  n <- length(point_risks)
  while (i <= n) {
    j <- i
    while (j < n && point_risks[j + 1] == point_risks[j]) j <- j + 1
    end <- min(j + 1, n)
    segments[[length(segments) + 1]] <- list(
      lats = waypoints$lat[i:end],
      lons = waypoints$lon[i:end],
      risk = point_risks[i]
    )
    i <- j + 1
  }
  segments
}


# =============================================================================
# STYLING
# =============================================================================

skismart_css <- "

  /* ── Palette ──────────────────────────────────────────────────────────── */
  :root {
    --navy:       #1B3A5C;
    --blue:       #2E6DA4;
    --blue-light: #5BA4CF;
    --blue-pale:  #EBF5FF;
    --snow:       #F4F8FC;
    --border:     #D6E4F0;
    --text:       #1A2B3C;
    --muted:      #6B7C8F;
    --white:      #FFFFFF;
    --danger-low:          #2E7D32;
    --danger-moderate:     #F57F17;
    --danger-considerable: #E65100;
    --danger-high:         #B71C1C;
    --danger-extreme:      #6A1B9A;
  }

  /* ── Page ─────────────────────────────────────────────────────────────── */
  body {
    background-color: var(--snow);
    font-family: 'Segoe UI', system-ui, -apple-system, sans-serif;
    color: var(--text);
  }

  /* ── Header bar ───────────────────────────────────────────────────────── */
  .skismart-header {
    background: linear-gradient(135deg, var(--navy) 0%, var(--blue) 100%);
    color: white;
    padding: 18px 28px 16px 28px;
    margin: -15px -15px 24px -15px;
    display: flex;
    align-items: center;
    gap: 16px;
  }
  .skismart-header h1 {
    margin: 0;
    font-size: 28px;
    font-weight: 700;
    letter-spacing: -0.5px;
    color: white;
  }
  .skismart-header p {
    margin: 2px 0 0 0;
    font-size: 13px;
    color: rgba(255,255,255,0.75);
    letter-spacing: 0.2px;
  }

  /* ── Sidebar card ─────────────────────────────────────────────────────── */
  .well {
    background: var(--white) !important;
    border: 1px solid var(--border) !important;
    border-radius: 10px !important;
    box-shadow: 0 2px 8px rgba(27,58,92,0.07) !important;
    padding: 20px !important;
  }
  .well h4 {
    color: var(--navy);
    font-size: 13px;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: 0.8px;
    margin-bottom: 14px;
  }
  .well hr {
    border-color: var(--border);
    margin: 16px 0;
  }
  .well .help-block {
    color: var(--muted);
    font-size: 11px;
    line-height: 1.5;
  }

  /* ── Labels & inputs ──────────────────────────────────────────────────── */
  label {
    font-size: 12px;
    font-weight: 600;
    color: var(--muted);
    text-transform: uppercase;
    letter-spacing: 0.5px;
  }
  .form-control, .selectize-input {
    border-color: var(--border) !important;
    border-radius: 6px !important;
    font-size: 14px !important;
  }
  .form-control:focus {
    border-color: var(--blue-light) !important;
    box-shadow: 0 0 0 2px rgba(91,164,207,0.2) !important;
  }

  /* ── Find Resorts button ──────────────────────────────────────────────── */
  #find_resorts {
    background: linear-gradient(135deg, var(--blue) 0%, var(--blue-light) 100%);
    border: none;
    border-radius: 8px;
    font-size: 14px;
    font-weight: 600;
    letter-spacing: 0.4px;
    padding: 10px;
    box-shadow: 0 3px 10px rgba(46,109,164,0.35);
    transition: opacity 0.15s;
  }
  #find_resorts:hover { opacity: 0.88; }

  /* ── Result cards ─────────────────────────────────────────────────────── */
  .result-card {
    background: var(--white);
    border: 1px solid var(--border);
    border-radius: 10px;
    padding: 22px 24px;
    margin-bottom: 20px;
    box-shadow: 0 2px 8px rgba(27,58,92,0.06);
  }
  .result-card h3 {
    color: var(--navy);
    font-size: 17px;
    font-weight: 700;
    margin-top: 0;
    margin-bottom: 4px;
  }
  .result-card .subtitle {
    color: var(--muted);
    font-size: 13px;
    margin-bottom: 16px;
  }

  /* ── Tables ───────────────────────────────────────────────────────────── */
  .table {
    font-size: 13px;
    border-collapse: separate;
    border-spacing: 0;
    width: 100%;
  }
  .table thead th {
    background-color: var(--blue-pale);
    color: var(--navy);
    font-size: 11px;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: 0.6px;
    padding: 10px 12px;
    border-bottom: 2px solid var(--border);
  }
  .table tbody tr:hover { background-color: var(--blue-pale); }
  .table tbody td {
    padding: 10px 12px;
    border-bottom: 1px solid var(--border);
    vertical-align: middle;
  }
  .table tbody tr:last-child td { border-bottom: none; }

  /* ── Warnings panel ───────────────────────────────────────────────────── */
  .warnings-card {
    background: #FFFBEA;
    border: 1px solid #F9E08A;
    border-left: 4px solid #F0B429;
    border-radius: 8px;
    padding: 14px 18px;
    margin-bottom: 20px;
  }
  .warnings-card h4 {
    color: #7D5A00;
    font-size: 13px;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: 0.6px;
    margin: 0 0 10px 0;
  }
  .warnings-card ul {
    margin: 0;
    padding-left: 18px;
    font-size: 13px;
    color: #5A4000;
    line-height: 1.8;
  }

  /* ── Avalanche danger badges ──────────────────────────────────────────── */
  .badge-low          { color: var(--danger-low);          font-weight: 700; }
  .badge-moderate     { color: var(--danger-moderate);     font-weight: 700; }
  .badge-considerable { color: var(--danger-considerable); font-weight: 700; }
  .badge-high         { color: var(--danger-high);         font-weight: 700; }
  .badge-extreme      { color: var(--danger-extreme);      font-weight: 700; }

  /* ── Welcome banner ───────────────────────────────────────────────────── */
  .welcome-banner {
    background: var(--blue-pale);
    border: 1px solid var(--border);
    border-left: 4px solid var(--blue-light);
    border-radius: 8px;
    padding: 18px 20px;
    color: var(--navy);
    font-size: 14px;
    margin-top: 8px;
  }
  .welcome-banner strong { color: var(--blue); }

  /* ── Data source footer ───────────────────────────────────────────────── */
  .data-footer {
    font-size: 11px;
    color: var(--muted);
    text-align: center;
    padding: 12px 0 4px 0;
    border-top: 1px solid var(--border);
    margin-top: 24px;
  }
"

mountain_logo <- tags$svg(
  xmlns = "http://www.w3.org/2000/svg",
  width = "48", height = "38", viewBox = "0 0 48 38",
  tags$polygon(points = "24,2 46,36 2,36",  fill = "rgba(255,255,255,0.25)"),
  tags$polygon(points = "24,2 31,14 17,14", fill = "white"),
  tags$polygon(points = "36,12 46,36 26,36", fill = "rgba(255,255,255,0.18)")
)


# =============================================================================
# UI
# =============================================================================

ui <- fluidPage(

  tags$head(tags$style(HTML(skismart_css))),

  # ── Header ──────────────────────────────────────────────────────────────────
  div(class = "skismart-header",
    mountain_logo,
    div(
      tags$h1("SkiSmart"),
      tags$p("Colorado ski resort recommendations · Live conditions")
    )
  ),

  sidebarLayout(

    # ── Sidebar ─────────────────────────────────────────────────────────────
    sidebarPanel(
      width = 3,

      h4("Starting Location"),
      textInput(
        inputId     = "start_location",
        label       = "Your city or address",
        value       = "Denver, CO",
        placeholder = "e.g. Denver, CO  or  80302"
      ),

      hr(),
      h4("Trip Details"),

      dateInput(
        inputId = "trip_date",
        label   = "Date of trip",
        value   = Sys.Date() + 1,
        min     = Sys.Date(),
        max     = Sys.Date() + 10,
        format  = "mm/dd/yyyy"
      ),

      numericInput(
        inputId = "max_drive_hours",
        label   = "Max drive time (hours)",
        value   = 4,
        min     = 1,
        max     = 8,
        step    = 0.5
      ),

      hr(),
      h4("Skier Profile"),

      selectInput(
        inputId  = "ability_level",
        label    = "Ability level",
        choices  = c(
          "Beginner"     = "beginner",
          "Intermediate" = "intermediate",
          "Advanced"     = "advanced",
          "Expert"       = "expert"
        ),
        selected = "intermediate"
      ),

      selectInput(
        inputId  = "terrain_pref",
        label    = "Terrain preference",
        choices  = c(
          "Mostly groomed runs"          = "groomed",
          "Mix of groomed and off-piste" = "mixed",
          "Challenging / off-piste"      = "challenging"
        ),
        selected = "mixed"
      ),

      selectInput(
        inputId  = "pass_type",
        label    = "Season pass",
        choices  = c(
          "None / buy day ticket" = "none",
          "Ikon Pass"             = "ikon",
          "Epic Pass"             = "epic"
        ),
        selected = "none"
      ),

      hr(),
      h4("Safety Preferences"),

      sliderInput(
        inputId = "risk_tolerance",
        label   = "Risk tolerance",
        min     = 0,
        max     = 1,
        value   = 0.5,
        step    = 0.1,
        ticks   = FALSE
      ),
      helpText("0 = avoid all risk, 1 = comfortable with higher danger ratings"),

      hr(),
      actionButton(
        inputId = "find_resorts",
        label   = "Find Resorts",
        class   = "btn-primary",
        width   = "100%"
      ),

      br(), br(),
      helpText(
        "Weather: Open-Meteo (10-day forecast).",
        tags$br(),
        "Avalanche: CAIC (today & tomorrow only).",
        tags$br(),
        "33 Colorado resorts · Season dates applied."
      )
    ),

    # ── Main panel ────────────────────────────────────────────────────────────
    mainPanel(
      width = 9,

      uiOutput("status_message"),

      # ── Map ──────────────────────────────────────────────────────────────
      leafletOutput("route_map", height = "400px"),
      br(),

      # ── Route risk panel ─────────────────────────────────────────────────
      uiOutput("route_risk_panel"),

      # ── Resort recommendations ────────────────────────────────────────────
      uiOutput("results_card"),

      # ── Warnings ─────────────────────────────────────────────────────────
      uiOutput("warnings_panel"),

      # ── Avalanche conditions ──────────────────────────────────────────────
      uiOutput("avalanche_card"),

      div(class = "data-footer",
        "Data sources: Open-Meteo · Colorado Avalanche Information Center (CAIC) · Static resort database"
      )
    )
  )
)


# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {

  # ── Full pipeline ────────────────────────────────────────────────────────────
  pipeline_result <- eventReactive(input$find_resorts, {

    # --- Stage 1: conditions scoring ------------------------------------------
    showNotification("Scoring resorts...", type = "message",
                     duration = NULL, id = "loading_s1")
    stage1_df <- tryCatch(
      score_all_resorts(resorts, input$trip_date, input$ability_level),
      error = function(e) {
        showNotification(paste("Scoring error:", e$message), type = "error", duration = 8)
        NULL
      }
    )
    removeNotification("loading_s1")

    if (is.null(stage1_df) || nrow(stage1_df) == 0) {
      return(list(error = "No resorts available for that date."))
    }

    # --- Geocode start location -----------------------------------------------
    showNotification("Geocoding start location...", type = "message",
                     duration = NULL, id = "loading_geo")
    geo <- tryCatch(geocode_location(input$start_location, n_results = 1),
                    error = function(e) NULL)
    removeNotification("loading_geo")

    if (is.null(geo) || nrow(geo) == 0) {
      showNotification(paste0("Could not geocode '", input$start_location,
                              "'. Showing scoring results only."),
                       type = "warning", duration = 8)
      return(list(stage1_df = stage1_df,
                  geo_error = paste("Could not geocode:", input$start_location)))
    }

    start_lat   <- geo$lat[1]
    start_lon   <- geo$lon[1]
    start_label <- input$start_location
    user_max_mins <- input$max_drive_hours * 60

    # --- Stage 2: drive-time reranking ----------------------------------------
    showNotification("Fetching drive times...", type = "message",
                     duration = NULL, id = "loading_route")

    # Join lat/lon from resorts CSV onto top 3 scored resorts
    top3 <- stage1_df |>
      filter(!is.na(composite_score)) |>
      slice_head(n = 3) |>
      left_join(resorts[, c("resort_name", "latitude", "longitude")],
                by = "resort_name") |>
      transmute(
        resort_name      = resort_name,
        lat              = latitude,
        lon              = longitude,
        conditions_score = composite_score
      )

    reranked <- tryCatch(
      rerank_with_drive_time(top3, start_lat, start_lon, user_max_mins, GH_KEY),
      error = function(e) {
        showNotification(paste("Routing error:", e$message), type = "warning", duration = 6)
        NULL
      }
    )
    removeNotification("loading_route")

    if (is.null(reranked)) {
      return(list(stage1_df = stage1_df,
                  start_lat = start_lat, start_lon = start_lon,
                  start_label = start_label))
    }

    # --- Winner route for map polyline ----------------------------------------
    winner <- reranked$winner
    winner_route <- tryCatch(
      fetch_route(start_lat, start_lon, winner$lat, winner$lon, GH_KEY),
      error = function(e) NULL
    )

    # --- CDOT + Groq for winner -----------------------------------------------
    showNotification("Analyzing road conditions...", type = "message",
                     duration = NULL, id = "loading_llm")

    llm_out <- NULL; route_prep <- NULL

    if (!is.null(winner_route)) {
      cdot_df <- tryCatch(fetch_road_conditions(CDOT_KEY), error = function(e) NULL)

      if (!is.null(cdot_df) && nrow(cdot_df) > 0) {
        route_prep <- tryCatch(
          prepare_route_conditions(winner_route, cdot_df,
                                   winner$resort_name, input$trip_date),
          error = function(e) NULL
        )
        if (!is.null(route_prep) && nchar(GROQ_KEY) > 0) {
          llm_out <- tryCatch(
            call_groq_route_summary(route_prep$llm_prompt, GROQ_KEY),
            error = function(e) NULL
          )
        }
      }
    }
    removeNotification("loading_llm")

    list(
      stage1_df    = stage1_df,
      reranked     = reranked,
      winner_route = winner_route,
      route_prep   = route_prep,
      llm          = llm_out,
      start_lat    = start_lat,
      start_lon    = start_lon,
      start_label  = start_label
    )
  })


  # ── Welcome / status message ─────────────────────────────────────────────────
  output$status_message <- renderUI({
    if (!isTruthy(pipeline_result())) {
      div(class = "welcome-banner",
        "Select your ", strong("trip date"), " and ", strong("ability level"),
        " in the sidebar, then click ", strong("Find Resorts"), ".",
        tags$br(), tags$br(),
        tags$small(style = "color: #6B7C8F;",
          "Scores combine weather quality (50%) and terrain accessibility (50%),
           adjusted for avalanche danger when available."
        )
      )
    } else if (!is.null(pipeline_result()$error)) {
      div(class = "alert alert-danger", pipeline_result()$error)
    } else if (!is.null(pipeline_result()$geo_error)) {
      div(class = "alert alert-warning", pipeline_result()$geo_error)
    }
  })


  # ── Leaflet map ──────────────────────────────────────────────────────────────
  output$route_map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = -106.0, lat = 39.5, zoom = 7)
  })

  observeEvent(pipeline_result(), {
    result <- pipeline_result()
    if (is.null(result)) return()

    proxy <- leafletProxy("route_map") |> clearMarkers() |> clearShapes() |> clearControls()

    # Start location marker
    if (!is.null(result$start_lat)) {
      proxy <- proxy |>
        addCircleMarkers(
          lng = result$start_lon, lat = result$start_lat,
          radius = 10, color = "#333", fillColor = "#333", fillOpacity = 1,
          popup = paste0("<b>Start:</b> ", result$start_label),
          label = result$start_label
        )
    }

    # Route polyline colored by risk segment
    if (!is.null(result$winner_route) && !is.null(result$winner_route$waypoints)) {
      wp <- result$winner_route$waypoints

      if (!is.null(result$route_prep) && nrow(result$route_prep$filtered_df) > 0) {
        conds <- result$route_prep$filtered_df |>
          dplyr::distinct(segment_id, condition_id, .keep_all = TRUE) |>
          dplyr::transmute(lat = start_lat, lon = start_lon, row_risk = row_risk)
        segments <- color_route_segments(wp, conds)
      } else {
        segments <- list(list(lats = wp$lat, lons = wp$lon, risk = "clear"))
      }

      for (seg in segments) {
        proxy <- proxy |>
          addPolylines(
            lng = seg$lons, lat = seg$lats,
            color   = risk_color(seg$risk),
            weight  = if (seg$risk %in% c("high", "do_not_travel")) 6 else 4,
            opacity = 0.85
          )
      }
    }

    # Resort markers
    if (!is.null(result$reranked)) {
      ranking     <- result$reranked$ranking
      winner_name <- result$reranked$winner$resort_name

      for (i in seq_len(nrow(ranking))) {
        r         <- ranking[i, ]
        is_winner <- r$resort_name == winner_name
        drive_str <- if (is.na(r$duration_mins)) "Drive time unavailable" else
          sprintf("%.0f min / %.0f mi", r$duration_mins, r$distance_miles)
        proxy <- proxy |>
          addCircleMarkers(
            lng = r$lon, lat = r$lat,
            radius      = if (is_winner) 14 else 10,
            color       = if (is_winner) "#1B3A5C" else "#555",
            fillColor   = if (is_winner) "#2E6DA4" else "#aaa",
            fillOpacity = 0.9, weight = 2,
            popup = sprintf("<b>#%d %s</b><br>Drive: %s<br>Score: %.3f",
                            i, r$resort_name, drive_str, r$final_score),
            label = r$resort_name
          )
      }
    } else if (!is.null(result$stage1_df)) {
      top <- result$stage1_df |>
        filter(!is.na(composite_score)) |>
        slice_head(n = 3) |>
        left_join(resorts[, c("resort_name", "latitude", "longitude")], by = "resort_name")

      for (i in seq_len(nrow(top))) {
        r <- top[i, ]
        proxy <- proxy |>
          addCircleMarkers(
            lng = r$longitude, lat = r$latitude,
            radius = if (i == 1) 14 else 10,
            color  = if (i == 1) "#1B3A5C" else "#555",
            fillColor = if (i == 1) "#2E6DA4" else "#aaa",
            fillOpacity = 0.9, weight = 2,
            popup = sprintf("<b>#%d %s</b><br>Score: %.3f", i, r$resort_name, r$composite_score),
            label = r$resort_name
          )
      }
    }

    # Legend
    proxy <- proxy |>
      addLegend(
        position = "bottomright",
        colors   = c("#dc3545", "#fd7e14", "#888888", "#2E6DA4"),
        labels   = c("High risk road section", "Moderate risk road section",
                     "Clear route", "Recommended resort"),
        opacity  = 0.85,
        title    = "Map Legend"
      )
  })


  # ── Route risk panel ─────────────────────────────────────────────────────────
  output$route_risk_panel <- renderUI({
    req(pipeline_result())
    result <- pipeline_result()
    if (is.null(result$reranked)) return(NULL)

    winner    <- result$reranked$winner
    drive_str <- if (is.na(winner$duration_mins)) "unavailable" else
      sprintf("%.0f min / %.0f mi", winner$duration_mins, winner$distance_miles)

    flag_ui <- switch(result$reranked$drive_time_flag,
      "all_exceed_preference" = div(
        class = "alert alert-warning", style = "margin-top:8px;",
        strong("Note: "), "All resorts exceed your max drive time. Ranked by conditions."
      ),
      "slightly_over" = div(
        class = "alert alert-info", style = "margin-top:8px;",
        strong("Note: "), "Top resort is slightly over your preferred drive time."
      ),
      NULL
    )

    if (!is.null(result$llm)) {
      risk <- result$llm$risk_level
      badge_color <- switch(risk,
        "low" = "#28a745", "moderate" = "#fd7e14",
        "high" = "#dc3545", "do_not_travel" = "#7B2D2D", "#6c757d"
      )
      risk_label <- switch(risk,
        "low" = "LOW RISK", "moderate" = "MODERATE RISK",
        "high" = "HIGH RISK", "do_not_travel" = "DO NOT TRAVEL", "UNKNOWN"
      )
      key_action_ui <- if (!is.na(result$llm$key_action) &&
                           trimws(result$llm$key_action) != "None") {
        div(class = "alert alert-secondary", style = "margin-top:8px;",
            strong("Key action: "), result$llm$key_action)
      }

      div(class = "result-card",
        h3(paste("Route to", winner$resort_name)),
        div(class = "subtitle", paste("Estimated drive:", drive_str)),
        flag_ui,
        div(style = "margin:12px 0;",
          span(risk_label, style = sprintf(
            "background:%s;color:white;padding:6px 16px;border-radius:4px;
             font-weight:bold;font-size:15px;letter-spacing:0.5px;", badge_color))
        ),
        p(result$llm$summary),
        key_action_ui
      )
    } else {
      no_llm_msg <- if (nchar(GROQ_KEY) == 0) {
        div(class = "alert alert-warning",
            "Add ", code("GROQ_API_KEY"), " to .env for AI-powered road condition summaries.")
      } else {
        p(class = "text-muted",
          "Road condition summary unavailable. Check ",
          a("CoTrip", href = "https://cotrip.org", target = "_blank"),
          " or call 511.")
      }
      div(class = "result-card",
        h3(paste("Route to", winner$resort_name)),
        div(class = "subtitle", paste("Estimated drive:", drive_str)),
        flag_ui, no_llm_msg
      )
    }
  })


  # ── Resort results card ──────────────────────────────────────────────────────
  output$results_card <- renderUI({
    req(pipeline_result())
    result <- pipeline_result()
    req(!is.null(result$stage1_df))

    ability_label <- c(
      beginner = "Beginner", intermediate = "Intermediate",
      advanced = "Advanced", expert = "Expert"
    )[[input$ability_level]]

    subtitle <- paste0(ability_label, " · ", format(input$trip_date, "%B %d, %Y"))
    if (!is.null(result$reranked)) {
      subtitle <- paste0(subtitle, " · reranked by drive time from ", input$start_location)
    }

    div(class = "result-card",
      h3("Top 5 Recommended Resorts"),
      div(class = "subtitle", subtitle),
      tableOutput("results_table")
    )
  })

  output$results_table <- renderTable({
    req(pipeline_result())
    result <- pipeline_result()
    req(!is.null(result$stage1_df))

    df <- result$stage1_df |>
      filter(!is.na(composite_score)) |>
      slice_head(n = 5)

    # If reranked, annotate the top 3 with drive times
    if (!is.null(result$reranked)) {
      drive_lookup <- result$reranked$ranking |>
        transmute(resort_name,
                  drive_str = if_else(is.na(duration_mins), "—",
                                      sprintf("%.0f min", duration_mins)))
      df <- df |> left_join(drive_lookup, by = "resort_name")
    } else {
      df <- df |> mutate(drive_str = "—")
    }

    df |>
      mutate(
        Resort    = resort_name,
        Score     = sprintf("%.3f", composite_score),
        Weather   = ifelse(is.na(weather_score), "—", sprintf("%.2f", weather_score)),
        Terrain   = ifelse(is.na(terrain_score), "—", sprintf("%.2f", terrain_score)),
        Lifts     = ifelse(is.na(lift_pct), "—", paste0(round(lift_pct * 100), "%")),
        Avalanche = ifelse(!avalanche_applied, "—", coalesce(avalanche_danger, "—")),
        `Drive Time` = drive_str
      ) |>
      select(Resort, Score, Weather, Terrain, Lifts, Avalanche, `Drive Time`)

  }, striped = FALSE, hover = FALSE, bordered = FALSE, na = "—",
     width = "100%", align = "lrrrrrl")


  # ── Warnings panel ───────────────────────────────────────────────────────────
  output$warnings_panel <- renderUI({
    req(pipeline_result())
    result <- pipeline_result()
    req(!is.null(result$stage1_df))

    df <- result$stage1_df |>
      filter(!is.na(composite_score)) |>
      slice_head(n = 5) |>
      filter(!is.na(warnings))

    if (nrow(df) == 0) return(NULL)

    div(class = "warnings-card",
      h4("Conditions to Note"),
      tags$ul(
        lapply(seq_len(nrow(df)), function(i) {
          tags$li(strong(df$resort_name[i]), ": ", df$warnings[i])
        })
      )
    )
  })


  # ── Avalanche card ───────────────────────────────────────────────────────────
  output$avalanche_card <- renderUI({
    req(pipeline_result())
    result <- pipeline_result()
    req(!is.null(result$stage1_df))

    df <- result$stage1_df
    if (!any(df$avalanche_applied, na.rm = TRUE)) return(NULL)

    div(class = "result-card",
      h3("Avalanche Conditions"),
      div(class = "subtitle", "Today's CAIC danger ratings for scored resorts"),
      tableOutput("avalanche_table")
    )
  })

  output$avalanche_table <- renderTable({
    req(pipeline_result())
    result <- pipeline_result()
    req(!is.null(result$stage1_df))

    df <- result$stage1_df
    if (!any(df$avalanche_applied, na.rm = TRUE)) return(NULL)

    df |>
      filter(avalanche_applied) |>
      arrange(desc(match(avalanche_danger,
                         c("Extreme", "High", "Considerable", "Moderate", "Low")))) |>
      select(Resort = resort_name, `Danger Rating` = avalanche_danger)

  }, striped = FALSE, hover = FALSE, bordered = FALSE, na = "—", width = "100%")
}


# =============================================================================
# RUN
# =============================================================================

shinyApp(ui = ui, server = server)
