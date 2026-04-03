# =============================================================================
# app.R — SkiSmart Shiny Application
# =============================================================================
#
# PIPELINE
#   Stage 1 — R/composite_score.R  : weather + terrain + avalanche → top resorts
#   Stage 2 — city coords → route → drive-time rerank → CDOT → Groq LLM summary
#
# TO RUN
#   Open in RStudio and click "Run App", or: shiny::runApp("app.R")
#   Working directory must be the project root.
# =============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(leaflet)
library(DT)
library(echarts4r)
library(tippy)
library(httr2)
library(lubridate)

source("R/composite_score.R")
source("R/load_env.R")
source("R/api_geocode.R")
source("R/api_routing.R")
source("R/api_cdot.R")
source("R/route_conditions.R")
source("R/llm_route_summary.R")
source("R/stage2_rerank.R")

resorts <- read.csv("data/resorts.csv", stringsAsFactors = FALSE)
cities  <- read.csv("data/cities.csv",  stringsAsFactors = FALSE)

GH_KEY   <- Sys.getenv("GRAPHHOPPER_API_KEY")
CDOT_KEY <- Sys.getenv("CDOT_API_KEY")
GROQ_KEY <- Sys.getenv("GROQ_API_KEY")


# =============================================================================
# HELPERS
# =============================================================================

risk_color <- function(risk) {
  dplyr::case_when(
    risk == "do_not_travel" ~ "#7B2D2D",
    risk == "high"          ~ "#dc3545",
    risk == "moderate"      ~ "#fd7e14",
    TRUE                    ~ "#888888"
  )
}

color_route_segments <- function(waypoints, conditions, buffer_deg = 0.05) {
  priority <- c("clear" = 0, "low" = 1, "moderate" = 2, "high" = 3, "do_not_travel" = 4)
  point_risks <- vapply(seq_len(nrow(waypoints)), function(i) {
    dists  <- sqrt((conditions$lat - waypoints$lat[i])^2 +
                   (conditions$lon - waypoints$lon[i])^2)
    nearby <- conditions$row_risk[dists <= buffer_deg]
    if (length(nearby) == 0) return("clear")
    nearby[which.max(priority[nearby])]
  }, character(1))
  segments <- list(); i <- 1; n <- length(point_risks)
  while (i <= n) {
    j <- i
    while (j < n && point_risks[j + 1] == point_risks[j]) j <- j + 1
    end <- min(j + 1, n)
    segments[[length(segments) + 1]] <- list(
      lats = waypoints$lat[i:end], lons = waypoints$lon[i:end], risk = point_risks[i]
    )
    i <- j + 1
  }
  segments
}

score_label <- function(s) {
  dplyr::case_when(
    s >= 0.70 ~ "Excellent",
    s >= 0.55 ~ "Good",
    s >= 0.40 ~ "Fair",
    s >= 0.25 ~ "Poor",
    TRUE      ~ "Not recommended"
  )
}

score_color <- function(s) {
  dplyr::case_when(
    s >= 0.70 ~ "#2E7D32",
    s >= 0.55 ~ "#558B2F",
    s >= 0.40 ~ "#F57F17",
    s >= 0.25 ~ "#E65100",
    TRUE      ~ "#B71C1C"
  )
}

# Fetches hourly ski-day weather for the sparkline and snapshot card.
# Returns a list: $hourly (data frame) + $depth_in + $snowfall_72hr_in + $avg_temp_f
fetch_resort_display_data <- function(lat, lon, ski_date) {
  tryCatch({
    resp <- httr2::request("https://api.open-meteo.com/v1/forecast") |>
      httr2::req_url_query(
        latitude      = round(lat, 6),
        longitude     = round(lon, 6),
        hourly        = "temperature_2m,snowfall,snow_depth",
        past_days     = 3,
        forecast_days = 10,
        timezone      = "auto"
      ) |>
      httr2::req_perform()

    if (httr2::resp_status(resp) != 200) return(NULL)

    body   <- httr2::resp_body_json(resp, simplifyVector = TRUE)
    hourly <- as.data.frame(body$hourly) |>
      dplyr::mutate(
        datetime = lubridate::ymd_hm(time, tz = body$timezone),
        date     = as.Date(datetime, tz = body$timezone),
        hour     = lubridate::hour(datetime),
        temp_f   = temperature_2m * 9 / 5 + 32,
        snow_in  = snowfall * 0.393701,
        depth_in = snow_depth * 39.3701
      )

    ski_h <- hourly |>
      dplyr::filter(date == as.Date(ski_date), hour >= 8, hour <= 16) |>
      dplyr::mutate(hour_label = dplyr::case_when(
        hour == 12 ~ "12pm",
        hour >  12 ~ paste0(hour - 12, "pm"),
        TRUE       ~ paste0(hour, "am")
      )) |>
      dplyr::select(hour, hour_label, temp_f, snow_in)

    # Snow depth at midday
    depth_row <- hourly |>
      dplyr::filter(date == as.Date(ski_date), hour == 12) |>
      dplyr::slice(1)
    if (nrow(depth_row) == 0)
      depth_row <- hourly |> dplyr::filter(date == as.Date(ski_date)) |> dplyr::slice(1)
    depth_in <- if (nrow(depth_row) > 0) depth_row$depth_in[1] else NA_real_

    # 72-hour snowfall accumulation
    ski_dt <- as.POSIXct(as.Date(ski_date), tz = body$timezone)
    snowfall_72hr <- hourly |>
      dplyr::filter(datetime >= ski_dt - 72 * 3600, datetime < ski_dt) |>
      dplyr::summarise(total = sum(snow_in, na.rm = TRUE)) |>
      dplyr::pull(total)

    avg_temp <- if (nrow(ski_h) > 0) mean(ski_h$temp_f, na.rm = TRUE) else NA_real_

    list(
      hourly           = if (nrow(ski_h) > 0) ski_h else NULL,
      depth_in         = round(depth_in,     0),
      snowfall_72hr_in = round(snowfall_72hr, 1),
      avg_temp_f       = round(avg_temp,      0)
    )
  }, error = function(e) NULL)
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
  }

  body {
    background-color: var(--snow);
    font-family: 'Segoe UI', system-ui, -apple-system, sans-serif;
    color: var(--text);
  }

  /* ── Header ───────────────────────────────────────────────────────────── */
  .skismart-header {
    background: linear-gradient(135deg, var(--navy) 0%, var(--blue) 100%);
    color: white;
    padding: 24px 28px 16px 28px;
    margin: -15px -15px 24px -15px;
    display: flex;
    align-items: center;
    justify-content: flex-start;
    gap: 16px;
  }
  .sk-header-brand {
    display: flex;
    align-items: center;
    gap: 14px;
  }
  .skismart-header h1 {
    margin: 0;
    font-size: 30px;
    font-weight: 700;
    letter-spacing: -0.5px;
    color: white;
    line-height: 1.1;
  }
  .skismart-header .sk-tagline {
    margin: 3px 0 0 0;
    font-size: 12px;
    color: rgba(255,255,255,0.70);
    font-style: italic;
    letter-spacing: 0.1px;
  }
  .sk-live-badge {
    margin-left: auto;
    font-size: 11px;
    color: rgba(255,255,255,0.65);
    background: rgba(255,255,255,0.12);
    padding: 4px 11px;
    border-radius: 20px;
    white-space: nowrap;
  }

  /* ── Sidebar ──────────────────────────────────────────────────────────── */
  .well {
    background: var(--white) !important;
    border: 1px solid var(--border) !important;
    border-radius: 10px !important;
    box-shadow: 0 2px 8px rgba(27,58,92,0.07) !important;
    padding: 20px !important;
  }
  .well h4 {
    color: var(--navy);
    font-size: 12px;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: 0.8px;
    margin-bottom: 12px;
  }
  .well hr { border-color: var(--border); margin: 14px 0; }
  .well .help-block { color: var(--muted); font-size: 11px; line-height: 1.5; }

  label {
    font-size: 11px;
    font-weight: 600;
    color: var(--muted);
    text-transform: uppercase;
    letter-spacing: 0.5px;
  }
  .form-control, .selectize-input {
    border-color: var(--border) !important;
    border-radius: 6px !important;
    font-size: 13px !important;
  }
  .form-control:focus {
    border-color: var(--blue-light) !important;
    box-shadow: 0 0 0 2px rgba(91,164,207,0.2) !important;
  }

  #find_resorts {
    background: linear-gradient(135deg, var(--blue) 0%, var(--blue-light) 100%);
    border: none;
    border-radius: 8px;
    font-size: 13px;
    font-weight: 600;
    letter-spacing: 0.4px;
    padding: 10px;
    box-shadow: 0 3px 10px rgba(46,109,164,0.35);
    transition: opacity 0.15s;
  }
  #find_resorts:hover { opacity: 0.88; }

  /* ── Cards ────────────────────────────────────────────────────────────── */
  .result-card {
    background: var(--white);
    border: 1px solid var(--border);
    border-radius: 10px;
    padding: 20px 22px;
    margin-bottom: 18px;
    box-shadow: 0 2px 8px rgba(27,58,92,0.06);
  }
  .result-card h3 {
    color: var(--navy);
    font-size: 17px;
    font-weight: 700;
    margin: 0 0 4px 0;
  }
  .result-card .subtitle { color: var(--muted); font-size: 13px; margin-bottom: 14px; }

  /* ── Section labels ───────────────────────────────────────────────────── */
  .sk-section-label {
    font-size: 11px;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: 1.2px;
    color: var(--muted);
    margin-bottom: 6px;
    margin-top: 6px;
  }

  /* ── Focus card ───────────────────────────────────────────────────────── */
  .sk-focus-header {
    display: flex;
    justify-content: space-between;
    align-items: flex-start;
    margin-bottom: 16px;
  }
  .sk-resort-name {
    font-size: 26px !important;
    font-weight: 700 !important;
    color: var(--navy) !important;
    margin: 0 0 3px 0 !important;
  }
  .sk-score-badge {
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    padding: 10px 20px;
    border-radius: 8px;
    color: white;
    min-width: 88px;
    flex-shrink: 0;
    margin-left: 16px;
  }
  .sk-score-num { font-size: 24px; font-weight: 700; line-height: 1; }
  .sk-score-lbl { font-size: 12px; font-weight: 600; margin-top: 4px; opacity: 0.92; }

  .sk-panel-label {
    font-size: 10px;
    font-weight: 600;
    text-transform: uppercase;
    letter-spacing: 0.6px;
    color: var(--muted);
    margin-bottom: 6px;
  }
  .sk-route-stats {
    font-size: 11px;
    color: var(--muted);
    margin-top: 6px;
    text-align: center;
  }
  /* ── Comparison table ─────────────────────────────────────────────────── */
  table.dataTable {
    font-size: 14px !important;
    border: none !important;
    width: 100% !important;
  }
  table.dataTable thead th {
    background-color: var(--blue-pale) !important;
    color: var(--navy) !important;
    font-size: 12px !important;
    font-weight: 700 !important;
    text-transform: uppercase !important;
    letter-spacing: 0.6px !important;
    padding: 11px 14px !important;
    border-bottom: 2px solid var(--border) !important;
    cursor: default;
  }
  table.dataTable tbody td {
    padding: 12px 14px !important;
    border-bottom: 1px solid var(--border) !important;
    vertical-align: middle !important;
    font-size: 14px !important;
  }
  table.dataTable tbody tr:last-child td { border-bottom: none !important; }
  table.dataTable tbody tr { cursor: pointer; }
  table.dataTable tbody tr:hover td { background-color: var(--blue-pale) !important; }
  table.dataTable tbody tr.selected td {
    background-color: var(--blue-pale) !important;
    color: var(--text) !important;
    box-shadow: none !important;
  }
  table.dataTable tbody tr.selected td:first-child {
    border-left: 3px solid var(--blue) !important;
  }
  .dataTables_wrapper { padding: 0 !important; }
  .dataTables_wrapper .dataTables_info,
  .dataTables_wrapper .dataTables_paginate { display: none !important; }

  /* ── Warnings card ────────────────────────────────────────────────────── */
  .warnings-card {
    background: #FFFBEA;
    border: 1px solid #F9E08A;
    border-left: 4px solid #F0B429;
    border-radius: 8px;
    padding: 13px 17px;
    margin-bottom: 18px;
  }
  .warnings-card h4 {
    color: #7D5A00; font-size: 12px; font-weight: 700;
    text-transform: uppercase; letter-spacing: 0.6px; margin: 0 0 9px 0;
  }
  .warnings-card ul { margin: 0; padding-left: 17px; font-size: 13px; color: #5A4000; line-height: 1.8; }

  /* ── Avalanche note ───────────────────────────────────────────────────── */
  .sk-avy-note {
    font-size: 12px;
    color: var(--muted);
    margin-bottom: 12px;
    line-height: 1.5;
  }
  .sk-avy-note a { color: var(--blue); text-decoration: underline; }

  /* ── Snapshot card ────────────────────────────────────────────────────── */
  .sk-snapshot-grid {
    display: grid;
    grid-template-columns: 1fr 1fr;
    gap: 16px 12px;
    margin-top: 10px;
  }
  .sk-snap-cell { text-align: center; }
  .sk-snap-value { font-size: 26px; font-weight: 700; color: var(--navy); line-height: 1; }
  .sk-snap-label { font-size: 11px; color: var(--muted); margin-top: 4px; line-height: 1.3; }

  /* ── Welcome banner ───────────────────────────────────────────────────── */
  .welcome-banner {
    background: var(--blue-pale);
    border: 1px solid var(--border);
    border-left: 4px solid var(--blue-light);
    border-radius: 8px;
    padding: 17px 20px;
    color: var(--navy);
    font-size: 14px;
    margin-bottom: 16px;
  }
  .welcome-banner strong { color: var(--blue); }

  /* ── Tables (base) ────────────────────────────────────────────────────── */
  .table {
    font-size: 13px; border-collapse: separate; border-spacing: 0; width: 100%;
  }
  .table thead th {
    background-color: var(--blue-pale); color: var(--navy);
    font-size: 11px; font-weight: 700; text-transform: uppercase;
    letter-spacing: 0.6px; padding: 9px 12px; border-bottom: 2px solid var(--border);
  }
  .table tbody tr:hover { background-color: var(--blue-pale); }
  .table tbody td { padding: 9px 12px; border-bottom: 1px solid var(--border); vertical-align: middle; }
  .table tbody tr:last-child td { border-bottom: none; }

  /* ── Score interpretation bullets ────────────────────────────────────── */
  .sk-score-interp {
    margin-top: 16px;
    border-top: 1px solid var(--border);
    padding-top: 14px;
  }
  .sk-interp-header {
    font-size: 10px;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: 1px;
    color: var(--muted);
    margin-bottom: 10px;
  }
  .sk-interp-item {
    display: flex;
    align-items: flex-start;
    gap: 12px;
    background: var(--snow);
    border-left: 3px solid var(--blue);
    border-radius: 0 6px 6px 0;
    padding: 10px 14px;
    margin-bottom: 8px;
  }
  .sk-interp-icon { font-size: 18px; flex-shrink: 0; line-height: 1.3; }
  .sk-interp-label {
    font-size: 12px;
    font-weight: 700;
    color: var(--navy);
    display: block;
    margin-bottom: 2px;
  }
  .sk-interp-text { font-size: 13px; color: var(--text); line-height: 1.5; }
  .sk-interp-loading {
    font-size: 12px;
    color: var(--muted);
    font-style: italic;
    padding: 10px 0 4px 0;
  }

  /* ── Footer ───────────────────────────────────────────────────────────── */
  .data-footer {
    font-size: 11px; color: var(--muted); text-align: center;
    padding: 11px 0 4px 0; border-top: 1px solid var(--border); margin-top: 20px;
  }
"

mountain_logo <- tags$svg(
  xmlns = "http://www.w3.org/2000/svg",
  width = "44", height = "35", viewBox = "0 0 48 38",
  tags$polygon(points = "24,2 46,36 2,36",  fill = "rgba(255,255,255,0.25)"),
  tags$polygon(points = "24,2 31,14 17,14", fill = "white"),
  tags$polygon(points = "36,12 46,36 26,36", fill = "rgba(255,255,255,0.18)")
)


# =============================================================================
# UI
# =============================================================================

ui <- fluidPage(

  theme = bslib::bs_theme(
    version   = 5,
    primary   = "#2E6DA4",
    secondary = "#5BA4CF",
    bg        = "#F4F8FC",
    fg        = "#1A2B3C"
  ),

  tags$head(
    tags$style(HTML(skismart_css)),
    use_tippy()
  ),

  # ── Header ──────────────────────────────────────────────────────────────────
  div(class = "skismart-header",
    div(class = "sk-header-brand",
      mountain_logo,
      div(
        tags$h1("SkiSmart"),
        tags$p(class = "sk-tagline",
          "Because finding the right place to ski shouldn't take longer than the drive there.")
      )
    ),
    textOutput("live_time", inline = TRUE) |>
      tagAppendAttributes(class = "sk-live-badge")
  ),

  sidebarLayout(

    # ── Sidebar ─────────────────────────────────────────────────────────────
    sidebarPanel(
      width = 3,

      h4("Trip Details"),

      selectizeInput(
        inputId  = "user_city",
        label    = "Starting City",
        choices  = sort(cities$city_label),
        selected = "Denver, CO",
        options  = list(placeholder = "Search city...")
      ),

      dateInput(
        inputId = "trip_date",
        label   = "Date of Trip",
        value   = Sys.Date() + 1,
        min     = Sys.Date(),
        max     = Sys.Date() + 10,
        format  = "mm/dd/yyyy"
      ),

      hr(),
      h4("Skier Ability"),

      selectInput(
        inputId  = "ability_level",
        label    = NULL,
        choices  = c(
          "Beginner"     = "beginner",
          "Intermediate" = "intermediate",
          "Advanced"     = "advanced",
          "Expert"       = "expert"
        ),
        selected = "intermediate"
      ),

      sliderInput(
        inputId = "max_drive_hours",
        label   = "Max drive time (hours)",
        min     = 1, max = 12, value = 4, step = 0.5,
        ticks   = FALSE
      ),

      hr(),
      actionButton(
        inputId = "find_resorts",
        label   = "Find Resorts",
        class   = "btn-primary w-100"
      ),

      br(), br(),
      helpText(
        tags$small(
          "Weather: Open-Meteo (10-day forecast).", tags$br(),
          "Avalanche: CAIC (today & tomorrow only).", tags$br(),
          "Roads: Colorado DOT 511.", tags$br(),
          "33 Colorado resorts · Season dates applied."
        )
      )
    ),

    # ── Main panel ────────────────────────────────────────────────────────────
    mainPanel(
      width = 9,

      uiOutput("status_message"),

      # Top recommendation focus card
      uiOutput("focus_card"),

      # Comparison table
      uiOutput("comparison_section"),

      # Bottom row: avalanche + resort snapshot
      uiOutput("bottom_row"),

      div(class = "data-footer",
        "Data sources: Open-Meteo · Colorado Avalanche Information Center (CAIC) · Colorado DOT 511 · Static resort database"
      )
    )
  )
)


# =============================================================================
# SERVER  — pipeline logic unchanged; new reactives added for UI only
# =============================================================================

server <- function(input, output, session) {

  # ── Live timestamp ───────────────────────────────────────────────────────────
  live_time_val <- reactiveVal(format(Sys.time(), "%I:%M %p"))
  observe({
    invalidateLater(60000)
    live_time_val(format(Sys.time(), "%I:%M %p"))
  })
  output$live_time <- renderText(paste("Live data ·", trimws(live_time_val())))

  # ── Selected resort index (1 = top resort) ───────────────────────────────────
  selected_idx <- reactiveVal(1)

  observeEvent(input$resorts_table_rows_selected, {
    idx <- input$resorts_table_rows_selected
    if (length(idx) == 0) return()
    selected_idx(idx[1])
  }, ignoreNULL = TRUE)

  # ── City coordinates ─────────────────────────────────────────────────────────
  user_city_data <- reactive({
    cities |> dplyr::filter(city_label == input$user_city)
  })

  # ── Stage 1 + 2 pipeline ─────────────────────────────────────────────────────
  pipeline_result <- eventReactive(input$find_resorts, {

    # Reset focus to top resort for new run
    selected_idx(1)

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

    if (is.null(stage1_df) || nrow(stage1_df) == 0)
      return(list(error = "No resorts available for that date."))

    # --- Start location from city dropdown (no geocoding needed) ---------------
    city <- user_city_data()
    if (nrow(city) == 0)
      return(list(stage1_df = stage1_df, geo_error = "Selected city not found."))

    start_lat     <- city$latitude[1]
    start_lon     <- city$longitude[1]
    start_label   <- input$user_city
    user_max_mins <- input$max_drive_hours * 60

    # --- Stage 2: drive-time reranking ----------------------------------------
    showNotification("Fetching drive times...", type = "message",
                     duration = NULL, id = "loading_route")

    top5_stage2 <- stage1_df |>
      dplyr::filter(!is.na(composite_score)) |>
      dplyr::slice_head(n = 5) |>
      dplyr::left_join(resorts[, c("resort_name", "latitude", "longitude")],
                       by = "resort_name") |>
      dplyr::transmute(
        resort_name      = resort_name,
        lat              = latitude,
        lon              = longitude,
        conditions_score = composite_score
      )

    reranked <- tryCatch(
      rerank_with_drive_time(top5_stage2, start_lat, start_lon, user_max_mins, GH_KEY),
      error = function(e) {
        showNotification(paste("Routing error:", e$message), type = "warning", duration = 6)
        NULL
      }
    )
    removeNotification("loading_route")

    # --- Determine winner ---
    # Stage 2 winner if within limit; NULL if all exceed; Stage 1 fallback if routing failed
    if (!is.null(reranked) && !is.null(reranked$winner)) {
      winner <- list(
        resort_name = reranked$winner$resort_name,
        lat         = reranked$winner$lat,
        lon         = reranked$winner$lon
      )
    } else if (!is.null(reranked) && is.null(reranked$winner)) {
      winner <- NULL   # all resorts exceed drive time limit
    } else {
      top1 <- stage1_df |>
        dplyr::filter(!is.na(composite_score)) |>
        dplyr::slice_head(n = 1) |>
        dplyr::left_join(resorts[, c("resort_name", "latitude", "longitude")],
                         by = "resort_name")
      winner <- list(
        resort_name = top1$resort_name[1],
        lat         = top1$latitude[1],
        lon         = top1$longitude[1]
      )
    }

    # --- Fetch winner route ---------------------------------------------------
    winner_route <- NULL
    if (!is.null(winner)) {
      showNotification("Fetching route to recommended resort...", type = "message",
                       duration = NULL, id = "loading_wr")
      winner_route <- tryCatch(
        fetch_route(start_lat, start_lon, winner$lat, winner$lon, GH_KEY),
        error = function(e) NULL
      )
      removeNotification("loading_wr")
    }

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
      winner       = winner,
      winner_route = winner_route,
      route_prep   = route_prep,
      llm          = llm_out,
      start_lat    = start_lat,
      start_lon    = start_lon,
      start_label  = start_label
    )
  })

  # ── Top 5 resorts with resort metadata joined ─────────────────────────────────
  top5_joined <- reactive({
    result <- pipeline_result()
    if (is.null(result$stage1_df)) return(NULL)
    result$stage1_df |>
      dplyr::filter(!is.na(composite_score)) |>
      dplyr::slice_head(n = 5) |>
      dplyr::left_join(
        resorts[, c("resort_name", "latitude", "longitude",
                    "elevation_base_ft", "trails_total")],
        by = "resort_name"
      )
  })

  # ── After pipeline runs, focus the Stage 2 winner ────────────────────────────
  observeEvent(pipeline_result(), {
    result <- pipeline_result()
    selected_idx(1)
    if (is.null(result$winner)) return()
    df <- top5_joined()
    if (is.null(df)) return()
    idx <- which(df$resort_name == result$winner$resort_name)
    if (length(idx) > 0) selected_idx(idx[1])
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # ── Currently focused resort row ─────────────────────────────────────────────
  selected_resort <- reactive({
    df <- top5_joined()
    if (is.null(df)) return(NULL)
    df[min(selected_idx(), nrow(df)), ]
  })

  # ── Display weather for selected resort (sparkline + snapshot) ───────────────
  resort_display <- reactive({
    r <- selected_resort()
    if (is.null(r)) return(NULL)
    fetch_resort_display_data(r$latitude, r$longitude, input$trip_date)
  })

  # ── Route for the currently focused resort ───────────────────────────────────
  # Uses cached winner_route for the winner (no extra API call).
  # On-demand fetch for other table rows the user clicks.
  focus_route <- reactive({
    result <- pipeline_result()
    r      <- selected_resort()
    city   <- user_city_data()
    if (is.null(result) || is.null(result$stage1_df) || is.null(r) || nrow(city) == 0)
      return(NULL)
    if (is.na(r$latitude) || is.na(r$longitude)) return(NULL)
    if (!is.null(result$winner) && r$resort_name == result$winner$resort_name)
      return(result$winner_route)
    tryCatch(
      fetch_route(city$latitude[1], city$longitude[1], r$latitude, r$longitude, GH_KEY),
      error = function(e) NULL
    )
  })


  # ── Welcome / status message ─────────────────────────────────────────────────
  output$status_message <- renderUI({
    if (!isTruthy(pipeline_result())) {
      div(class = "welcome-banner",
        "Select your ", strong("starting city"), ", ", strong("trip date"),
        ", and ", strong("ability level"), ", then click ", strong("Find Resorts"), ".",
        tags$br(), tags$br(),
        tags$small(style = "color:#6B7C8F;",
          "Scores combine weather quality (50%) and terrain accessibility (50%),
           adjusted for avalanche danger when available."
        )
      )
    } else if (!is.null(pipeline_result()$error)) {
      div(class = "alert alert-danger", pipeline_result()$error)
    }
  })


  # ── Focus card ───────────────────────────────────────────────────────────────
  output$focus_card <- renderUI({
    result <- pipeline_result()
    if (is.null(result) || !is.null(result$error) || is.null(result$stage1_df))
      return(NULL)

    # No resorts qualify within the user's drive time limit
    if (!is.null(result$reranked) &&
        result$reranked$drive_time_flag == "no_resorts_within_limit") {
      min_drive <- min(result$reranked$ranking$duration_mins, na.rm = TRUE)
      min_hrs   <- round(min_drive / 60, 1)
      return(
        div(
          div(class = "sk-section-label", "TOP RECOMMENDATION"),
          div(class = "warnings-card",
            h4("No resorts within your drive time preference"),
            tags$p(sprintf(
              "The closest resort is approximately %.1f hours away. Try increasing your max drive time in the sidebar.",
              min_hrs
            ))
          )
        )
      )
    }

    r <- selected_resort()
    if (is.null(r)) return(NULL)

    # Drive-time string for selected resort
    drive_str <- "—"
    if (!is.null(result$reranked)) {
      rm <- result$reranked$ranking |>
        dplyr::filter(resort_name == r$resort_name)
      if (nrow(rm) > 0 && !is.na(rm$duration_mins[1]))
        drive_str <- sprintf("%.0f min · %.0f mi",
                             rm$duration_mins[1], rm$distance_miles[1])
    }

    # Road conditions summary now rendered inside score_interp_section bullets

    # Score badge
    s   <- r$composite_score
    lbl <- score_label(s)
    col <- score_color(s)

    # Resort subtitle (elevation + trails)
    parts <- c()
    if (!is.na(r$elevation_base_ft))
      parts <- c(parts, paste0(formatC(r$elevation_base_ft, format = "d",
                                       big.mark = ","), " ft base"))
    if (!is.na(r$trails_total))
      parts <- c(parts, paste0(r$trails_total, " trails"))

    div(
      div(class = "sk-section-label", "TOP RECOMMENDATION"),
      div(class = "result-card",

        # Resort name + score badge
        div(class = "sk-focus-header",
          div(
            tags$h3(class = "sk-resort-name", r$resort_name),
            if (length(parts) > 0)
              div(class = "subtitle", paste(parts, collapse = " · "))
          ),
          div(class = "sk-score-badge",
            style = paste0("background:", col, ";"),
            div(class = "sk-score-num", sprintf("%.2f", s)),
            div(class = "sk-score-lbl", lbl)
          )
        ),

        # Two-column: route map | weather sparkline
        fluidRow(
          column(6,
            div(class = "sk-panel-label",
              paste("Route from", input$user_city)
            ),
            leafletOutput("focus_map", height = "230px"),
            div(class = "sk-route-stats", drive_str)
          ),
          column(6,
            div(class = "sk-panel-label",
              paste("Ski day forecast ·", format(input$trip_date, "%b %d"))
            ),
            echarts4rOutput("weather_spark", height = "230px")
          )
        ),

        # Score interpretation + road conditions bullets (loads after Groq call)
        uiOutput("score_interp_section")
      )
    )
  })

  # ── Focus map — fully reactive renderLeaflet (no leafletProxy) ───────────────
  # Depends on focus_route() and selected_resort() so it re-draws whenever the
  # user selects a different resort or the pipeline produces new results.
  # This avoids the leafletProxy timing bug where proxy commands are dropped
  # because the uiOutput containing leafletOutput re-renders simultaneously.
  output$focus_map <- renderLeaflet({
    result <- pipeline_result()
    r      <- selected_resort()
    city   <- user_city_data()

    m <- leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = -106.0, lat = 39.5, zoom = 7)

    if (is.null(result) || is.null(result$stage1_df) || is.null(r) || nrow(city) == 0)
      return(m)
    if (is.null(result$winner)) return(m)

    route <- focus_route()

    # Start pin
    m <- m |> addCircleMarkers(
      lng = city$longitude[1], lat = city$latitude[1],
      radius = 8, color = "#333", fillColor = "#333", fillOpacity = 1,
      popup = paste0("<b>Start:</b> ", result$start_label),
      label = result$start_label
    )

    # Route polyline
    if (!is.null(route) && !is.null(route$waypoints) && nrow(route$waypoints) >= 2) {
      wp <- route$waypoints

      # CDOT risk colouring only available for the winner's route
      has_cdot <- !is.null(result$route_prep) &&
                  nrow(result$route_prep$filtered_df) > 0 &&
                  r$resort_name == result$winner$resort_name

      if (has_cdot) {
        conds <- result$route_prep$filtered_df |>
          dplyr::distinct(segment_id, condition_id, .keep_all = TRUE) |>
          dplyr::transmute(lat = start_lat, lon = start_lon, row_risk = row_risk)

        for (seg in color_route_segments(wp, conds)) {
          m <- m |> addPolylines(
            lng     = seg$lons,
            lat     = seg$lats,
            color   = risk_color(seg$risk),
            weight  = if (seg$risk %in% c("high", "do_not_travel")) 6 else 4,
            opacity = 0.85
          )
        }
        m <- m |> addLegend(
          position = "bottomright",
          colors   = c("#2E6DA4", "#fd7e14", "#dc3545", "#7B2D2D"),
          labels   = c("Clear", "Moderate risk", "High risk", "Do not travel"),
          opacity  = 0.85, title = "Road conditions"
        )
      } else {
        m <- m |> addPolylines(
          lng = wp$lon, lat = wp$lat,
          color = "#2E6DA4", weight = 4, opacity = 0.8
        )
      }
    }

    # Resort pin
    m <- m |> addCircleMarkers(
      lng = r$longitude, lat = r$latitude,
      radius = 14, color = "#1B3A5C", fillColor = "#2E6DA4",
      fillOpacity = 0.9, weight = 2,
      popup = paste0("<b>", r$resort_name, "</b>"),
      label = r$resort_name
    )

    m
  })


  # ── Weather sparkline ─────────────────────────────────────────────────────────
  output$weather_spark <- renderEcharts4r({
    disp <- resort_display()
    hrly <- if (!is.null(disp)) disp$hourly else NULL

    if (is.null(hrly) || nrow(hrly) == 0) {
      return(
        data.frame(x = "—", y = 0) |>
          e_charts(x) |>
          e_scatter(y, symbolSize = 0) |>
          e_title(subtext = "Weather data unavailable",
                  subtextStyle = list(color = "#6B7C8F", fontSize = 12)) |>
          e_grid(top = 10)
      )
    }

    hrly |>
      e_charts(hour_label) |>
      e_bar(snow_in, name = "Snow (in/hr)",
            itemStyle = list(color = "#5BA4CF"),
            barMaxWidth = 22) |>
      e_line(temp_f, name = "Temp (°F)",
             y_index = 1,
             lineStyle = list(color = "#E65100", width = 2),
             itemStyle = list(color = "#E65100"),
             symbol = "circle", symbolSize = 5) |>
      e_y_axis(index = 0, name = "in/hr", min = 0,
               nameTextStyle = list(fontSize = 10, color = "#5BA4CF"),
               axisLabel = list(fontSize = 9),
               splitLine = list(show = FALSE)) |>
      e_y_axis(index = 1, name = "°F", position = "right",
               nameTextStyle = list(fontSize = 10, color = "#E65100"),
               axisLabel = list(fontSize = 9),
               splitLine = list(show = FALSE)) |>
      e_x_axis(axisLabel = list(fontSize = 10),
               splitLine = list(show = FALSE)) |>
      e_tooltip(trigger = "axis") |>
      e_legend(bottom = 0, textStyle = list(fontSize = 10)) |>
      e_grid(top = 8, bottom = 44, left = 48, right = 52)
  })


  # ── Comparison table ──────────────────────────────────────────────────────────
  output$comparison_section <- renderUI({
    result <- pipeline_result()
    if (is.null(result) || !is.null(result$error) || is.null(result$stage1_df))
      return(NULL)

    div(
      div(class = "sk-section-label", "ALL RESORTS — CLICK TO EXPLORE"),
      div(class = "result-card", DT::dataTableOutput("resorts_table"))
    )
  })

  output$resorts_table <- DT::renderDataTable({
    df <- top5_joined()
    if (is.null(df)) return(NULL)

    avy_html <- function(applied, danger) {
      if (!applied) return("<span style='color:#aaa;font-size:11px'>N/A</span>")
      if (is.na(danger) || danger == "") return("—")
      col <- switch(danger,
        "Low"          = "#2E7D32",
        "Moderate"     = "#F57F17",
        "Considerable" = "#E65100",
        "High"         = "#B71C1C",
        "Extreme"      = "#6A1B9A",
        "#888"
      )
      sprintf('<span style="color:%s;font-weight:600;font-size:12px">%s</span>',
              col, danger)
    }

    display <- df |>
      dplyr::mutate(
        `#`            = seq_len(dplyr::n()),
        Resort         = resort_name,
        Score          = paste0(
          sprintf("%.2f", composite_score),
          sprintf(' <span style="color:%s;font-size:11px;margin-left:3px">%s</span>',
                  score_color(composite_score), score_label(composite_score))
        ),
        Weather        = ifelse(is.na(weather_score), "—",
                                sprintf("%.2f", weather_score)),
        `Terrain open` = paste0("~", round(terrain_score * 100), "% open for ",
                                input$ability_level, "s"),
        Lifts          = ifelse(is.na(lift_pct), "—",
                                paste0(round(lift_pct * 100), "%")),
        Avalanche      = mapply(avy_html, avalanche_applied, avalanche_danger,
                                SIMPLIFY = TRUE)
      ) |>
      dplyr::select(`#`, Resort, Score, Weather, `Terrain open`, Lifts, Avalanche)

    # Custom header container with tippy tooltips
    header <- htmltools::withTags(table(
      class = "display",
      thead(tr(
        tags$th("#"),
        tags$th(`data-tippy-content` = "Resort name", "Resort"),
        tags$th(
          `data-tippy-content` = "Combined score (0–1): weather quality 50% + terrain accessibility 50%, adjusted for avalanche danger",
          "Score"
        ),
        tags$th(
          `data-tippy-content` = "Weather quality score: snow depth, fresh snowfall (72h), temperature, and wind speed",
          "Weather"
        ),
        tags$th(
          `data-tippy-content` = "Estimated % of preferred terrain open for your ability level, based on snow depth and wind",
          "Terrain open"
        ),
        tags$th(
          `data-tippy-content` = "Estimated % of lifts operating based on current wind speed",
          "Lifts"
        ),
        tags$th(
          `data-tippy-content` = "CAIC avalanche danger rating. Available for today and tomorrow only.",
          "Avalanche"
        )
      ))
    ))

    DT::datatable(
      display,
      container  = header,
      escape     = FALSE,
      rownames   = FALSE,
      selection  = list(mode = "single", target = "row", selected = selected_idx()),
      options    = list(
        dom        = "t",
        ordering   = FALSE,
        pageLength = 50,
        initComplete = DT::JS(
          "function(settings, json) {
             if (typeof tippy !== 'undefined') {
               tippy('[data-tippy-content]', { placement: 'top', theme: 'light' });
             }
           }"
        )
      )
    )
  }, server = FALSE)


  # ── Bottom row: avalanche + snapshot ─────────────────────────────────────────
  output$bottom_row <- renderUI({
    result <- pipeline_result()
    if (is.null(result) || !is.null(result$error) || is.null(result$stage1_df))
      return(NULL)
    fluidRow(
      column(6, uiOutput("avalanche_card")),
      column(6, uiOutput("snapshot_card"))
    )
  })


  # ── Avalanche card ────────────────────────────────────────────────────────────
  output$avalanche_card <- renderUI({
    result <- pipeline_result()
    if (is.null(result$stage1_df)) return(NULL)
    df <- result$stage1_df
    if (!any(df$avalanche_applied, na.rm = TRUE)) return(NULL)

    div(class = "result-card",
      h3("Avalanche conditions · CAIC"),
      div(class = "sk-avy-note",
        "Avalanche data is only available for today and tomorrow. For future dates,",
        " check the ",
        tags$a("CAIC website", href = "https://avalanche.state.co.us",
               target = "_blank"),
        " directly."
      ),
      tableOutput("avalanche_table")
    )
  })

  output$avalanche_table <- renderTable({
    result <- pipeline_result()
    if (is.null(result$stage1_df)) return(NULL)
    df <- result$stage1_df
    if (!any(df$avalanche_applied, na.rm = TRUE)) return(NULL)
    df |>
      dplyr::filter(avalanche_applied) |>
      dplyr::arrange(desc(match(avalanche_danger,
                                c("Extreme", "High", "Considerable", "Moderate", "Low")))) |>
      dplyr::select(Resort = resort_name, `Danger Rating` = avalanche_danger)
  }, striped = FALSE, hover = FALSE, bordered = FALSE, na = "—", width = "100%")


  # ── Resort snapshot card ──────────────────────────────────────────────────────
  output$snapshot_card <- renderUI({
    r    <- selected_resort()
    disp <- resort_display()
    if (is.null(r)) return(NULL)

    depth_str   <- if (!is.null(disp) && !is.na(disp$depth_in))
                     paste0(disp$depth_in, '"') else "—"
    newsnow_str <- if (!is.null(disp) && !is.na(disp$snowfall_72hr_in))
                     paste0(disp$snowfall_72hr_in, '"') else "—"
    temp_str    <- if (!is.null(disp) && !is.na(disp$avg_temp_f))
                     paste0(disp$avg_temp_f, "°F") else "—"
    terrain_str <- paste0("~", round(r$terrain_score * 100), "%")

    div(class = "result-card",
      h3(paste("Resort snapshot ·", r$resort_name)),
      div(class = "sk-snapshot-grid",
        div(class = "sk-snap-cell",
          div(class = "sk-snap-value", depth_str),
          div(class = "sk-snap-label", "Base depth")
        ),
        div(class = "sk-snap-cell",
          div(class = "sk-snap-value", newsnow_str),
          div(class = "sk-snap-label", "New snow (72h)")
        ),
        div(class = "sk-snap-cell",
          div(class = "sk-snap-value", terrain_str),
          div(class = "sk-snap-label",
            paste0("Open for ", input$ability_level, "s"))
        ),
        div(class = "sk-snap-cell",
          div(class = "sk-snap-value", temp_str),
          div(class = "sk-snap-label", "Ski-hours avg temp")
        )
      )
    )
  })


  # ── Score interpretation + road conditions (Groq call per selected resort) ────
  output$score_interp_section <- renderUI({
    result <- pipeline_result()
    r      <- selected_resort()
    if (is.null(result) || is.null(r)) return(NULL)

    # --- LLM score interpretation (weather / terrain / avalanche) ---
    interp <- NULL
    if (isTruthy(GROQ_KEY)) {
      interp <- tryCatch(
        call_groq_score_interpretation(
          resort_name       = r$resort_name,
          weather_score     = r$weather_score,
          terrain_score     = r$terrain_score,
          avalanche_danger  = r$avalanche_danger,
          avalanche_applied = isTRUE(r$avalanche_applied),
          lift_pct          = r$lift_pct,
          ability_level     = input$ability_level,
          ski_date          = input$trip_date,
          groq_key          = GROQ_KEY
        ),
        error = function(e) NULL
      )
    }

    # --- Road conditions bullet text + color ---
    is_winner <- !is.null(result$winner) &&
                 r$resort_name == result$winner$resort_name
    road_text  <- NULL
    road_color <- "#6B7C8F"

    if (is_winner && !is.null(result$llm) && !is.na(result$llm$summary)) {
      road_text  <- result$llm$summary
      road_color <- switch(result$llm$risk_level,
        "do_not_travel" = "#7B2D2D",
        "high"          = "#dc3545",
        "moderate"      = "#fd7e14",
        "low"           = "#2E7D32",
        "#6B7C8F"
      )
    } else if (!is_winner && !is.null(result$reranked)) {
      road_text  <- paste0(
        "CDOT road conditions were analyzed for ",
        result$reranked$winner$resort_name,
        " (top recommendation). Check ", tags$a("CDOT 511", href = "https://cotrip.org",
        target = "_blank"), " for conditions on this route."
      )
    }

    # --- Bullet builder ---
    mk_bullet <- function(icon, label, text, border_color) {
      if (is.null(text) || (is.character(text) && (is.na(text) || nchar(trimws(text)) == 0)))
        return(NULL)
      div(class = "sk-interp-item",
        style = paste0("border-left-color:", border_color, ";"),
        span(class = "sk-interp-icon", icon),
        div(
          tags$strong(class = "sk-interp-label", label),
          div(class = "sk-interp-text", text)
        )
      )
    }

    bullets <- tagList(
      if (!is.null(interp)) mk_bullet("\u2744",    "Weather",         interp$weather,   "#2E6DA4"),
      if (!is.null(interp)) mk_bullet("\u26f7",    "Terrain",         interp$terrain,   "#2E7D32"),
      if (!is.null(interp)) mk_bullet("\u26a0",    "Avalanche risk",  interp$avalanche, "#E65100"),
      mk_bullet("\U0001F6E3", "Road conditions", road_text, road_color)
    )

    if (length(Filter(Negate(is.null), as.list(bullets))) == 0) return(NULL)

    div(class = "sk-score-interp",
      div(class = "sk-interp-header", "Conditions at a glance"),
      bullets
    )
  })

}


# =============================================================================
# RUN
# =============================================================================

shinyApp(ui = ui, server = server)
