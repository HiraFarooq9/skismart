# =============================================================================
# app_demo.R — SkiSmart UI Demo (no live scoring, dummy resort data)
# Run this to preview the map and UI layout without needing all APIs working.
# =============================================================================

library(shiny)
library(dplyr)
library(leaflet)

source("R/load_env.R")
source("R/api_geocode.R")
source("R/api_routing.R")

GH_KEY <- Sys.getenv("GRAPHHOPPER_API_KEY")

# Dummy CDOT conditions along the Denver→Breckenridge I-70 corridor
# Realistic trouble spots with row-level risk levels
DUMMY_CONDITIONS <- tibble(
  location      = c("Floyd Hill (I-70 EB)",
                    "Eisenhower / Johnson Tunnel",
                    "Silverthorne Interchange"),
  lat           = c(39.7245,  39.6846,  39.6320),
  lon           = c(-105.3891, -105.9066, -106.0649),
  row_risk      = c("moderate", "high",   "moderate"),
  description   = c(
    "Packed snow and ice on roadway. Slippery conditions near mile marker 244.",
    "Chain law Code 15 in effect westbound. Wind gusts up to 40 mph near tunnel.",
    "Snow covered roadway. Reduced visibility due to blowing snow. Allow extra time."
  )
)

# Helper: map risk level → polyline color
risk_color <- function(risk) {
  dplyr::case_when(
    risk == "do_not_travel" ~ "#7B2D2D",
    risk == "high"          ~ "#dc3545",
    risk == "moderate"      ~ "#fd7e14",
    TRUE                    ~ "#888888"   # clear / low → gray
  )
}

# Helper: color route waypoints by proximity to condition points.
#
# For each waypoint, finds the highest-priority risk level among any condition
# point within `buffer_deg` (~0.05° ≈ 3.5 miles). Groups consecutive waypoints
# with the same risk into a segment list, overlapping by one point so polylines
# connect seamlessly.
#
# @param waypoints   data frame with columns lat, lon (ordered along route)
# @param conditions  data frame with columns lat, lon, row_risk
# @param buffer_deg  proximity threshold in degrees
# @return list of lists, each with: $lats, $lons, $risk
color_route_segments <- function(waypoints, conditions, buffer_deg = 0.05) {
  priority <- c("clear" = 0, "low" = 1, "moderate" = 2,
                "high" = 3, "do_not_travel" = 4)

  # Assign a risk level to every waypoint
  point_risks <- vapply(seq_len(nrow(waypoints)), function(i) {
    dists   <- sqrt((conditions$lat - waypoints$lat[i])^2 +
                    (conditions$lon - waypoints$lon[i])^2)
    nearby  <- conditions$row_risk[dists <= buffer_deg]
    if (length(nearby) == 0) return("clear")
    nearby[which.max(priority[nearby])]
  }, character(1))

  # Group consecutive same-risk waypoints into segments (overlap by 1 point)
  segments <- list()
  i <- 1
  n <- length(point_risks)
  while (i <= n) {
    j <- i
    while (j < n && point_risks[j + 1] == point_risks[j]) j <- j + 1
    end <- min(j + 1, n)   # +1 overlap for seamless joins
    segments[[length(segments) + 1]] <- list(
      lats = waypoints$lat[i:end],
      lons = waypoints$lon[i:end],
      risk = point_risks[i]
    )
    i <- j + 1
  }
  segments
}

# Dummy top-3 resorts with realistic Colorado coords + fake scores
DUMMY_RESORTS <- tibble(
  resort_name    = c("Breckenridge", "Keystone", "Loveland"),
  lat            = c(39.4817,        39.6134,     39.6800),
  lon            = c(-106.0384,      -105.9769,  -105.8978),
  duration_mins  = c(93,             108,          60),
  distance_miles = c(80.4,           75.2,         56.1),
  drive_multiplier = c(1.0,          0.75,          1.0),
  conditions_score = c(72,           68,            65),
  final_score    = c(72,             51,            65)
)

# Dummy LLM output
DUMMY_LLM <- list(
  risk_level = "moderate",
  summary    = "Moderate snow expected along I-70 with chain law in effect near
                Eisenhower Tunnel. Roads passable but allow extra time. Conditions
                may deteriorate in the afternoon.",
  key_action = "Check chain requirements before departure and carry traction devices."
)

# =============================================================================
# UI
# =============================================================================

ui <- fluidPage(

  titlePanel(
    div(
      h1("SkiSmart", style = "margin-bottom: 0;"),
      h4("UI Demo — dummy resort data, real map routing",
         style = "color: #888; margin-top: 4px;")
    )
  ),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      h4("Starting Location"),
      textInput("start_location", "Your city or address",
                value = "Denver, CO", placeholder = "e.g. Denver, CO or 80302"),

      hr(),
      h4("Trip Details"),
      dateInput("trip_date", "Ski date", value = Sys.Date() + 1,
                min = Sys.Date(), max = Sys.Date() + 10, format = "mm/dd/yyyy"),
      numericInput("max_drive_hours", "Max drive time (hours)",
                   value = 4, min = 1, max = 8, step = 0.5),

      hr(),
      h4("Skier Profile"),
      selectInput("ability_level", "Ability level",
                  choices  = c("Beginner" = "beginner", "Intermediate" = "intermediate",
                               "Advanced" = "advanced", "Expert" = "expert"),
                  selected = "intermediate"),

      hr(),
      actionButton("find_resorts", "Find Resorts (Demo)", class = "btn-primary", width = "100%"),
      br(), br(),
      helpText("Demo mode: resort scores are fake. Map routing is live via GraphHopper.")
    ),

    mainPanel(
      width = 9,

      uiOutput("status_message"),

      leafletOutput("route_map", height = "400px"),
      br(),

      uiOutput("route_risk_panel"),

      h3("Recommended Resorts"),
      p("Ranked by conditions score, reranked with drive-time penalty."),
      tableOutput("top_resorts_table"),

      hr(),
      h3("Avalanche Conditions"),
      p("(Placeholder — CAIC data would appear here in the live app.)"),
      tableOutput("avalanche_table")
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {

  result <- eventReactive(input$find_resorts, {

    # Geocode start location (real API call)
    showNotification("Geocoding start location...", type = "message",
                     duration = NULL, id = "n_geo")
    geo <- tryCatch(geocode_location(input$start_location, n_results = 1),
                    error = function(e) NULL)
    removeNotification("n_geo")

    if (is.null(geo) || nrow(geo) == 0) {
      showNotification(paste("Could not geocode:", input$start_location),
                       type = "warning", duration = 6)
      return(NULL)
    }

    start_lat   <- geo$lat[1]
    start_lon   <- geo$lon[1]
    start_label <- input$start_location

    # Fetch real route to #1 resort (Breckenridge)
    showNotification("Fetching route to Breckenridge...", type = "message",
                     duration = NULL, id = "n_route")
    winner       <- DUMMY_RESORTS[1, ]
    winner_route <- tryCatch(
      fetch_route(start_lat, start_lon, winner$lat, winner$lon, GH_KEY),
      error = function(e) NULL
    )
    removeNotification("n_route")

    list(
      resorts      = DUMMY_RESORTS,
      winner_route = winner_route,
      llm          = DUMMY_LLM,
      drive_flag   = "within_preference",
      start_lat    = start_lat,
      start_lon    = start_lon,
      start_label  = start_label
    )
  })

  # ── Status message ───────────────────────────────────────────────────────────
  output$status_message <- renderUI({
    if (!isTruthy(result())) {
      div(class = "alert alert-info",
          "Set your preferences and click ", strong("Find Resorts (Demo)"),
          " to preview the UI.")
    }
  })

  # ── Leaflet map ──────────────────────────────────────────────────────────────
  output$route_map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = -106.0, lat = 39.5, zoom = 7)
  })

  observeEvent(result(), {
    r <- result()
    if (is.null(r)) return()

    proxy <- leafletProxy("route_map") |> clearMarkers() |> clearShapes()

    # Start marker
    proxy <- proxy |>
      addCircleMarkers(lng = r$start_lon, lat = r$start_lat,
                       radius = 10, color = "#333", fillColor = "#333",
                       fillOpacity = 1,
                       popup = paste0("<b>Start:</b> ", r$start_label),
                       label = r$start_label)

    # Route polyline — colored by segment risk level
    if (!is.null(r$winner_route) && !is.null(r$winner_route$waypoints)) {
      wp       <- r$winner_route$waypoints
      segments <- color_route_segments(wp, DUMMY_CONDITIONS)

      for (seg in segments) {
        proxy <- proxy |>
          addPolylines(
            lng     = seg$lons,
            lat     = seg$lats,
            color   = risk_color(seg$risk),
            weight  = if (seg$risk %in% c("high", "do_not_travel")) 6 else 4,
            opacity = 0.85
          )
      }
    }

    # Resort markers — blue for winner, gray for others
    resorts     <- r$resorts
    winner_name <- resorts$resort_name[1]

    for (i in seq_len(nrow(resorts))) {
      res       <- resorts[i, ]
      is_winner <- res$resort_name == winner_name
      popup_html <- sprintf(
        "<b>#%d %s</b><br>Drive: %.0f min / %.0f mi<br>Final score: %.0f",
        i, res$resort_name, res$duration_mins, res$distance_miles, res$final_score
      )
      proxy <- proxy |>
        addCircleMarkers(
          lng = res$lon, lat = res$lat,
          radius      = if (is_winner) 14 else 10,
          color       = if (is_winner) "#1a73e8" else "#555",
          fillColor   = if (is_winner) "#1a73e8" else "#aaa",
          fillOpacity = 0.9, weight = 2,
          popup = popup_html, label = res$resort_name
        )
    }

    # Legend
    proxy <- proxy |>
      addLegend(
        position = "bottomright",
        colors   = c("#dc3545", "#fd7e14", "#888888", "#1a73e8"),
        labels   = c("High risk road section", "Moderate risk road section",
                     "Clear route", "Recommended resort"),
        opacity  = 0.85,
        title    = "Map Legend"
      )
  })

  # ── Route risk panel ─────────────────────────────────────────────────────────
  output$route_risk_panel <- renderUI({
    req(result())
    r       <- result()
    winner  <- r$resorts[1, ]
    llm     <- r$llm

    badge_color <- switch(llm$risk_level,
      "low"           = "#28a745",
      "moderate"      = "#fd7e14",
      "high"          = "#dc3545",
      "do_not_travel" = "#7B2D2D",
      "#6c757d"
    )
    risk_label <- switch(llm$risk_level,
      "low"           = "LOW RISK",
      "moderate"      = "MODERATE RISK",
      "high"          = "HIGH RISK",
      "do_not_travel" = "DO NOT TRAVEL",
      "UNKNOWN"
    )

    tagList(
      hr(),
      h3(paste("Route Conditions to", winner$resort_name)),
      p(strong("Estimated drive: "),
        sprintf("%.0f min / %.0f mi", winner$duration_mins, winner$distance_miles)),
      div(
        style = "margin: 12px 0;",
        span(risk_label,
             style = sprintf(
               "background:%s; color:white; padding:6px 16px; border-radius:4px;
                font-weight:bold; font-size:15px;", badge_color))
      ),
      p(llm$summary),
      div(class = "alert alert-secondary",
          strong("Key action: "), llm$key_action),
      hr()
    )
  })

  # ── Top resorts table ────────────────────────────────────────────────────────
  output$top_resorts_table <- renderTable({
    req(result())
    result()$resorts |>
      mutate(
        Rank           = seq_len(n()),
        `Drive Time`   = sprintf("%.0f min / %.0f mi", duration_mins, distance_miles),
        `Drive Penalty` = if_else(drive_multiplier == 1.0, "None",
                                  sprintf("%.0f%%", (1 - drive_multiplier) * 100)),
        `Conditions Score` = conditions_score,
        `Final Score`  = final_score
      ) |>
      select(Rank, Resort = resort_name, `Drive Time`,
             `Drive Penalty`, `Conditions Score`, `Final Score`)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # ── Avalanche placeholder table ──────────────────────────────────────────────
  output$avalanche_table <- renderTable({
    req(result())
    tibble(
      Resort  = c("Breckenridge", "Keystone", "Loveland"),
      Zone    = c("Summit", "Summit", "Front Range"),
      Today   = c("Considerable", "Moderate", "Low"),
      Tomorrow = c("Moderate", "Moderate", "Low"),
      Problems = c("Wind Slab, Storm Slab", "Storm Slab", "None reported")
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
}

shinyApp(ui = ui, server = server)
