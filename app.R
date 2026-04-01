# =============================================================================
# app.R — SkiSmart Shiny Application
# =============================================================================
#
# STRUCTURE
#   ui     defines what the user sees (sidebar inputs + main panel outputs)
#   server defines what happens when inputs change (runs the scoring pipeline)
#
# PIPELINE
#   Stage 1 — score_resorts.R: conditions + avalanche scoring → top 3
#   Stage 2 — geocode → route → drive-time rerank → CDOT → Groq LLM summary
#
# TO RUN
#   Open this file in RStudio and click "Run App", or:
#   shiny::runApp("app.R")
#
# DEPENDENCIES
#   score_resorts.R sources fetch_caic.R.
#   Stage 2 files live in R/. API keys in .env (copy from .env.example).
# =============================================================================

library(shiny)
library(dplyr)
library(readr)
library(leaflet)

source("score_resorts.R")   # Stage 1 orchestrator (sources fetch_caic.R)
source("R/load_env.R")      # loads .env keys into Sys.setenv()
source("R/api_geocode.R")
source("R/api_routing.R")
source("R/api_cdot.R")
source("R/route_conditions.R")
source("R/llm_route_summary.R")
source("R/stage2_rerank.R")

GH_KEY   <- Sys.getenv("GRAPHHOPPER_API_KEY")
CDOT_KEY <- Sys.getenv("CDOT_API_KEY")
GROQ_KEY <- Sys.getenv("GROQ_API_KEY")

# Map risk level → polyline color
risk_color <- function(risk) {
  dplyr::case_when(
    risk == "do_not_travel" ~ "#7B2D2D",
    risk == "high"          ~ "#dc3545",
    risk == "moderate"      ~ "#fd7e14",
    TRUE                    ~ "#888888"
  )
}

# Color route waypoints by proximity to CDOT condition points.
# Returns a list of segments, each with $lats, $lons, $risk.
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
# UI
# =============================================================================

ui <- fluidPage(

  titlePanel(
    div(
      h1("SkiSmart", style = "margin-bottom: 0;"),
      h4("Find the best Colorado ski resort for your trip",
         style = "color: #666; margin-top: 4px;")
    )
  ),

  sidebarLayout(

    # ── Sidebar: all user inputs ─────────────────────────────────────────────
    sidebarPanel(
      width = 3,

      # ── Starting location ───────────────────────────────────────────────────
      h4("Starting Location"),

      textInput(
        inputId     = "start_location",
        label       = "Your city or address",
        value       = "Denver, CO",
        placeholder = "e.g. Denver, CO  or  80302"
      ),

      # ── Trip details ─────────────────────────────────────────────────────────
      hr(),
      h4("Trip Details"),

      dateInput(
        inputId = "trip_date",
        label   = "Ski date",
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

      # ── Skier profile ─────────────────────────────────────────────────────────
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

      # ── Safety preferences ─────────────────────────────────────────────────
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

      # ── Submit ─────────────────────────────────────────────────────────────
      hr(),
      actionButton(
        inputId = "find_resorts",
        label   = "Find Resorts",
        class   = "btn-primary",
        width   = "100%"
      ),

      br(), br(),
      helpText("Avalanche data updates once daily from CAIC.")
    ),

    # ── Main panel: results ─────────────────────────────────────────────────
    mainPanel(
      width = 9,

      uiOutput("status_message"),

      # ── Map ────────────────────────────────────────────────────────────────
      leafletOutput("route_map", height = "400px"),
      br(),

      # ── Route risk panel (winner only) ─────────────────────────────────────
      uiOutput("route_risk_panel"),

      # ── Top resort recommendations ─────────────────────────────────────────
      h3("Recommended Resorts"),
      p("Ranked by composite conditions score, reranked by drive time from your
        starting location."),
      tableOutput("top_resorts_table"),

      # ── Blocked resorts ────────────────────────────────────────────────────
      conditionalPanel(
        condition = "output.show_blocked",
        hr(),
        h4("Resorts Excluded"),
        p("The following resorts were excluded due to safety conditions:"),
        tableOutput("blocked_resorts_table")
      ),

      # ── Avalanche details ──────────────────────────────────────────────────
      hr(),
      h3("Current Avalanche Conditions"),
      p("Today's and tomorrow's danger ratings for all resorts, from the
         Colorado Avalanche Information Center (CAIC)."),
      tableOutput("avalanche_table"),

      # ── Debug panel ────────────────────────────────────────────────────────
      # TODO: Remove before final submission
      hr(),
      h4("Debug: Raw Scores (all resorts)"),
      tableOutput("debug_table")
    )
  )
)


# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {

  # ── Stage 1 + Stage 2 pipeline ──────────────────────────────────────────────
  pipeline_result <- eventReactive(input$find_resorts, {

    # --- Stage 1: conditions scoring ------------------------------------------
    showNotification("Fetching avalanche and terrain data...",
                     type = "message", duration = NULL, id = "loading_s1")

    user_inputs <- list(
      ability_level   = input$ability_level,
      max_drive_hours = input$max_drive_hours,
      terrain_pref    = input$terrain_pref,
      trip_date       = input$trip_date,
      pass_type       = input$pass_type,
      risk_tolerance  = input$risk_tolerance
    )

    stage1 <- tryCatch(
      score_resorts(user_inputs = user_inputs),
      error = function(e) {
        showNotification(paste("Stage 1 error:", e$message), type = "error", duration = 8)
        NULL
      }
    )
    removeNotification("loading_s1")

    if (is.null(stage1) || nrow(stage1$top) == 0) {
      return(list(stage1 = stage1, error = "No resorts scored in Stage 1."))
    }

    # --- Geocode user start location ------------------------------------------
    showNotification("Geocoding your start location...",
                     type = "message", duration = NULL, id = "loading_geo")

    geo <- tryCatch(
      geocode_location(input$start_location, n_results = 1),
      error = function(e) NULL
    )
    removeNotification("loading_geo")

    if (is.null(geo) || nrow(geo) == 0) {
      showNotification(
        paste0("Could not find '", input$start_location,
               "'. Showing Stage 1 results only."),
        type = "warning", duration = 8
      )
      return(list(stage1 = stage1,
                  geo_error = paste("Could not geocode:", input$start_location)))
    }

    start_lat     <- geo$lat[1]
    start_lon     <- geo$lon[1]
    start_label   <- input$start_location
    user_max_mins <- input$max_drive_hours * 60

    # --- Stage 2: drive-time reranking ----------------------------------------
    showNotification("Fetching drive times for top 3 resorts...",
                     type = "message", duration = NULL, id = "loading_route")

    top3 <- stage1$top |>
      head(3) |>
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
      return(list(stage1 = stage1,
                  start_lat = start_lat, start_lon = start_lon,
                  start_label = start_label))
    }

    # --- Fetch winner route (for map polyline) --------------------------------
    winner <- reranked$winner
    winner_route <- tryCatch(
      fetch_route(start_lat, start_lon, winner$lat, winner$lon, GH_KEY),
      error = function(e) NULL
    )

    # --- CDOT + Groq LLM for winner ------------------------------------------
    showNotification("Analyzing road conditions...",
                     type = "message", duration = NULL, id = "loading_llm")

    llm_out    <- NULL
    route_prep <- NULL

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
            call_gemini_route_summary(route_prep$llm_prompt, GROQ_KEY),
            error = function(e) NULL
          )
        }
      }
    }
    removeNotification("loading_llm")

    list(
      stage1       = stage1,
      reranked     = reranked,
      winner_route = winner_route,
      route_prep   = route_prep,
      llm          = llm_out,
      start_lat    = start_lat,
      start_lon    = start_lon,
      start_label  = start_label
    )
  })


  # ── Status message ───────────────────────────────────────────────────────────
  output$status_message <- renderUI({
    if (!isTruthy(pipeline_result())) {
      div(
        class = "alert alert-info",
        "Set your preferences and click ",
        strong("Find Resorts"),
        " to see recommendations."
      )
    } else if (!is.null(pipeline_result()$error)) {
      div(class = "alert alert-danger", pipeline_result()$error)
    } else if (!is.null(pipeline_result()$geo_error)) {
      div(class = "alert alert-warning", pipeline_result()$geo_error)
    }
  })


  # ── Leaflet map ──────────────────────────────────────────────────────────────
  output$route_map <- renderLeaflet({
    base_map <- leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = -106.0, lat = 39.5, zoom = 7)
    base_map
  })

  observeEvent(pipeline_result(), {
    result <- pipeline_result()
    if (is.null(result)) return()

    proxy <- leafletProxy("route_map") |> clearMarkers() |> clearShapes()

    # Start location marker
    if (!is.null(result$start_lat)) {
      proxy <- proxy |>
        addCircleMarkers(
          lng         = result$start_lon,
          lat         = result$start_lat,
          radius      = 10,
          color       = "#333",
          fillColor   = "#333",
          fillOpacity = 1,
          popup       = paste0("<b>Start:</b> ", result$start_label),
          label       = result$start_label
        )
    }

    # Route polyline — colored by segment risk level
    if (!is.null(result$winner_route) && !is.null(result$winner_route$waypoints)) {
      wp <- result$winner_route$waypoints

      # Build condition points for coloring (from real CDOT filtered_df if available)
      if (!is.null(result$route_prep) && nrow(result$route_prep$filtered_df) > 0) {
        conds <- result$route_prep$filtered_df |>
          dplyr::distinct(segment_id, condition_id, .keep_all = TRUE) |>
          dplyr::transmute(lat = start_lat, lon = start_lon, row_risk = row_risk)
        segments <- color_route_segments(wp, conds)
      } else {
        # No CDOT data — draw full route gray
        segments <- list(list(lats = wp$lat, lons = wp$lon, risk = "clear"))
      }

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

    # Resort markers (reranked or stage1 fallback)
    if (!is.null(result$reranked)) {
      ranking     <- result$reranked$ranking
      winner_name <- result$reranked$winner$resort_name

      for (i in seq_len(nrow(ranking))) {
        r         <- ranking[i, ]
        is_winner <- r$resort_name == winner_name
        drive_str <- if (is.na(r$duration_mins)) {
          "Drive time unavailable"
        } else {
          sprintf("%.0f min / %.0f mi", r$duration_mins, r$distance_miles)
        }
        proxy <- proxy |>
          addCircleMarkers(
            lng         = r$lon,
            lat         = r$lat,
            radius      = if (is_winner) 14 else 10,
            color       = if (is_winner) "#1a73e8" else "#555",
            fillColor   = if (is_winner) "#1a73e8" else "#aaa",
            fillOpacity = 0.9,
            weight      = 2,
            popup       = sprintf("<b>#%d %s</b><br>Drive: %s<br>Score: %.2f",
                                  i, r$resort_name, drive_str, r$final_score),
            label       = r$resort_name
          )
      }
    } else if (!is.null(result$stage1)) {
      top <- result$stage1$top |> head(3)
      for (i in seq_len(nrow(top))) {
        r <- top[i, ]
        proxy <- proxy |>
          addCircleMarkers(
            lng         = r$longitude,
            lat         = r$latitude,
            radius      = if (i == 1) 14 else 10,
            color       = if (i == 1) "#1a73e8" else "#555",
            fillColor   = if (i == 1) "#1a73e8" else "#aaa",
            fillOpacity = 0.9,
            weight      = 2,
            popup       = sprintf("<b>#%d %s</b><br>Score: %.1f", i, r$resort_name, r$composite_score),
            label       = r$resort_name
          )
      }
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
    req(pipeline_result())
    result <- pipeline_result()

    # Only show if we have at least a winner with drive time
    if (is.null(result$reranked)) return(NULL)

    winner      <- result$reranked$winner
    winner_name <- winner$resort_name
    drive_str   <- if (is.na(winner$duration_mins)) {
      "unavailable"
    } else {
      sprintf("%.0f min / %.0f mi", winner$duration_mins, winner$distance_miles)
    }

    # Drive flag alert
    flag_ui <- switch(result$reranked$drive_time_flag,
      "all_exceed_preference" = div(
        class = "alert alert-warning", style = "margin-top: 8px;",
        strong("Note: "),
        "All resorts exceed your max drive time. Results ranked by conditions score."
      ),
      "slightly_over" = div(
        class = "alert alert-info", style = "margin-top: 8px;",
        strong("Note: "),
        "Your top resort is slightly over your preferred drive time."
      ),
      NULL
    )

    # LLM risk badge + summary
    if (!is.null(result$llm)) {
      risk <- result$llm$risk_level
      badge_color <- switch(risk,
        "low"           = "#28a745",
        "moderate"      = "#fd7e14",
        "high"          = "#dc3545",
        "do_not_travel" = "#7B2D2D",
        "#6c757d"
      )
      risk_label <- switch(risk,
        "low"           = "LOW RISK",
        "moderate"      = "MODERATE RISK",
        "high"          = "HIGH RISK",
        "do_not_travel" = "DO NOT TRAVEL",
        "UNKNOWN"
      )
      key_action_ui <- if (!is.na(result$llm$key_action) &&
                           trimws(result$llm$key_action) != "None") {
        div(
          class = "alert alert-secondary",
          style = "margin-top: 8px;",
          strong("Key action: "), result$llm$key_action
        )
      }

      tagList(
        hr(),
        h3(paste("Route Conditions to", winner_name)),
        p(strong("Estimated drive: "), drive_str),
        flag_ui,
        div(
          style = "margin: 12px 0;",
          span(
            risk_label,
            style = sprintf(
              "background:%s; color:white; padding:6px 16px; border-radius:4px;
               font-weight:bold; font-size:15px; letter-spacing:0.5px;",
              badge_color
            )
          )
        ),
        p(result$llm$summary),
        key_action_ui,
        hr()
      )

    } else {
      # No LLM — still show drive time + flag
      no_llm_msg <- if (nchar(GROQ_KEY) == 0) {
        div(
          class = "alert alert-warning",
          "Add ", code("GROQ_API_KEY"), " to your .env file to enable AI-powered
           road condition summaries."
        )
      } else {
        p(class = "text-muted",
          "Road condition summary could not be retrieved. Check CDOT's ",
          a("CoTrip website", href = "https://cotrip.org", target = "_blank"),
          " or call 511 before driving.")
      }

      tagList(hr(), h3(paste("Route to", winner_name)),
              p(strong("Estimated drive: "), drive_str),
              flag_ui, no_llm_msg, hr())
    }
  })


  # ── Top resorts table ────────────────────────────────────────────────────────
  output$top_resorts_table <- renderTable({
    req(pipeline_result())
    result <- pipeline_result()

    if (!is.null(result$reranked)) {
      # Stage 2: reranked with drive time
      result$reranked$ranking |>
        mutate(
          Rank         = seq_len(n()),
          `Drive Time` = if_else(
            is.na(duration_mins), "—",
            sprintf("%.0f min / %.0f mi", duration_mins, distance_miles)
          ),
          `Drive Penalty` = if_else(
            drive_multiplier == 1.0, "None",
            sprintf("%.0f%%", (1 - drive_multiplier) * 100)
          ),
          `Final Score`   = round(final_score, 1)
        ) |>
        select(Rank, Resort = resort_name, `Drive Time`,
               `Drive Penalty`, `Final Score`)

    } else {
      req(result$stage1)
      # Stage 1 fallback (no drive times)
      result$stage1$top |>
        mutate(
          `Composite Score` = round(composite_score, 1),
          `Terrain Match`   = round(terrain_score, 1),
          `Avalanche Risk`  = round(risk_score, 1),
          `Danger Today`    = coalesce(danger_overall, "—"),
          `Danger Tomorrow` = coalesce(danger_tomorrow_overall, "—"),
          `Avalanche Problems` = coalesce(problem_types, "None reported"),
          `CAIC Zone`       = coalesce(caic_zone, "—")
        ) |>
        select(Resort = resort_name, `Composite Score`, `Terrain Match`,
               `Avalanche Risk`, `Danger Today`, `Danger Tomorrow`,
               `Avalanche Problems`, `CAIC Zone`)
    }
  }, striped = TRUE, hover = TRUE, bordered = TRUE)


  # ── Blocked resorts ──────────────────────────────────────────────────────────
  output$show_blocked <- reactive({
    req(pipeline_result())
    result <- pipeline_result()
    !is.null(result$stage1) && nrow(result$stage1$blocked) > 0
  })
  outputOptions(output, "show_blocked", suspendWhenHidden = FALSE)

  output$blocked_resorts_table <- renderTable({
    req(pipeline_result())
    result <- pipeline_result()
    req(!is.null(result$stage1))
    req(nrow(result$stage1$blocked) > 0)

    result$stage1$blocked |>
      select(Resort = resort_name,
             `Danger Rating` = danger_overall,
             Reason          = block_reason)
  }, striped = TRUE, bordered = TRUE)


  # ── Avalanche conditions table ───────────────────────────────────────────────
  output$avalanche_table <- renderTable({
    req(pipeline_result())
    result <- pipeline_result()
    req(!is.null(result$stage1))

    result$stage1$all |>
      filter(!is.na(danger_overall)) |>
      arrange(desc(danger_numeric)) |>
      mutate(
        `Today`    = coalesce(danger_overall, "—"),
        `Tomorrow` = coalesce(danger_tomorrow_overall, "—"),
        `Problems` = coalesce(problem_types, "None reported"),
        `Zone`     = coalesce(caic_zone, "—")
      ) |>
      select(Resort = resort_name, `Zone`, `Today`, `Tomorrow`, `Problems`)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)


  # ── Debug table ──────────────────────────────────────────────────────────────
  # TODO: Remove before final submission
  output$debug_table <- renderTable({
    req(pipeline_result())
    result <- pipeline_result()
    req(!is.null(result$stage1))

    result$stage1$all |>
      arrange(desc(composite_score)) |>
      mutate(across(c(composite_score, terrain_score, risk_score), round, 1)) |>
      select(resort_name, composite_score, terrain_score, risk_score,
             danger_overall, danger_numeric, hard_block, block_reason)
  }, striped = TRUE, bordered = TRUE)
}


# =============================================================================
# RUN
# =============================================================================

shinyApp(ui = ui, server = server)
