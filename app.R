# =============================================================================
# app.R — SkiSmart Shiny Application
# =============================================================================
#
# STRUCTURE
#   ui     defines what the user sees (sidebar inputs + main panel outputs)
#   server defines what happens when inputs change (runs the scoring pipeline)
#
# TO RUN
#   Open this file in RStudio and click "Run App", or:
#   shiny::runApp("app.R")
#
# DEPENDENCIES
#   All scoring logic lives in score_resorts.R (which sources fetch_caic.R).
#   This file only handles the UI and the reactive pipeline call.
# =============================================================================

library(shiny)
library(dplyr)
library(readr)

source("score_resorts.R")   # loads score_resorts(), which sources fetch_caic.R

# =============================================================================
# UI
# =============================================================================

ui <- fluidPage(

  # App title and styling
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

      # ── Trip details ────────────────────────────────────────────────────────
      h4("Trip Details"),

      dateInput(
        inputId = "trip_date",
        label   = "Ski date",
        value   = Sys.Date() + 1,
        min     = Sys.Date(),
        max     = Sys.Date() + 10,    # 10-day forecast horizon
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

      # ── Skier profile ────────────────────────────────────────────────────────
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
          "Mostly groomed runs"    = "groomed",
          "Mix of groomed and off-piste" = "mixed",
          "Challenging / off-piste" = "challenging"
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

      # ── Safety preferences ────────────────────────────────────────────────
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

      # ── Submit button ──────────────────────────────────────────────────────
      hr(),
      actionButton(
        inputId = "find_resorts",
        label   = "Find Resorts",
        class   = "btn-primary",
        width   = "100%"
      ),

      br(), br(),
      # Small note about data freshness
      helpText("Avalanche data updates once daily from CAIC.")
    ),

    # ── Main panel: results ───────────────────────────────────────────────────
    mainPanel(
      width = 9,

      # Status message while loading
      uiOutput("status_message"),

      # ── Top resort recommendations ─────────────────────────────────────────
      h3("Recommended Resorts"),
      p("Ranked by composite score based on terrain match and current
         avalanche conditions. Snow quality score will be added once the
         weather data module is complete."),

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

      # ── Debug panel (hidden in production) ────────────────────────────────
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

  # ── Reactive: run pipeline when button is clicked ─────────────────────────
  # isolate() means the pipeline only re-runs when the button is clicked,
  # not every time the user adjusts a slider.

  pipeline_result <- eventReactive(input$find_resorts, {

    # Show a loading message
    showNotification("Fetching live avalanche data...", type = "message",
                     duration = NULL, id = "loading")
    on.exit(removeNotification("loading"))

    # Build user_inputs list from Shiny input values
    user_inputs <- list(
      ability_level   = input$ability_level,
      max_drive_hours = input$max_drive_hours,
      terrain_pref    = input$terrain_pref,
      trip_date       = input$trip_date,
      pass_type       = input$pass_type,
      risk_tolerance  = input$risk_tolerance
    )

    # Run the scoring pipeline
    score_resorts(user_inputs = user_inputs)
  })

  # ── Status message ─────────────────────────────────────────────────────────
  output$status_message <- renderUI({
    if (!isTruthy(pipeline_result())) {
      div(
        class = "alert alert-info",
        "Set your preferences and click ",
        strong("Find Resorts"),
        " to see recommendations."
      )
    }
  })

  # ── Top resorts table ──────────────────────────────────────────────────────
  output$top_resorts_table <- renderTable({
    req(pipeline_result())
    result <- pipeline_result()

    result$top |>
      mutate(
        `Composite Score` = round(composite_score, 1),
        `Terrain Match`   = round(terrain_score, 1),
        `Avalanche Risk`  = round(risk_score, 1),
        `Danger Today`    = coalesce(danger_overall, "—"),
        `Danger Tomorrow` = coalesce(danger_tomorrow_overall, "—"),
        `Avalanche Problems` = coalesce(problem_types, "None reported"),
        `CAIC Zone`       = coalesce(caic_zone, "—")
      ) |>
      select(
        Resort = resort_name,
        `Composite Score`,
        `Terrain Match`,
        `Avalanche Risk`,
        `Danger Today`,
        `Danger Tomorrow`,
        `Avalanche Problems`,
        `CAIC Zone`
      )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # ── Blocked resorts ────────────────────────────────────────────────────────
  output$show_blocked <- reactive({
    req(pipeline_result())
    nrow(pipeline_result()$blocked) > 0
  })
  outputOptions(output, "show_blocked", suspendWhenHidden = FALSE)

  output$blocked_resorts_table <- renderTable({
    req(pipeline_result())
    result <- pipeline_result()
    req(nrow(result$blocked) > 0)

    result$blocked |>
      select(
        Resort       = resort_name,
        `Danger Rating` = danger_overall,
        Reason       = block_reason
      )
  }, striped = TRUE, bordered = TRUE)

  # ── Avalanche conditions table ─────────────────────────────────────────────
  output$avalanche_table <- renderTable({
    req(pipeline_result())

    pipeline_result()$all |>
      filter(!is.na(danger_overall)) |>
      arrange(desc(danger_numeric)) |>
      mutate(
        `Today`    = coalesce(danger_overall, "—"),
        `Tomorrow` = coalesce(danger_tomorrow_overall, "—"),
        `Problems` = coalesce(problem_types, "None reported"),
        `Zone`     = coalesce(caic_zone, "—")
      ) |>
      select(
        Resort   = resort_name,
        `Zone`,
        `Today`,
        `Tomorrow`,
        `Problems`
      )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # ── Debug table ────────────────────────────────────────────────────────────
  # TODO: Remove before final submission
  output$debug_table <- renderTable({
    req(pipeline_result())

    pipeline_result()$all |>
      arrange(desc(composite_score)) |>
      mutate(across(c(composite_score, terrain_score, risk_score), round, 1)) |>
      select(
        resort_name, composite_score, terrain_score, risk_score,
        danger_overall, danger_numeric, hard_block, block_reason
      )
  }, striped = TRUE, bordered = TRUE)
}

# =============================================================================
# RUN
# =============================================================================

shinyApp(ui = ui, server = server)
