# =============================================================================
# app.R — SkiSmart Shiny Application
# =============================================================================
#
# STRUCTURE
#   ui     — sidebar inputs + main panel outputs
#   server — runs scoring pipeline on button click
#
# PIPELINE
#   score_all_resorts() from R/composite_score.R
#   Inputs: ski_date + ability level
#   Output: dataframe ranked by composite_score
#
# TO RUN
#   Open in RStudio and click "Run App", or: shiny::runApp("app.R")
#   Working directory must be the project root.
# =============================================================================

library(shiny)
library(dplyr)

source("R/composite_score.R")

resorts <- read.csv("data/resorts.csv", stringsAsFactors = FALSE)

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

# SVG mountain logo
mountain_logo <- tags$svg(
  xmlns = "http://www.w3.org/2000/svg",
  width = "48", height = "38", viewBox = "0 0 48 38",
  # Background mountain
  tags$polygon(
    points = "24,2 46,36 2,36",
    fill   = "rgba(255,255,255,0.25)"
  ),
  # Snow cap
  tags$polygon(
    points = "24,2 31,14 17,14",
    fill   = "white"
  ),
  # Smaller peak (right)
  tags$polygon(
    points = "36,12 46,36 26,36",
    fill   = "rgba(255,255,255,0.18)"
  )
)

# =============================================================================
# UI
# =============================================================================

ui <- fluidPage(

  tags$head(tags$style(HTML(skismart_css))),

  # ── Header ────────────────────────────────────────────────────────────────
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

      h4("Trip Details"),

      dateInput(
        inputId = "trip_date",
        label   = "Date of trip",
        value   = Sys.Date() + 1,
        min     = Sys.Date(),
        max     = Sys.Date() + 10,
        format  = "mm/dd/yyyy"
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

    # ── Main panel ───────────────────────────────────────────────────────────
    mainPanel(
      width = 9,

      uiOutput("status_message"),
      uiOutput("results_card"),
      uiOutput("warnings_panel"),
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

  pipeline_result <- eventReactive(input$find_resorts, {
    showNotification("Fetching live conditions...", type = "message",
                     duration = NULL, id = "loading")
    on.exit(removeNotification("loading"))

    score_all_resorts(
      resorts  = resorts,
      ski_date = input$trip_date,
      ability  = input$ability_level
    )
  })

  # ── Welcome message ─────────────────────────────────────────────────────────
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
    }
  })

  # ── Results card ────────────────────────────────────────────────────────────
  output$results_card <- renderUI({
    req(pipeline_result())
    df <- pipeline_result()

    ability_label <- c(
      beginner = "Beginner", intermediate = "Intermediate",
      advanced = "Advanced", expert = "Expert"
    )[[input$ability_level]]

    div(class = "result-card",
      h3("Top 5 Recommended Resorts"),
      div(class = "subtitle",
        paste0(ability_label, " · ", format(input$trip_date, "%B %d, %Y"))
      ),
      tableOutput("results_table")
    )
  })

  output$results_table <- renderTable({
    req(pipeline_result())
    df <- pipeline_result() |>
      filter(!is.na(composite_score)) |>
      slice_head(n = 5)

    df |>
      mutate(
        Resort           = resort_name,
        Score            = sprintf("%.2f", composite_score),
        Weather          = ifelse(is.na(weather_score), "—", sprintf("%.2f", weather_score)),
        Terrain          = ifelse(is.na(terrain_score), "—", sprintf("%.2f", terrain_score)),
        `Lifts`          = ifelse(is.na(lift_pct), "—", paste0(round(lift_pct * 100), "%")),
        `Avalanche`      = ifelse(!avalanche_applied, "—", coalesce(avalanche_danger, "—"))
      ) |>
      select(Resort, Score, Weather, Terrain, Lifts, Avalanche)

  }, striped = FALSE, hover = FALSE, bordered = FALSE, na = "—",
     width = "100%", align = "lrrrrr")

  # ── Warnings panel ───────────────────────────────────────────────────────────
  output$warnings_panel <- renderUI({
    req(pipeline_result())
    df <- pipeline_result() |>
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
    df <- pipeline_result()
    if (!any(df$avalanche_applied, na.rm = TRUE)) return(NULL)

    avy_df <- df |>
      filter(avalanche_applied) |>
      arrange(desc(match(avalanche_danger,
                         c("Extreme", "High", "Considerable", "Moderate", "Low"))))

    div(class = "result-card",
      h3("Avalanche Conditions"),
      div(class = "subtitle", "Today's CAIC danger ratings for scored resorts"),
      tableOutput("avalanche_table")
    )
  })

  output$avalanche_table <- renderTable({
    req(pipeline_result())
    df <- pipeline_result()
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
