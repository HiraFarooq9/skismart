# launch_app.R
# Run this file to launch the SkiSmart Shiny app.
# Use this instead of runApp() if you get path errors.

app_path <- "C:/Users/maren/Dateien/Mein PC (DESKTOP-MQ5O0D6)/Desktop/Duke/Data Science and R/codex-duke/final project"

setwd(app_path)
shiny::runApp(app_path, launch.browser = TRUE)
