# =============================================================================
# load_env.R
# Loads API keys from .env into R environment variables.
# Source this at the top of any script or test that needs API keys.
#
# Usage:
#   source("R/load_env.R")
#   key <- Sys.getenv("CDOT_API_KEY")
# =============================================================================

.env_path <- file.path(
  here::here(),   # project root, works regardless of working directory
  ".env"
)

if (!file.exists(.env_path)) {
  stop("No .env file found at: ", .env_path,
       "\nCopy the .env.example file, rename it to .env, and fill in your keys.")
}

lines <- readLines(.env_path, warn = FALSE)
lines <- lines[nchar(trimws(lines)) > 0 & !startsWith(trimws(lines), "#")]

for (line in lines) {
  parts <- strsplit(line, "=", fixed = TRUE)[[1]]
  if (length(parts) >= 2) {
    key   <- trimws(parts[[1]])
    value <- trimws(paste(parts[-1], collapse = "="))  # handles values containing =
    do.call(Sys.setenv, setNames(list(value), key))
  }
}

rm(lines, line, parts, key, value, .env_path)
