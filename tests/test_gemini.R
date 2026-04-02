# =============================================================================
# test_gemini.R
# Quick smoke test for the Gemini LLM connection.
# Run with: source("tests/test_gemini.R")
# =============================================================================

source("R/load_env.R")
source("R/llm_route_summary.R")

groq_key <- Sys.getenv("GROQ_API_KEY")

if (nchar(groq_key) == 0) {
  stop("GROQ_API_KEY not found in .env — add it and re-run.")
}

cat("--- Groq API smoke test ---\n")
cat("Model:", GROQ_MODEL, "\n\n")

# Minimal hardcoded prompt — no other dependencies needed
test_prompt <- paste0(
  "You are a road safety assistant helping a skier plan a drive to Breckenridge on 2026-04-05.\n",
  "Estimated drive: 85 miles, 95 minutes under normal conditions.\n",
  "Preliminary risk assessment (keyword-based): moderate\n\n",
  "CONDITIONS ALONG ROUTE:\n",
  "  [I-70 WB | Chain Law | updated 2026-04-05 06:00]\n",
  "  Chain law in effect between Vail and Silverthorne. Packed snow on road.\n\n",
  "Respond with EXACTLY this structure and no other text:\n\n",
  "RISK_LEVEL: [low | moderate | high | do_not_travel]\n",
  "SUMMARY: [2-3 sentences on current driving conditions to Breckenridge.",
  " Name specific roads or passes if mentioned.",
  " Flag chain laws or closures explicitly.]\n",
  "KEY_ACTION: [The single most important thing the driver should do,",
  " or \"None\" if conditions are clear.]"
)

result <- call_groq_route_summary(test_prompt, groq_key)

cat("=== RESULT ===\n")
cat("risk_level :", result$risk_level, "\n")
cat("summary    :", result$summary,    "\n")
cat("key_action :", result$key_action, "\n")
cat("\n=== RAW RESPONSE ===\n")
cat(result$raw_text, "\n")
