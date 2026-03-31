# =============================================================================
# test_scoring.R
# Unit + scenario tests for weather_score.R and terrain_open_score.R
#
# Run with:
#   source("tests/test_scoring.R")
# (working directory must be the project root)
# =============================================================================

library(dplyr)

source("R/weather_score.R")
source("R/terrain_open_score.R")


# -----------------------------------------------------------------------------
# Lightweight test helpers
# -----------------------------------------------------------------------------

.passed <- 0
.failed <- 0

check <- function(desc, actual, expected, tol = 1e-6) {
  actual   <- round(actual,   10)
  expected <- round(expected, 10)
  if (abs(actual - expected) <= tol) {
    cat(sprintf("  PASS  %s\n", desc))
    .passed <<- .passed + 1
  } else {
    cat(sprintf("  FAIL  %s\n        expected %s  got %s\n", desc, expected, actual))
    .failed <<- .failed + 1
  }
}

check_true <- function(desc, expr) {
  if (isTRUE(expr)) {
    cat(sprintf("  PASS  %s\n", desc))
    .passed <<- .passed + 1
  } else {
    cat(sprintf("  FAIL  %s\n", desc))
    .failed <<- .failed + 1
  }
}

section <- function(title) cat(sprintf("\n── %s\n", title))

# Helper: pull score out of list-returning functions
sc <- function(x) if (is.list(x)) x$score else x


# =============================================================================
# WEATHER SCORE TESTS
# =============================================================================

section("score_snow_depth — no hard exclude, scale from 0\"")
check("0\"   → 0.0",  sc(score_snow_depth(0)),   0.0)
check("12\"  → 0.05", sc(score_snow_depth(12)),  0.05)   # halfway 0→24: 0.0 + 0.5*(0.1)
check("24\"  → 0.1",  sc(score_snow_depth(24)),  0.1)
check("30\"  → 0.2",  sc(score_snow_depth(30)),  0.2)
check("36\"  → 0.5",  sc(score_snow_depth(36)),  0.5)
check("48\"  → 0.8",  sc(score_snow_depth(48)),  0.8)
check("72\"  → 0.9",  sc(score_snow_depth(72)),  0.9)

section("score_snow_depth — interpolation midpoints")
check("27\" (mid 24–30) → 0.15",  sc(score_snow_depth(27)), 0.15)  # 0.1 + 0.5*(0.2-0.1)
check("33\" (mid 30–36) → 0.35",  sc(score_snow_depth(33)), 0.35)
check("42\" (mid 36–48) → 0.65",  sc(score_snow_depth(42)), 0.65)
check("60\" (mid 48–72) → 0.85",  sc(score_snow_depth(60)), 0.85)

section("score_snow_depth — cap at 100\"")
check("100\" → 1.0", sc(score_snow_depth(100)), 1.0)
check("150\" → 1.0", sc(score_snow_depth(150)), 1.0)

section("score_snow_depth — warnings")
check_true("warning fires below 24\"",        !is.null(score_snow_depth(10)$warning))
check_true("warning fires 24\"–30\"",         !is.null(score_snow_depth(27)$warning))
check_true("no warning above 30\"",            is.null(score_snow_depth(40)$warning))

section("score_snowfall — boundaries")
check("0\"  → 0.0",  score_snowfall(0),  0.0)
check("18\" → 1.0",  score_snowfall(18), 1.0)
check("25\" → 1.0",  score_snowfall(25), 1.0)

section("score_snowfall — breakpoints")
check("1\"  → 0.1",  score_snowfall(1),  0.1)
check("3\"  → 0.3",  score_snowfall(3),  0.3)
check("6\"  → 0.6",  score_snowfall(6),  0.6)
check("12\" → 0.9",  score_snowfall(12), 0.9)

section("score_snowfall — interpolation midpoints")
check("2\"  (mid 1–3)   → 0.2",  score_snowfall(2),  0.2)
check("9\"  (mid 6–12)  → 0.75", score_snowfall(9),  0.75)
check("15\" (mid 12–18) → 0.95", score_snowfall(15), 0.95)

section("get_precip_multiplier")
check("no precip → 1.0",                       get_precip_multiplier(25, 0)$multiplier,       1.0)
check("cold + precip → snow → 1.0",            get_precip_multiplier(20, 3)$multiplier,       1.0)
check("borderline temp, low snow → mixed 0.7", get_precip_multiplier(32, 2, 1)$multiplier,   0.7)
check("borderline temp, high snowfall → snow", get_precip_multiplier(32, 6, 6)$multiplier,   1.0)
check("light rain  → 0.6",                     get_precip_multiplier(36, 1.0)$multiplier,    0.6)
check("moderate rain → 0.4",                   get_precip_multiplier(36, 5.0)$multiplier,    0.4)
check("heavy rain  → 0.2",                     get_precip_multiplier(36, 10.0)$multiplier,   0.2)
check_true("type: no precip = 'none'",         get_precip_multiplier(25, 0)$type == "none")
check_true("type: cold snow = 'snow'",         get_precip_multiplier(20, 3)$type == "snow")
check_true("type: mixed = 'mixed'",            get_precip_multiplier(32, 2, 1)$type == "mixed")
check_true("type: light rain label correct",   get_precip_multiplier(36, 1)$type == "light_rain")

section("score_temperature — step function (flat per band)")
check("< 0°F → 0.5 (hardcoded)",  sc(score_temperature(-10)), 0.5)
check("0°F   → 0.8",              sc(score_temperature(0)),   0.8)
check("7°F   → 0.8 (flat band)",  sc(score_temperature(7)),   0.8)
check("14.9°F → 0.8 (flat band)", sc(score_temperature(14.9)),0.8)
check("15°F  → 1.0",              sc(score_temperature(15)),  1.0)
check("22°F  → 1.0 (flat band)",  sc(score_temperature(22)),  1.0)
check("29.9°F → 1.0 (flat band)", sc(score_temperature(29.9)),1.0)
check("30°F  → 0.7",              sc(score_temperature(30)),  0.7)
check("31°F  → 0.7 (flat band)",  sc(score_temperature(31)),  0.7)
check("32°F  → 0.3",              sc(score_temperature(32)),  0.3)
check("33.5°F → 0.3 (flat band)", sc(score_temperature(33.5)),0.3)
check("35°F  → 0.1",              sc(score_temperature(35)),  0.1)
check("50°F  → 0.1",              sc(score_temperature(50)),  0.1)
check_true("warning fires below 0°F",  !is.null(score_temperature(-5)$warning))
check_true("no warning at 22°F",        is.null(score_temperature(22)$warning))

section("score_wind — step function (flat per band)")
check("0 mph   → 1.0",            sc(score_wind(0)),    1.0)
check("10 mph  → 1.0",            sc(score_wind(10)),   1.0)
check("14.9 mph → 1.0 (flat)",    sc(score_wind(14.9)), 1.0)
check("15 mph  → 0.7",            sc(score_wind(15)),   0.7)
check("20 mph  → 0.7 (flat)",     sc(score_wind(20)),   0.7)
check("25 mph  → 0.4",            sc(score_wind(25)),   0.4)
check("30 mph  → 0.4 (flat)",     sc(score_wind(30)),   0.4)
check("35 mph  → 0.2",            sc(score_wind(35)),   0.2)
check("40 mph  → 0.2 (flat)",     sc(score_wind(40)),   0.2)
check("45 mph  → 0.05",           sc(score_wind(45)),   0.05)
check("60 mph  → 0.05",           sc(score_wind(60)),   0.05)
check_true("warning fires at 50 mph",  !is.null(score_wind(50)$warning))
check_true("no warning at 10 mph",      is.null(score_wind(10)$warning))

section("get_weather_weights — sums to 1.0")
for (ab in c("beginner", "intermediate", "advanced", "expert")) {
  check(paste(ab, "weights sum to 1.0"), sum(get_weather_weights(ab)), 1.0)
}
check_true("beginner: depth is largest weight",
           get_weather_weights("beginner")["depth"] == max(get_weather_weights("beginner")))
check_true("expert: snowfall is largest weight",
           get_weather_weights("expert")["snowfall"] == max(get_weather_weights("expert")))


# =============================================================================
# TERRAIN OPEN SCORE TESTS
# =============================================================================

section("get_terrain_pct_by_depth — 0\" base")
r0 <- get_terrain_pct_by_depth(0)
check("0\" greens = 0.00",  r0["greens"],     0.00)
check("0\" blues  = 0.00",  r0["blues"],      0.00)
check("0\" blacks = 0.00",  r0["blacks"],     0.00)
check("0\" dbl    = 0.00",  r0["dbl_blacks"], 0.00)

section("get_terrain_pct_by_depth — breakpoints")
r24 <- get_terrain_pct_by_depth(24)
check("24\" greens = 0.60",  r24["greens"],     0.60)
check("24\" blues  = 0.20",  r24["blues"],      0.20)
check("24\" blacks = 0.00",  r24["blacks"],     0.00)
check("24\" dbl    = 0.00",  r24["dbl_blacks"], 0.00)

r36 <- get_terrain_pct_by_depth(36)
check("36\" greens = 0.95",  r36["greens"],     0.95)
check("36\" blues  = 0.75",  r36["blues"],      0.75)
check("36\" blacks = 0.40",  r36["blacks"],     0.40)
check("36\" dbl    = 0.10",  r36["dbl_blacks"], 0.10)

r72 <- get_terrain_pct_by_depth(72)
check("72\" greens = 1.00",  r72["greens"],     1.00)
check("72\" blues  = 1.00",  r72["blues"],      1.00)
check("72\" blacks = 0.90",  r72["blacks"],     0.90)
check("72\" dbl    = 0.70",  r72["dbl_blacks"], 0.70)

section("get_terrain_pct_by_depth — interpolation")
r12 <- get_terrain_pct_by_depth(12)   # halfway between 0 and 24
check("12\" greens = 0.30",  round(r12["greens"], 3),     0.300)  # 0 + 0.5*0.60
check("12\" blues  = 0.10",  round(r12["blues"], 3),      0.100)  # 0 + 0.5*0.20
check("12\" blacks = 0.00",  round(r12["blacks"], 3),     0.000)
check("12\" dbl    = 0.00",  round(r12["dbl_blacks"], 3), 0.000)

r27 <- get_terrain_pct_by_depth(27)   # halfway between 24 and 30
check("27\" greens = 0.70",  round(r27["greens"], 3),     0.700)
check("27\" blues  = 0.35",  round(r27["blues"], 3),      0.350)
check("27\" blacks = 0.05",  round(r27["blacks"], 3),     0.050)
check("27\" dbl    = 0.00",  round(r27["dbl_blacks"], 3), 0.000)

r60 <- get_terrain_pct_by_depth(60)   # halfway between 48 and 72
check("60\" greens = 1.00",  r60["greens"],     1.00)
check("60\" blues  = 0.95",  r60["blues"],      0.95)
check("60\" blacks = 0.80",  r60["blacks"],     0.80)
check("60\" dbl    = 0.55",  r60["dbl_blacks"], 0.55)

section("apply_snowfall_bump — no bump below 3\"")
base   <- c(greens = 0.60, blues = 0.20, blacks = 0.00, dbl_blacks = 0.00)
result <- apply_snowfall_bump(base, 2)
check("2\" snow — greens unchanged", result["greens"], 0.60)
check("2\" snow — blacks unchanged", result["blacks"], 0.00)

section("apply_snowfall_bump — bump tiers")
base <- c(greens = 0.60, blues = 0.40, blacks = 0.30, dbl_blacks = 0.20)
check("4\" snow (+0.05) greens = 0.65", apply_snowfall_bump(base, 4)["greens"],  0.65)
check("8\" snow (+0.10) greens = 0.70", apply_snowfall_bump(base, 8)["greens"],  0.70)
check("14\" snow (+0.15) greens = 0.75",apply_snowfall_bump(base, 14)["greens"], 0.75)

section("apply_snowfall_bump — tier caps respected")
near_max <- c(greens = 0.95, blues = 0.95, blacks = 0.85, dbl_blacks = 0.65)
bumped   <- apply_snowfall_bump(near_max, 14)  # +0.15
check("greens capped at 1.00",     bumped["greens"],     1.00)
check("blues  capped at 1.00",     bumped["blues"],      1.00)
check("blacks capped at 0.90",     bumped["blacks"],     0.90)
check("dbl_blacks capped at 0.70", bumped["dbl_blacks"], 0.70)

section("get_lift_pct — step function")
check("0 mph   → 1.00",   get_lift_pct(0)$pct,    1.00)
check("10 mph  → 1.00",   get_lift_pct(10)$pct,   1.00)
check("15 mph  → 0.90",   get_lift_pct(15)$pct,   0.90)
check("20 mph  → 0.90",   get_lift_pct(20)$pct,   0.90)
check("25 mph  → 0.70",   get_lift_pct(25)$pct,   0.70)
check("35 mph  → 0.40",   get_lift_pct(35)$pct,   0.40)
check("45 mph  → 0.15",   get_lift_pct(45)$pct,   0.15)
check("60 mph  → 0.15",   get_lift_pct(60)$pct,   0.15)

section("get_terrain_weights — sums to 1.0")
for (ab in c("beginner", "intermediate", "advanced", "expert")) {
  check(paste(ab, "terrain weights sum to 1.0"), sum(get_terrain_weights(ab)), 1.0)
}


# =============================================================================
# SCENARIO TESTS — manual review
# Run the full composite score for realistic resort profiles and check that
# the numbers feel intuitively right.
# =============================================================================

run_scenario <- function(label, depth, snow72, temp, wind, precip,
                         snow_mmhr, ability) {
  w <- compute_weather_score(
    depth_in = depth, snowfall_72hr_in = snow72, temp_f = temp,
    wind_mph = wind, precip_mm_hr = precip, snowfall_mm_hr = snow_mmhr,
    ability  = ability
  )
  t <- compute_terrain_open_score(
    depth_in = depth, snowfall_72hr_in = snow72,
    wind_mph = wind,  ability = ability
  )
  cat(sprintf("\n  %-45s  weather=%.3f  terrain=%.3f\n", label, w$score, t$score))
  cat(sprintf("    depth=%.2f snow=%.2f temp=%.2f wind=%.2f | precip_mult=%.2f\n",
              w$components["depth_score"], w$components["snowfall_score"],
              w$components["temp_score"],  w$components["wind_score"],
              w$precip$multiplier))
  cat(sprintf("    terrain open: greens=%.0f%% blues=%.0f%% blacks=%.0f%% dbl=%.0f%%  lifts=%.0f%%\n",
              t$terrain_pcts["greens"]*100, t$terrain_pcts["blues"]*100,
              t$terrain_pcts["blacks"]*100, t$terrain_pcts["dbl_blacks"]*100,
              t$lift_pct*100))
  all_warn <- c(w$warnings, t$warnings)
  if (length(all_warn) > 0)
    cat(sprintf("    ⚠  %s\n", paste(all_warn, collapse = "\n    ⚠  ")))
}

cat("\n\n══════════════════════════════════════════════════════════════\n")
cat("SCENARIO TESTS  (review output manually — does it feel right?)\n")
cat("══════════════════════════════════════════════════════════════\n")

run_scenario("1. Perfect powder day (intermediate)",
             depth=60, snow72=14, temp=22, wind=8,  precip=0, snow_mmhr=0, ability="intermediate")
# Expect: weather ~0.90+, terrain ~0.99, no warnings

run_scenario("2. Great powder day (expert)",
             depth=55, snow72=20, temp=18, wind=5,  precip=0, snow_mmhr=0, ability="expert")
# Expect: high score, snowfall component dominant

run_scenario("3. Marginal early-season (beginner)",
             depth=28, snow72=3,  temp=34, wind=30, precip=0, snow_mmhr=0, ability="beginner")
# Expect: ~0.3–0.5, thin base warning, wind warning, very restricted terrain

run_scenario("4. Spring slush + light rain (intermediate)",
             depth=45, snow72=0,  temp=38, wind=10, precip=1.5, snow_mmhr=0, ability="intermediate")
# Expect: temp (0.1) + rain multiplier (0.6) tanks weather score

run_scenario("5. Thin base rescued by fresh snow (advanced)",
             depth=18, snow72=10, temp=24, wind=12, precip=0, snow_mmhr=0, ability="advanced")
# Expect: depth rescued to 0.2, terrain heavily restricted, warning shown

run_scenario("6. Extreme wind otherwise great (expert)",
             depth=70, snow72=5,  temp=10, wind=50, precip=0, snow_mmhr=0, ability="expert")
# Expect: wind score (0.05) collapses both scores despite deep base


# =============================================================================
# SUMMARY
# =============================================================================

cat(sprintf(
  "\n══════════════════════════════════════════════════════════════\n%d passed  |  %d failed\n══════════════════════════════════════════════════════════════\n\n",
  .passed, .failed
))
