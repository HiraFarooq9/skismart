# SkiSmart

A web application that helps recreational skiers plan ski trips in Colorado by synthesizing resort conditions, weather forecasts, and road conditions into a single ranked recommendation.

---

## Problem Statement

Planning a ski trip currently requires manually checking multiple sources: resort websites for lift/trail status, weather services for snowfall forecasts, and state DOT portals for road conditions. SkiSmart answers the core question — *"Where should I ski, and how do I get there?"* — by pulling all of these into one place.

---

## Architecture Overview

The app is built around a five-stage data pipeline:

```
User Inputs
    → Stage 1: Resort Scoring
    → Stage 2: Route Risk Scoring
    → Stage 3: Feasibility + Fallback
    → Stage 4: Departure Optimizer
    → Stage 5: LLM Synthesis
    → Output Dashboard
```

Layer 1 (MVP) covers Stages 1–3 and the dashboard. Layer 2 adds the full route risk map and departure optimizer once Layer 1 is stable.

---

## Tech Stack

R/Shiny. Tidyverse (dplyr etc.) approved throughout. API calls via `httr2`. Date handling via `lubridate`.

---

## Data Sources

| Source | Purpose | Key | Cost |
|---|---|---|---|
| Static Resort CSV | Base resort attributes (~30 CO resorts) | None | Free |
| Open-Meteo | Snowfall, snow depth, temp, wind forecasts | None | Free |
| Colorado DOT 511 (cotrip.org) | Road closures, chain laws | Required (register) | Free |
| CAIC (avalanche.state.co.us) | Avalanche danger by zone | None | Free |
| GraphHopper | Driving routes, distance, duration | Required | Free tier (500 req/day) |
| Nominatim / OpenStreetMap | Geocode user origin to coordinates | None | Free |
| Google Gemini (`gemini-2.0-flash-lite`) | Route conditions LLM summary | Required (AI Studio) | Free tier |

**Design principle:** No data is pre-downloaded or stored statically (except the resort CSV). Every app session fetches live data at runtime.

---

## Static Resort Database

`data/resorts.csv` — manually curated from OnTheSnow, Uncovercolorado, and official resort websites.

**Fields:** resort_id, resort_name, latitude, longitude, elevation_base_ft, elevation_summit_ft, vertical_drop_ft, trails_total, trails_green_pct, trails_blue_pct, trails_black_pct, trails_double_black_pct, lifts_total, total_acres, snowmaking_acres, season_open, season_close, resort_url

Covers ~30 major Colorado resorts. Chosen over live scraping for reliability and reproducibility.

---

## User Inputs

| Input | Type | Notes |
|---|---|---|
| Origin city/location | Text | Geocoded via Nominatim |
| Ski date | Date (YYYY-MM-DD) | The trip date; used directly as the scoring anchor |
| Skier ability | One of: beginner, intermediate, advanced, expert | Single input per session — not per person |
| Terrain preferences | TBD | Used in terrain match score (teammate) |
| Resort vibe | TBD | Used in terrain match score (teammate) |
| Vehicle type | TBD | Used in route risk scoring |
| Chains available | Yes/No | Used in feasibility check |
| Max drive time | Hours | Hard block in feasibility check |
| Risk tolerance | TBD | Scales route risk weights |
| Season pass | None / Ikon / Epic | Used in resort filtering |

---

## Scoring Logic

### Stage 1 — Resort Scoring

Each resort receives a composite score built from four components:

| Component | Owner | Status |
|---|---|---|
| Weather score | Hira | Complete |
| Terrain open score | Hira | Complete |
| Avalanche / conditions risk score | Teammate | In progress |
| Terrain match to user ability | Teammate | In progress |

Top 3 resorts by weighted composite advance to route scoring.

---

### Weather Score (`R/weather_score.R`)

Scores the quality of skiing conditions on a 0–1 scale.

**Formula:**
```
weather_score = (w_depth * depth_score
               + w_snowfall * snowfall_score
               + w_temp * temp_score
               + w_wind * wind_score) * precip_multiplier
```

**Functions:**

| Function | Purpose |
|---|---|
| `score_snow_depth(depth_in)` | Base depth → 0–1, piecewise linear |
| `score_snowfall(snowfall_in)` | 72hr accumulation → 0–1, piecewise linear |
| `get_precip_multiplier(temp_f, precip_mm_hr, snowfall_mm_hr)` | Rain/mixed/snow classification → multiplier (0.2–1.0) |
| `score_temperature(temp_f)` | Temperature → 0–1, step function |
| `score_wind(wind_mph)` | Wind speed → 0–1, step function |
| `get_weather_weights(ability)` | Returns ability-specific weight vector |
| `compute_weather_score(...)` | Orchestrates all components into final score |

**Ability weights:**

| Variable | Beginner | Intermediate | Advanced | Expert |
|---|---|---|---|---|
| Base depth | 0.45 | 0.40 | 0.35 | 0.30 |
| Fresh snowfall | 0.15 | 0.25 | 0.35 | 0.40 |
| Temperature | 0.20 | 0.15 | 0.15 | 0.15 |
| Wind | 0.20 | 0.20 | 0.15 | 0.15 |

**Scoring breakpoints:**

*Snow depth (piecewise linear):*
- 0" → 0.0 — 24" → 0.1 — 30" → 0.2 — 36" → 0.5 — 48" → 0.8 — 72" → 0.9 — 100"+ → 1.0

*Fresh snowfall (piecewise linear):*
- 0" → 0.0 — 1" → 0.1 — 3" → 0.3 — 6" → 0.6 — 12" → 0.9 — 18"+ → 1.0

*Temperature (step function — flat value per band):*
- < 0°F → 0.5 (extreme cold warning) | 0–15°F → 0.8 | 15–30°F → 1.0 | 30–32°F → 0.7 | 32–35°F → 0.3 | 35°F+ → 0.1

*Wind (step function — flat value per band):*
- < 15 mph → 1.0 | 15–25 → 0.7 | 25–35 → 0.4 | 35–45 → 0.2 | 45+ → 0.05

*Precipitation multiplier:*
- No precip or snow (temp < 30°F) → 1.0
- Mixed/sleet (30–34°F) → 0.7
- Light rain (< 2.5 mm/hr) → 0.6 | Moderate rain (2.5–7.5 mm/hr) → 0.4 | Heavy rain (> 7.5 mm/hr) → 0.2
- Override: if snowfall_mm_hr > 5 at 30–34°F, treat as snow (1.0)

**Key design decisions:**

1. **Precipitation as a multiplier, not additive.** Rain can destroy an otherwise good score regardless of depth or fresh snow — a 0.2 multiplier for heavy rain reflects real conditions.

2. **72-hour snowfall window.** Consistent across both the snowfall score and the API lookback (`past_days = 3`).

3. **Snow depth and snowfall use piecewise linear interpolation; temperature and wind use step functions.** Depth and snowfall benefit from smooth transitions (a resort at 35" should score meaningfully better than one at 30"). Temperature and wind bands represent categorically different condition types, so flat values per band are appropriate.

4. **Precipitation type derived from temperature + snowfall rate**, not a separate API field: temp < 30°F → snow; 30–34°F → mixed (unless snowfall_mm_hr > 5, then snow override); > 34°F → rain.

5. **No hard exclude on snow depth.** The original design excluded resorts with < 24" base. Removed after observing that base-lodge coordinates can read 0" while upper mountain is fully operational. Resort operating status is checked separately via `season_open`/`season_close` dates in the CSV (feasibility, Stage 3). Depth purely scores condition quality on a continuous 0–1 scale.

---

### Terrain Open Score (`R/terrain_open_score.R`)

Estimates how much of a skier's preferred terrain is accessible — distinct from weather score which measures condition quality.

**Design (v2):** Scores on **absolute open trail counts**, not percentages. A resort with 50 open trails ranks above one with 7 open trails even if the smaller resort has a higher % open. Raw scores are normalized to 0–1 across all candidate resorts at the orchestration layer.

```
open_trails_per_tier = trails_total × tier_share × estimated_pct_open
weighted_open_count  = sum(open_trails_per_tier × ability_weights)
raw_score            = weighted_open_count × lift_pct

# After scoring all resorts:
normalized_score = (raw_score - min) / (max - min)
```

**Functions:**

| Function | Purpose |
|---|---|
| `get_terrain_pct_by_depth(depth_in)` | Snow depth → estimated % open per trail tier, piecewise linear |
| `apply_snowfall_bump(terrain_pcts, snowfall_72hr_in)` | Fresh snow bonus per tier, respects tier caps |
| `get_lift_pct(wind_mph)` | Wind speed → estimated % of lifts operating, step function |
| `get_terrain_weights(ability)` | Returns ability-specific tier weight vector |
| `compute_terrain_open_score(depth_in, snowfall_72hr_in, wind_mph, ability, trails_total, trail_mix)` | Returns raw ability-weighted open trail count (unnormalised) |
| `normalize_terrain_scores(raw_scores)` | Scales a named vector of raw scores to 0–1 across all resorts |

**New required inputs to `compute_terrain_open_score()`:**
- `trails_total` — total trail count from `resorts.csv`
- `trail_mix` — named vector: `c(greens=0.20, blues=0.40, blacks=0.30, dbl_blacks=0.10)` derived from CSV percentage columns

**Depth → terrain open % (piecewise linear per tier):**

| Snow Depth | Greens | Blues | Blacks | Dbl Blacks |
|---|---|---|---|---|
| 0" | 0% | 0% | 0% | 0% |
| 24" | 60% | 20% | 0% | 0% |
| 30" | 80% | 50% | 10% | 0% |
| 36" | 95% | 75% | 40% | 10% |
| 48" | 100% | 90% | 70% | 40% |
| 72"+ | 100% | 100% | 90% | 70% |

**Fresh snowfall bump (step function, additive, capped at tier maximums above):**
- 0–3" → no bump | 3–6" → +5% | 6–12" → +10% | 12"+ → +15%

**Wind → lift operating % (step function):**
- < 15 mph → 100% | 15–25 → 90% | 25–35 → 70% | 35–45 → 40% | 45+ → 15%

**Ability weights by trail tier:**

| Tier | Beginner | Intermediate | Advanced | Expert |
|---|---|---|---|---|
| Greens | 0.70 | 0.30 | 0.10 | 0.10 |
| Blues | 0.30 | 0.60 | 0.30 | 0.10 |
| Blacks | 0.00 | 0.10 | 0.50 | 0.30 |
| Double Blacks | 0.00 | 0.00 | 0.10 | 0.50 |

**Key design decisions:**

1. **Absolute counts over percentages.** A large resort with 50% of 200 trails open (100 trails) outranks a small resort with 70% of 10 trails open (7 trails). Percentages alone distort the score in favor of small resorts.

2. **Normalization at the orchestration layer.** Raw scores are trail counts (not 0–1), so `normalize_terrain_scores()` must be called after scoring all resorts to scale relative to the best candidate in the set.

3. **Tier caps on snowfall bump.** Blacks cap at 90%, double blacks at 70% — operational limits fresh snow alone cannot overcome.

4. **Lift pct as a multiplier.** Wind-driven closures scale the entire score. 40% of lifts running means even fully open terrain is less reachable.

---

### Open-Meteo API Layer (`R/api_openmeteo.R`)

Fetches and unit-converts live weather data for a resort coordinate. No API key required.

**Functions:**

| Function | Purpose |
|---|---|
| `fetch_weather_openmeteo(lat, lon, ski_date)` | Fetches all scoring inputs for one resort |
| `fetch_weather_all_resorts(resorts_df, ski_date)` | Iterates over all resorts, returns named list |

**Output of `fetch_weather_openmeteo()` — naming matches scoring function parameters exactly:**

| Field | Units | Aggregation |
|---|---|---|
| `depth_in` | inches | Midday snapshot on ski_date |
| `snowfall_72hr_in` | inches | Sum over 72hrs before ski_date 00:00 |
| `temp_f` | °F | Mean over ski hours (8am–4pm) |
| `wind_mph` | mph | Max over ski hours (worst-case) |
| `precip_mm_hr` | mm/hr | Max over ski hours (worst-case) |
| `snowfall_mm_hr` | mm/hr | Mean over ski hours (for precip type classification only) |

**Key design decisions:**

1. **`past_days = 3` + `forecast_days = 10`.** The 3-day lookback retrieves the 72hr snowfall window. 10-day forward matches the product forecast horizon.

2. **`ski_date` = the trip date entered by the user.** Used directly as the scoring anchor — no arrival/departure distinction.

3. **Wind and precipitation use max over ski hours; temperature uses mean.** Conservative (worst-case) for anything affecting safety or access.

4. **Error isolation per resort.** `fetch_weather_all_resorts()` returns `list(error = ...)` per failed resort rather than crashing the pipeline.

---

### Stage 2 — Route Risk Score

For each of the top 3 candidate resorts, a driving route is fetched and CDOT road condition data is filtered to the route corridor. Gemini produces a plain-language summary and risk level.

**Pipeline:**
```
User start address → geocode (Nominatim) → lat/lon
lat/lon + resort coords → GraphHopper → route geometry + drive time
Route bbox → CDOT /roadConditions → filtered condition rows
Filtered rows → Gemini prompt → risk level + summary + key action
Drive time vs user max → soft penalty applied to resort ranking
```

**Modules:**

| File | Purpose |
|---|---|
| `R/api_geocode.R` | Geocodes user address/city to lat/lon via Nominatim |
| `R/api_routing.R` | Fetches driving route from GraphHopper |
| `R/api_cdot.R` | Fetches CDOT road conditions (one row per condition entry per segment) |
| `R/route_conditions.R` | Spatial filter + risk level derivation + Gemini prompt builder |
| `R/llm_route_summary.R` | Calls Gemini, parses structured response |

**CDOT data structure (confirmed from live API):**
- Single endpoint: `/roadConditions`
- Each segment has `primaryLatitude/Longitude` + `secondaryLatitude/Longitude` for spatial filtering
- Conditions nested in `currentConditions[]` — one entry per condition type (forecast, surface, chain law, etc.)
- `additionalData` field contains free text passed to the LLM

**Gemini output format:**
```
RISK_LEVEL: low | moderate | high | do_not_travel
SUMMARY: 2-3 sentence plain-language description
KEY_ACTION: single most important driver action, or "None"
```

**Drive time integration:**
- Route returns `duration_mins` under normal conditions
- Compared to user's max drive time input
- Penalty multiplier applied to resort's final score:
  - Within max → 1.0 (no penalty)
  - Up to 20% over → 0.75
  - More than 20% over → 0.4
- If all resorts exceed max, drop multipliers and rank by conditions score; LLM communicates the tradeoff

### Feasibility Rules (Stage 3)

**Hard blocks** (resort rejected):
- Road closed with no alternate
- Avalanche HIGH/EXTREME on route
- Resort closed for the season
- Drive time exceeds user maximum

**Soft flags** (warning shown):
- Chains required but user has none
- Storm forecast during drive window
- Snow base below 30"

If the top resort is blocked, the app falls back to #2, then #3, and explains each rejection.

---

## Output Dashboard

- **Map** — route colored green/yellow/red by risk; avalanche zone overlays; resort pins with score badges; rejected resorts marked with reason
- **Recommended resort panel** — score breakdown, estimated terrain open
- **Departure timing chart** — hourly risk curve, optimal departure/return window
- **Flagged route segments**
- **LLM-generated trip briefing** — plain-language recommendation, safety advisories, gear list calibrated to forecast temperatures

---

## Scope

- **Geography:** Colorado resorts (initial). Expansion to additional states if time permits.
- **Forecast horizon:** Up to 10 days (Open-Meteo reliable range; historical climatology as fallback beyond 14 days).

---

## Development Log

### Session 1 — Project Initialization
- Defined product scope and two-layer architecture
- Curated static resort CSV (`data/resorts.csv`) with ~30 Colorado resorts
- Established data source inventory and API access plan
- Initialized repository structure (`data/`, `R/`)

### Session 3 — Stage 2 Routing Pipeline + Terrain Score Fix
- Built `R/load_env.R` — loads API keys from `.env` file (never committed)
- Built `R/api_geocode.R` — Nominatim geocoder, no key required, biased toward Colorado
- Built `R/api_routing.R` — GraphHopper driving route fetch (distance, duration, waypoints)
- Built `R/api_cdot.R` — CDOT `/roadConditions` integration; confirmed live API field names; one row per condition entry
- Built `R/route_conditions.R` — spatial corridor filter + keyword-based risk level + Gemini prompt builder
- Built `R/llm_route_summary.R` — Gemini `gemini-2.0-flash-lite` call; structured response parser
- Built `tests/test_stage2.R` — interactive section-by-section test script
- Updated `R/terrain_open_score.R` — switched from % open to absolute open trail counts; added `normalize_terrain_scores()`; `compute_terrain_open_score()` now requires `trails_total` and `trail_mix` parameters
- Confirmed CDOT has only one working endpoint (`/roadConditions`); `/closures` and `/restrictions` return 404
- Confirmed GraphHopper as routing provider (ORS account activation issues; OSRM public server unreliable)
- Confirmed Gemini model: `gemini-2.0-flash-lite` on AI Studio key (free tier)
- Designed drive time penalty logic: within max → 1.0; up to 20% over → 0.75; >20% over → 0.4; all exceed → rank by conditions, LLM communicates tradeoff

### Session 2 — Stage 1 Scoring Modules
- Built `R/weather_score.R` — 7 functions, full weather composite with ability weights
- Built `R/terrain_open_score.R` — 5 functions, terrain accessibility with lift adjustment
- Built `R/api_openmeteo.R` — Open-Meteo fetch + unit conversion layer
- Built `tests/test_scoring.R` — standalone unit + scenario tests (run with `source("tests/test_scoring.R")`)
- Confirmed tech stack: R/Shiny, tidyverse approved
- Confirmed single ability input per session (not per person)
- Confirmed ski_date = trip date entered by user (no arrival/departure split)
- Removed hard exclude on snow depth < 24" — resort open/closed handled by season dates in CSV
- Removed thin-base rescue clause — fresh snowfall captured independently by `score_snowfall()`
- Fixed naming consistency: API output field names now match scoring function parameter names exactly

**Next:** Teammate integrates avalanche/conditions risk score and terrain match, then combines all Stage 1 components into the resort ranker.

---

*This document is updated continuously as the project develops.*
