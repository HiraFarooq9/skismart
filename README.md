# SkiSmart — Pipeline Documentation

**Authors:** Hira Farooq & Maren Roether
**Course:** GLHLTH 562 — Data Science for Global Health
**Last updated:** 2026-03-31

SkiSmart is a Shiny web application that recommends Colorado ski resorts to recreational skiers based on their preferences, current snow and avalanche conditions, and route safety.

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

## Project Structure

```
final project/
├── README.md                  # this file
├── app.R                      # Shiny UI + server (main application entry point)
├── score_resorts.R            # Stage 1: resort scoring master script
├── fetch_caic.R               # Stage 1: CAIC avalanche data fetching
├── R/
│   ├── weather_score.R        # Stage 1: weather scoring functions (Hira)
│   ├── terrain_open_score.R   # Stage 1: terrain accessibility scoring (Hira)
│   └── api_openmeteo.R        # Stage 1: Open-Meteo API fetch layer (Hira)
├── resorts - clean.csv        # static resort database (33 Colorado resorts)
├── launch_app.R               # helper: launches app with explicit path (Windows fix)
├── caic_zone_lookup.R         # (legacy) standalone zone lookup, superseded by fetch_caic.R
└── debug_caic_api.R           # (dev only) API exploration script, not part of pipeline
```

---

## Tech Stack

R/Shiny. Tidyverse (dplyr etc.) approved throughout. API calls via `httr2`. Date handling via `lubridate`. Spatial operations via `sf`.

---

## Data Sources

| Source | Purpose | Key | Cost |
|---|---|---|---|
| Static Resort CSV | Base resort attributes (33 CO resorts) | None | Free |
| Open-Meteo | Snowfall, snow depth, temp, wind forecasts | None | Free |
| CAIC (avalanche.state.co.us) | Avalanche danger by zone | None | Free |
| Colorado DOT 511 (cotrip.org) | Road closures, chain laws | Required (register) | Free |
| GraphHopper | Driving routes, distance, duration | Required | Free tier (500 req/day) |
| Nominatim / OpenStreetMap | Geocode user origin to coordinates | None | Free |
| Google Gemini (`gemini-2.0-flash-lite`) | Route conditions LLM summary | Required (AI Studio) | Free tier |

**Design principle:** No data is pre-downloaded or stored statically (except the resort CSV). Every app session fetches live data at runtime.

---

## User Inputs

| Input | Type | Notes |
|---|---|---|
| Origin city/location | Text | Geocoded via Nominatim |
| Ski date | Date (YYYY-MM-DD) | Used directly as the scoring anchor |
| Skier ability | One of: beginner, intermediate, advanced, expert | Single input per session |
| Terrain preferences | TBD | Used in terrain match score |
| Resort vibe | TBD | Used in terrain match score |
| Vehicle type | TBD | Used in route risk scoring |
| Chains available | Yes/No | Used in feasibility check |
| Max drive time | Hours | Hard block in feasibility check |
| Risk tolerance | 0–1 | Scales risk penalty weights |
| Season pass | None / Ikon / Epic | Used in resort filtering |

---

## 1. Data Source Origin

### 1a. Static Resort Database
- **File:** `resorts - clean.csv`
- **Origin:** Manually compiled from resort websites, OnTheSnow, and Wikipedia
- **Coverage:** 33 Colorado ski resorts
- **Key fields:** `resort_id`, `resort_name`, `latitude`, `longitude`, `elevation_base_ft`, `elevation_summit_ft`, `vertical_drop_ft`, `trails_total`, `trails_green_pct`, `trails_blue_pct`, `trails_black_pct`, `trails_double_black_pct`, `lifts_total`, `total_acres`, `snowmaking_acres`, `season_open`, `season_close`
- **Note:** `resort_id = 9` is intentionally absent (gap in original source data)

### 1b. CAIC Avalanche Forecasts
- **Source:** Colorado Avalanche Information Center (CAIC)
- **API base:** `https://avalanche.state.co.us/api-proxy/avid`
- **Auth:** None — free, no API key required
- **Update frequency:** Once daily
- **Coverage:** All Colorado backcountry forecast zones

### 1c. Open-Meteo Weather Forecasts
- **Source:** Open-Meteo (open-meteo.com)
- **Auth:** None — free, no API key required
- **Fields used:** snow depth, 72h snowfall, temperature forecast, precipitation, wind speed

### 1d. User Input
- **Source:** Shiny UI reactive inputs — `app.R`
- **Fields:** ski date (up to 10 days ahead), max drive time (hours), ability level, terrain preference, season pass type, risk tolerance
- **How it works:** All inputs are collected in a named list and passed to `score_resorts(user_inputs)` when the user clicks Find Resorts

---

## 2. Data Ingestion

### 2a. Static Resort Database
Read at script load time using `readr::read_csv()` in `score_resorts.R`:

```r
resorts_raw <- read_csv("resorts - clean.csv", col_types = cols(...))
```

### 2b. CAIC Avalanche API — How We Found the Endpoint

The CAIC API endpoint was not publicly documented. We identified it by inspecting network traffic in the browser (F12 → Network → Fetch/XHR) while loading `https://avalanche.state.co.us/forecasts/backcountry`. This revealed two relevant endpoints:

| Endpoint | Returns | Used for |
|---|---|---|
| `/products/all` | JSON array of forecast objects with danger ratings | Danger ratings per zone |
| `/products/all/area?productType=avalancheforecast&includeExpired=true` | GeoJSON FeatureCollection of zone polygons | Zone boundary shapes for spatial matching |

**Key discovery:** The API does not return human-readable zone names (like "Vail & Summit County") in the forecast objects. Instead, each forecast has an `areaId` hash. The GeoJSON zone polygons carry the same `id` in their properties. Resort coordinates are spatially matched to zone polygons to obtain the `areaId`, which then links each resort to its forecast.

Both calls are made using `httr2::request()` with a 15-second timeout. Results are cached in an R environment (`.caic_cache`) for the duration of the session to avoid redundant API calls on Shiny re-renders.

```r
# Two API calls regardless of how many resorts are queried
url_forecasts <- "https://avalanche.state.co.us/api-proxy/avid?_api_proxy_uri=%2Fproducts%2Fall"
url_polygons  <- "https://avalanche.state.co.us/api-proxy/avid?_api_proxy_uri=%2Fproducts%2Fall%2Farea%3F..."
```

### 2c. Open-Meteo Weather API

Fetched via `R/api_openmeteo.R` using `httr2`. No API key required. Uses `past_days = 3` to capture the 72-hour snowfall lookback window and `forecast_days = 10` to match the product forecast horizon.

---

## 3. Data Processing

### 3a. Avalanche Data (`fetch_caic.R`)

**Step 1 — Fetch all forecasts**
`/products/all` returns a JSON array of ~20 forecast objects covering all Colorado zones. Each object contains:

```json
{
  "areaId": "38f7df6...",
  "issueDateTime": "2026-03-30T22:30:00Z",
  "dangerRatings": {
    "days": [
      { "position": 1, "alp": "considerable", "tln": "moderate", "btl": "low", "date": "..." },
      { "position": 2, "alp": "moderate",      "tln": "low",      "btl": "low", "date": "..." }
    ]
  },
  "avalancheProblems": {
    "days": [ [{ "name": "Wind Slab" }, { "name": "Storm Slab" }], ... ]
  }
}
```

- `alp` = above treeline (alpine), `tln` = at treeline, `btl` = below treeline
- `days[1]` = today's forecast, `days[2]` = tomorrow's forecast
- Ratings are lowercase strings: `"low"`, `"moderate"`, `"considerable"`, `"high"`, `"extreme"`, `"noRating"`

**Step 2 — Fetch zone polygons (GeoJSON)**
`/products/all/area` returns a GeoJSON FeatureCollection where each feature is a MultiPolygon for one forecast zone. The feature's `properties.id` equals the `areaId` in the forecast objects.

**Step 3 — Spatial join**
Resort coordinates are converted to `sf` point objects and matched against zone polygons using `sf::st_join(..., join = sf::st_within)`:

```r
resorts_sf <- sf::st_as_sf(resorts, coords = c("lon", "lat"), crs = 4326)
joined     <- sf::st_join(resorts_sf, zone_polys["id"], join = sf::st_within, left = TRUE)
```

**Step 4 — Parse and normalise**
Each forecast object is parsed by `.parse_forecast()`. Danger rating strings are normalised to title case. The overall danger rating is derived as the highest of the three elevation bands. A numeric encoding (1–5) is added for use in the scoring formula.

**Output schema of `get_avalanche_data_for_resorts()`:**

| Column | Type | Description |
|---|---|---|
| `resort_id` | int | matches resort CSV |
| `resort_name` | chr | |
| `caic_zone` | chr | human-readable zone name (from hardcoded lookup) |
| `forecast_date` | date | date forecast was issued |
| `danger_overall` | chr | highest of the three bands today |
| `danger_numeric` | int | 1 (Low) – 5 (Extreme), NA for No Rating |
| `danger_alpine` | chr | above-treeline rating today |
| `danger_treeline` | chr | at-treeline rating today |
| `danger_below_treeline` | chr | below-treeline rating today |
| `danger_tomorrow_overall` | chr | highest band tomorrow |
| `danger_tomorrow_numeric` | int | 1–5, tomorrow |
| `danger_tomorrow_alpine` | chr | |
| `danger_tomorrow_treeline` | chr | |
| `danger_tomorrow_below_treeline` | chr | |
| `problem_types` | chr | comma-separated avalanche problem names (today) |

---

### 3b. Weather Score (`R/weather_score.R`)

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

*Temperature (step function):*
- < 0°F → 0.5 | 0–15°F → 0.8 | 15–30°F → 1.0 | 30–32°F → 0.7 | 32–35°F → 0.3 | 35°F+ → 0.1

*Wind (step function):*
- < 15 mph → 1.0 | 15–25 → 0.7 | 25–35 → 0.4 | 35–45 → 0.2 | 45+ → 0.05

*Precipitation multiplier:*
- No precip or snow (temp < 30°F) → 1.0
- Mixed/sleet (30–34°F) → 0.7
- Light rain (< 2.5 mm/hr) → 0.6 | Moderate rain → 0.4 | Heavy rain (> 7.5 mm/hr) → 0.2
- Override: if snowfall_mm_hr > 5 at 30–34°F, treat as snow (1.0)

**Key design decisions:**

1. **Precipitation as a multiplier, not additive.** Rain can destroy an otherwise good score regardless of depth or fresh snow.
2. **72-hour snowfall window.** Consistent with the API lookback (`past_days = 3`).
3. **Piecewise linear for depth/snowfall; step functions for temperature/wind.** Depth and snowfall benefit from smooth transitions; temperature and wind bands represent categorically different condition types.
4. **No hard exclude on snow depth.** Resort operating status is checked separately via `season_open`/`season_close` (feasibility, Stage 3). Depth purely scores condition quality on a 0–1 scale.

---

### 3c. Terrain Open Score (`R/terrain_open_score.R`)

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

**Fresh snowfall bump (step function, additive, capped at tier maximums):**
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

### 3d. Open-Meteo API Layer (`R/api_openmeteo.R`)

Fetches and unit-converts live weather data for a resort coordinate. No API key required.

| Function | Purpose |
|---|---|
| `fetch_weather_openmeteo(lat, lon, ski_date)` | Fetches all scoring inputs for one resort |
| `fetch_weather_all_resorts(resorts_df, ski_date)` | Iterates over all resorts, returns named list |

**Output fields (naming matches scoring function parameters exactly):**

| Field | Units | Aggregation |
|---|---|---|
| `depth_in` | inches | Midday snapshot on ski_date |
| `snowfall_72hr_in` | inches | Sum over 72hrs before ski_date 00:00 |
| `temp_f` | °F | Mean over ski hours (8am–4pm) |
| `wind_mph` | mph | Max over ski hours (worst-case) |
| `precip_mm_hr` | mm/hr | Max over ski hours (worst-case) |
| `snowfall_mm_hr` | mm/hr | Mean over ski hours (for precip type classification only) |

---

### 3e. Terrain Match Score (`score_resorts.R`)

Measures how well a resort's trail difficulty distribution matches what is appropriate for the user's ability level.

**Target distributions:**

| Ability | Green | Blue | Black | Dbl Black |
|---|---|---|---|---|
| Beginner | 50% | 40% | 10% | 0% |
| Intermediate | 10% | 50% | 30% | 10% |
| Advanced | 0% | 20% | 50% | 30% |
| Expert | 0% | 10% | 30% | 60% |

**Formula:**
```
deviation     = mean(|actual_pct - target_pct|)  across four difficulty bands
terrain_score = max(0, 100 - deviation × 200)
```

A perfect match scores 100. A mean deviation of 0.5 (complete mismatch) scores 0.

**Example:** Breckenridge (14% green, 31% blue, 19% black, 36% double black) for an expert user:
```
deviation = mean(0.14, 0.21, 0.11, 0.24) = 0.175
score     = 100 - 0.175 × 200 = 65
```

---

### 3f. Conditions Risk Score (`score_resorts.R`)

Converts `danger_numeric` (1–5) to a 0–100 risk penalty, adjusted by user risk tolerance:

```
base_risk     = (danger_numeric - 1) / 4 × 100
adjusted_risk = base_risk × (1 - risk_tolerance × 0.4)
```

A user with high risk tolerance (1.0) reduces the penalty by 40%.

---

### 3g. Composite Score (`score_resorts.R`)

**Current formula** (while weather data is placeholder):
```
composite = terrain_score × 0.70 + (100 - risk_score) × 0.30
```

**Planned formula** once Open-Meteo weather data is integrated:
```
composite = snow_score × 0.40 + terrain_score × 0.35 + (100 - risk_score) × 0.25
```

**Hard blocks** — resorts are removed from ranking entirely if:
- `danger_numeric >= 4` (High or Extreme avalanche danger)
- *(planned)* Road closed with no alternate route (CDOT 511)
- *(planned)* Resort closed for the season

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

### Stage 3 — Feasibility Rules

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

## 4. Output

### 4a. Pipeline Output (`score_resorts.R`)

`score_resorts()` returns a named list:

| Element | Contents |
|---|---|
| `$top` | Top N ranked resorts (default 3) with all scores |
| `$blocked` | Resorts removed due to hard blocks, with reasons |
| `$all` | Full scored dataframe for all 33 resorts |
| `$inputs` | User inputs used for this run |
| `$timestamp` | When the pipeline was run |

### 4b. Shiny App Output (`app.R`)

The app displays three panels in the main area:

| Panel | Contents |
|---|---|
| **Recommended Resorts** | Top 3 ranked resorts: composite score, terrain match, avalanche risk, today/tomorrow danger ratings, problem types, CAIC zone |
| **Resorts Excluded** | Only shown if any resorts were hard-blocked; lists resort name, danger rating, and reason |
| **Current Avalanche Conditions** | Full table of all resorts with today/tomorrow ratings, sorted by danger level |

### 4c. Planned Output Dashboard

- **Map** — route colored green/yellow/red by risk; avalanche zone overlays; resort pins with score badges; rejected resorts marked with reason
- **Recommended resort panel** — score breakdown, estimated terrain open
- **Departure timing chart** — hourly risk curve, optimal departure/return window
- **Flagged route segments**
- **LLM-generated trip briefing** — plain-language recommendation, safety advisories, gear list calibrated to forecast temperatures

A loading notification appears while the CAIC API is being called. All outputs update only when the user clicks **Find Resorts** (not on every input change), to avoid unnecessary API calls.

---

## 5. Replication Instructions

### Dependencies

```r
install.packages(c("shiny", "httr2", "dplyr", "purrr", "readr", "lubridate", "sf"))
```

The `sf` package requires system-level GDAL/GEOS libraries. On Windows these are bundled with the CRAN binary — a standard `install.packages("sf")` is sufficient.

### Running the Shiny App

**Recommended:** Open `app.R` in RStudio and click the **Run App** button in the top-right of the editor pane.

Alternatively, open `launch_app.R` and click **Source** — this sets the working directory explicitly before launching (useful on Windows).

### Running the Scoring Pipeline Directly (Without UI)

```r
setwd("path/to/final project")
source("score_resorts.R")

# With default inputs
result <- score_resorts()
print(result$top)

# With custom inputs
result <- score_resorts(user_inputs = list(
  ability_level   = "expert",
  max_drive_hours = 3,
  risk_tolerance  = 0.8
))
print(result$top)
print(result$blocked)
```

### API Keys

None required for current modules. The CAIC API is free and open. Open-Meteo is also free with no key. Future modules (OpenRouteService, Anthropic/OpenAI) require keys — see proposal for details.

### Cache Behaviour

CAIC data is cached in memory for the R session. To force a fresh API call:

```r
clear_caic_cache()
result <- score_resorts()
```

---

## Pending Modules

| Module | File | Status | Feeds into |
|---|---|---|---|
| Weather forecast fetch | `fetch_weather.R` | Not started | `snow_score` in composite |
| Route risk scoring | — | Not started | Stage 2 |
| CDOT road conditions | — | Not started | Hard blocks |
| Departure optimizer | — | Not started | Stage 4 |
| LLM synthesis | — | Not started | Stage 5 |
| Leaflet map | `app.R` | Not started | Resort pins, route overlay |
| Origin city input | `app.R` | Not started | Drive time filtering (needs OpenRouteService) |

---

## Development Log

### Session 1 — Project Initialization
- Defined product scope and two-layer architecture
- Curated static resort CSV (`resorts - clean.csv`) with 33 Colorado resorts
- Established data source inventory and API access plan
- Initialized repository structure

### Session 3 — Stage 2 Routing Pipeline + Terrain Score Fix (Hira)
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

### Session 2 — Stage 1 Scoring Modules (Hira)
- Built `R/weather_score.R` — 7 functions, full weather composite with ability weights
- Built `R/terrain_open_score.R` — 5 functions, terrain accessibility with lift adjustment
- Built `R/api_openmeteo.R` — Open-Meteo fetch + unit conversion layer
- Built `tests/test_scoring.R` — standalone unit + scenario tests
- Confirmed: single ability input per session; `ski_date` = trip date entered by user
- Removed hard exclude on snow depth < 24" — resort open/closed handled by season dates in CSV

### Session 2 — Stage 1 Avalanche + App Scaffold (Maren)
- Built `fetch_caic.R` — CAIC AVID API fetch with `sf` spatial join to match resorts to zone polygons
- Built `score_resorts.R` — master Stage 1 scoring script (terrain match + conditions risk + composite)
- Built `app.R` / `launch_app.R` — Shiny UI skeleton with CAIC danger output panel
- Identified undocumented CAIC API endpoints via browser network inspection

**Next:** Integrate Hira's weather/terrain-open scores into `score_resorts.R`, then begin Stage 2 route risk.

---

*This document is updated continuously as the project develops.*
