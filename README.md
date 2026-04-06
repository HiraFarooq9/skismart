# SkiSmart — Pipeline Documentation

**Authors:** Hira Farooq & Maren Roether
**Course:** GLHLTH 562 — Data Science for Global Health
**Last updated:** 2026-04-07

**Live app:** https://marenroe.shinyapps.io/ski_smart/

SkiSmart is a Shiny web application that recommends Colorado ski resorts to recreational skiers based on their preferences, current snow and avalanche conditions, and route safety.

---

## Deployment

The app is hosted on shinyapps.io at **https://marenroe.shinyapps.io/ski_smart/**

To redeploy after making local changes, open the project in RStudio (via `skismart.Rproj`) and run:

```r
rsconnect::deployApp(appDir = getwd(), appName = "ski_smart")
```

API keys are bundled at deploy time via a `.Renviron` file in the project root (gitignored). The free shinyapps.io tier does not expose environment variables through the dashboard UI.

---

## Demo Mode

A **Demo Mode** toggle is available in the sidebar for presentations and demonstrations. When enabled, the app bypasses all live API calls and displays static peak-season (January) data for five Front Range / Summit County resorts departing from Denver:

| Resort | Score | Drive |
|---|---|---|
| Breckenridge Ski Resort | 0.84 | 1h 32m · 79 mi |
| Keystone Resort | 0.78 | 1h 28m · 76 mi |
| Copper Mountain | 0.74 | 1h 35m · 81 mi |
| Vail Ski Resort | 0.69 | 1h 55m · 97 mi |
| Steamboat Ski Resort | 0.63 | 2h 42m · 157 mi |

Demo mode includes simulated route polylines with color-coded road conditions (Vail Pass and Rabbit Ears Pass shown as moderate/orange), pre-written LLM score interpretations, weather sparkline charts, and avalanche ratings — all without consuming API credits. Uncheck Demo Mode to return to live data.

---

## Problem Statement

Planning a ski trip currently requires manually checking multiple sources: resort websites for lift/trail status, weather services for snowfall forecasts, and state DOT portals for road conditions. SkiSmart answers the core question — *"Where should I ski, and how do I get there?"* — by pulling all of these into one place.

---

## Architecture Overview

The app is built around a four-stage data pipeline:

```
User Inputs
    → Stage 1: Resort Scoring
    → Stage 2: Route Risk Scoring
    → Stage 3: Feasibility + Fallback
    → Stage 4: LLM Synthesis
    → Output Dashboard
```

Layer 1 (MVP) covers Stages 1–3 and the dashboard. Layer 2 adds the full route risk map and LLM synthesis once Layer 1 is stable.

---

## Project Structure

```
skismart/
├── README.md                  # this file
├── app.R                      # Shiny UI + server — full Stage 1 + Stage 2 pipeline
├── app_demo.R                 # [development only — superseded by built-in Demo Mode in app.R]
├── launch_app.R               # helper: launches app with explicit path (Windows fix)
├── .env                       # [development only — superseded by .Renviron for deployment]
├── .env.example               # [development only — superseded by .Renviron for deployment]
├── data/
│   ├── resorts.csv            # static resort database (33 Colorado resorts)
│   └── cities.csv             # ~70 starting cities with lat/lon (CO, WY, UT, NM + surrounding)
├── R/
│   ├── composite_score.R      # Stage 1: master scoring orchestrator (Maren)
│   ├── fetch_caic.R           # Stage 1: CAIC avalanche fetch + spatial join (Maren)
│   ├── api_openmeteo.R        # Stage 1: Open-Meteo weather fetch + unit conversion (Hira)
│   ├── weather_score.R        # Stage 1: weather quality scoring functions (Hira)
│   ├── terrain_open_score.R   # Stage 1: terrain open score — absolute trail counts (Hira)
│   ├── load_env.R             # [development only — loads API keys from .env; superseded by .Renviron]
│   ├── api_geocode.R          # Stage 2: Nominatim geocoder (Hira)
│   ├── api_routing.R          # Stage 2: GraphHopper route fetch (Hira)
│   ├── api_cdot.R             # Stage 2: CDOT /roadConditions fetch (Hira)
│   ├── route_conditions.R     # Stage 2: spatial filter + per-row risk + LLM prompt (Hira)
│   ├── llm_route_summary.R    # Stage 2: Groq LLM call + response parser (Hira)
│   ├── stage2_rerank.R        # Stage 2: drive-time hard exclusion + resort reranking (Hira)
│   └── demo_data.R            # Demo mode: static peak-season mock data for presentations
└── tests/
    ├── test_scoring.R         # Stage 1 unit + scenario tests
    └── test_stage2.R          # Stage 2 section-by-section test script
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
| Groq (`llama-3.3-70b-versatile`) | Route conditions LLM summary | Required (console.groq.com) | Free tier, no card |

**Design principle:** No data is pre-downloaded or stored statically (except the resort CSV). Every app session fetches live data at runtime.

---

## User Inputs

| Input | Type | Notes |
|---|---|---|
| Starting city | Dropdown | ~70 cities across CO, WY, UT, NM, and surrounding states — no geocoding needed |
| Ski date | Date (YYYY-MM-DD) | Up to 10 days ahead; used as scoring anchor |
| Skier ability | One of: beginner, intermediate, advanced, expert | Single input per session |
| Max drive time | Hours (1–12, step 0.5) | Hard exclusion: resorts exceeding this limit are removed from ranking entirely |

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
- **Fields:** ski date (up to 10 days ahead), max drive time (hours), ability level
- **How it works:** All inputs are collected as reactive values and passed to `score_all_resorts()` when the user clicks Find Resorts

---

## 2. Data Ingestion

### 2a. Static Resort Database
Read at script load time using `read.csv()` in `composite_score.R`:

```r
resorts_raw <- read.csv("data/resorts.csv")
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

### 3d. Season Open/Closed Filter (`R/composite_score.R`)

Before any API calls are made, resorts are filtered against their `season_open` and `season_close` dates from `resorts.csv`. This prevents API calls for closed resorts and ensures closed resorts never appear in recommendations.

**Logic:**
- Dates are stored as `MM-DD` in the CSV (e.g. `"11-27"`, `"04-19"`)
- Ski seasons span two calendar years (open in fall, close in spring)
- The function determines the correct calendar year for each bound based on which half of the season the `ski_date` falls in

**Example:** A resort with `season_open = "11-15"` and `season_close = "04-10"`:
- For a `ski_date` of 2026-03-31: opens 2025-11-15, closes 2026-04-10 → **open**
- For a `ski_date` of 2026-05-01: opens 2025-11-15, closes 2026-04-10 → **closed, excluded**

Resorts with missing season dates are assumed open (fail-safe).

---

### 3e. Open-Meteo API Layer (`R/api_openmeteo.R`)

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

### 3e. Composite Score (`R/composite_score.R`)

Master scoring function integrating all Stage 1 components. Uses a **two-pass structure** because terrain score normalization requires the full distribution across all resorts before any composite can be computed.

**Pass 1** — fetch weather and compute raw terrain scores for all open resorts.

**Normalize** — `normalize_terrain_scores()` scales raw trail counts to 0–1 relative to the best-scoring resort in the candidate set.

**Pass 2** — compute composite scores and apply avalanche multiplier.

**Formula:**
```
base_score = (weather_score × 0.5) + (terrain_score_normalized × 0.5)
```

**Avalanche multiplier** (applied only when ski_date is today or tomorrow):

| Danger level | Multiplier |
|---|---|
| Low | × 1.00 |
| Moderate | × 0.90 |
| Considerable | × 0.75 |
| High | × 0.50 |
| Extreme | score = NA — resort excluded |

If ski_date is beyond tomorrow or CAIC data is unavailable, the multiplier is skipped entirely (`avalanche_applied = FALSE`).

**Output columns:** `resort_id`, `resort_name`, `composite_score`, `weather_score`, `terrain_score`, `avalanche_danger`, `avalanche_applied`, `lift_pct`, `warnings`, `error`

Results are sorted by `composite_score` descending (NAs last). The app displays the top 5.

---


### Stage 2 — Route Risk Score

For each of the top 5 candidate resorts, a driving route is fetched from GraphHopper. Resorts that exceed the user's max drive time are **hard-excluded**. The remaining resorts are ranked by their Stage 1 conditions score. CDOT road condition data is then fetched for the winner's route and summarised by the Groq LLM.

**Pipeline:**
```
User city (dropdown) → lat/lon lookup from cities.csv
lat/lon + resort coords → GraphHopper → route geometry + drive time (top 5 resorts)
Hard exclude: drop any resort with drive time > user max
Rank remaining by conditions score → winner selected
Winner route bbox → CDOT /roadConditions → filtered condition rows
Filtered rows → Groq prompt → risk level + summary + key action
```

**Modules:**

| File | Purpose |
|---|---|
| `R/api_geocode.R` | Nominatim geocoder (retained for ad-hoc use; city coordinates now come from `cities.csv`) |
| `R/api_routing.R` | Fetches driving route from GraphHopper (waypoints, distance, duration, bbox) |
| `R/api_cdot.R` | Fetches CDOT `/roadConditions` — one row per condition entry per segment |
| `R/route_conditions.R` | Spatial corridor filter + per-row risk classification + Groq prompt builder |
| `R/llm_route_summary.R` | Calls Groq (llama-3.3-70b-versatile), parses route summary + score interpretation |
| `R/stage2_rerank.R` | Drive-time hard exclusion + reranking of top 5 resorts |

**CDOT data structure (confirmed from live API):**
- Single working endpoint: `/roadConditions` — `/closures` and `/restrictions` return 404
- Each segment has `primaryLatitude/Longitude` + `secondaryLatitude/Longitude` for spatial filtering
- Conditions nested in `currentConditions[]` — one entry per condition type (forecast, surface, chain law, etc.)
- `additionalData` field contains free text passed to the LLM

**Per-row risk classification (`classify_cdot_risks()` in `route_conditions.R`):**

Each filtered condition row is tagged with a `row_risk` level via keyword matching:

| row_risk | Keywords |
|---|---|
| `do_not_travel` | "closed", "closure", "no travel" |
| `high` | "chain law", "traction law", "code 15/16" |
| `moderate` | "ice", "icy", "packed snow", "snow covered", "blowing snow" |
| `low` | no matching keywords |

This powers the route segment coloring on the Leaflet map.

**Groq LLM output format:**
```
RISK_LEVEL: low | moderate | high | do_not_travel
SUMMARY: 2-3 sentence plain-language description
KEY_ACTION: single most important driver action, or "None"
```

**Note on LLM provider:** Groq was chosen over Google Gemini after Gemini's free tier quota was exhausted. Groq's `llama-3.3-70b-versatile` is free, requires no credit card, and uses the OpenAI-compatible chat completions format. Groq is used for two distinct calls: (1) route conditions summary, and (2) plain-language score interpretation (weather / terrain / avalanche bullets shown in the focus card).

**Drive time integration (hard exclusion):**
- GraphHopper routes are fetched for the top 5 Stage 1 resorts
- Any resort with `duration_mins > user_max_mins` is **hard-excluded** from consideration
- Remaining resorts are ranked by their Stage 1 conditions score — no penalty multipliers
- If **all** resorts exceed the user's limit, the app shows a "no resorts within your drive time" message with the closest available distance, prompting the user to increase their limit
- The focus card map shows the winner's route colored by CDOT risk level; clicking any row in the comparison table fetches and displays that resort's route on demand

### Stage 3 — Feasibility Rules

Three hard exclusion rules are applied before a winner is selected:

1. **Resort closed for the season** — filtered out before any API calls using `season_open`/`season_close` dates in `resorts.csv`
2. **Drive time exceeds user maximum** — hard-excluded after GraphHopper routing; if all resorts exceed the limit, the app shows a "no resorts within your drive time" message
3. **Avalanche Extreme (level 5)** — composite score is set to `NA`, pushing the resort to the bottom of rankings and out of consideration

No soft flags or cascading fallback logic — the remaining resorts within limit are simply ranked by conditions score.

---

## 4. Output

### 4a. Pipeline Output (`R/composite_score.R`)

`score_all_resorts()` returns a plain dataframe sorted by `composite_score` descending (NAs last), with one row per open resort:

| Column | Type | Description |
|---|---|---|
| `resort_id` | int | matches resort CSV |
| `resort_name` | chr | |
| `composite_score` | dbl | 0–1; NA if avalanche danger is Extreme |
| `weather_score` | dbl | 0–1 |
| `terrain_score` | dbl | 0–1, normalized across all resorts |
| `avalanche_danger` | chr | e.g. "Moderate"; NA if not applicable |
| `avalanche_applied` | lgl | TRUE if multiplier was applied |
| `lift_pct` | dbl | estimated % of lifts operating |
| `warnings` | chr | semicolon-separated warning strings; NA if none |
| `error` | chr | weather fetch error message; NA if successful |

### 4b. Shiny App Output (`app.R`)

All outputs update only when the user clicks **Find Resorts** — not on every input change — to avoid unnecessary API calls. Loading notifications appear at each pipeline stage.

**Design:** navy/blue gradient header with mountain SVG logo, white result cards, yellow warnings panel. No external CSS frameworks beyond Bootstrap (bundled with Shiny).

**User inputs (sidebar):** starting city (dropdown), trip date, ability level, max drive time (1–12 hrs).

| Panel | Contents | Shown when |
|---|---|---|
| **Top Recommendation** | Resort name, score badge, Leaflet route map, ski-day weather sparkline, Groq score interpretation bullets (weather / terrain / avalanche / road conditions) | After run; replaced by "no resorts within limit" message if all exceed drive time |
| **Leaflet Route Map** | Start pin, route polyline colored by CDOT risk level, resort pin; clicking table rows re-draws the map for that resort | Inside focus card |
| **All Resorts Table** | Top 5 resorts: score, weather, terrain open %, lifts %, avalanche danger; click any row to explore | Always after run |
| **Resort Snapshot** | Base depth, new snow (72h), terrain open %, avg ski-hour temp for selected resort | Always after run |
| **Avalanche Conditions** | CAIC danger ratings sorted by severity | Today/tomorrow ski dates only |

**Route segment coloring** — the `color_route_segments()` helper checks each GraphHopper waypoint for proximity to CDOT condition points (within ~3.5 miles). Consecutive waypoints with the same risk level are grouped into a single polyline segment and drawn with the appropriate color. High-risk segments are drawn at weight 6 vs 4 for clear segments.

**Graceful degradation:** each API stage is wrapped in `tryCatch`. If geocoding fails, Stage 1 results still display. If routing fails, the map shows resort pins without a route. If Groq is unavailable or the key is missing, the rest of the app still functions.

### 4c. `app_demo.R` — UI Preview Without Live Scoring *(development only)*

A standalone prototype used during development for testing the UI and map without a working pipeline. Superseded by the built-in **Demo Mode** toggle in `app.R` (see Demo Mode section above). Kept in the repository for reference but no longer used.

---

## 5. Replication Instructions

### Dependencies

```r
install.packages(c("shiny", "bslib", "leaflet", "httr2", "dplyr", "purrr",
                   "readr", "lubridate", "sf", "here",
                   "DT", "echarts4r", "tippy"))
```

The `sf` package requires system-level GDAL/GEOS libraries. On Windows these are bundled with the CRAN binary — a standard `install.packages("sf")` is sufficient.

### Running the Shiny App

**Recommended:** Open `app.R` in RStudio and click the **Run App** button in the top-right of the editor pane.

Alternatively, open `launch_app.R` and click **Source** — this sets the working directory explicitly before launching (useful on Windows).

### Running the Scoring Pipeline Directly (Without UI)

```r
setwd("path/to/final project")
source("R/composite_score.R")

resorts <- read.csv("data/resorts.csv")

# Score all open resorts for a given date and ability
results <- score_all_resorts(resorts, ski_date = Sys.Date() + 1, ability = "intermediate")
print(results[, c("resort_name", "composite_score", "weather_score",
                  "terrain_score", "avalanche_danger")])
```

### API Keys

Create a `.Renviron` file in the project root and add your keys. This file is gitignored and must never be committed.

```
CDOT_API_KEY=your_cdot_key         # register at cotrip.org
GRAPHHOPPER_API_KEY=your_key       # console.graphhopper.com — free, 500 req/day
GROQ_API_KEY=your_key              # console.groq.com — free, no credit card required
```

Restart R after editing `.Renviron` so the keys are loaded into the session. Stage 1 (resort scoring) requires no API keys — CAIC and Open-Meteo are free and open.

### Cache Behaviour

CAIC data is cached in memory for the R session. To force a fresh API call:

```r
source("R/fetch_caic.R")
clear_caic_cache()
results <- score_all_resorts(resorts, ski_date = Sys.Date() + 1, ability = "intermediate")
```

---

## Pending Items

No pending items — the project is complete and deployed at **https://marenroe.shinyapps.io/ski_smart/**

---

## Development Log

### Session 1 — Project Initialization
- Defined product scope and two-layer architecture
- Curated static resort CSV (`resorts - clean.csv`) with 33 Colorado resorts
- Established data source inventory and API access plan
- Initialized repository structure

### Session 4 — Shiny UI + Route Map + Risk Coloring (Hira)
- Built full `app.R` UI with Stage 2 pipeline wiring:
  - Added `textInput("start_location")` to sidebar
  - Added `leafletOutput("route_map")` — Leaflet map with CartoDB Positron tiles
  - Added route risk panel: colored risk badge, Groq LLM summary, key action advisory
  - Updated resort table to show drive time, drive penalty %, and final score
  - Wired server: geocode → rerank (×3 routes) → CDOT → Groq; all stages wrapped in `tryCatch` for graceful degradation
- Added `classify_cdot_risks()` to `R/route_conditions.R` — tags each CDOT condition row with a `row_risk` level via keyword matching; `prepare_route_conditions()` now returns `filtered_df` with this column
- Added `color_route_segments()` helper (in both `app.R` and `app_demo.R`) — splits route waypoints into same-risk groups and returns colored polyline segments; high-risk segments drawn thicker
- Route coloring: gray = clear, orange = moderate, red/thicker = high, dark red = do_not_travel
- Built `app_demo.R` — full UI preview using dummy resort data, real geocoding + routing, dummy CDOT conditions at Floyd Hill / Eisenhower Tunnel / Silverthorne for testing the map without a working Stage 1 pipeline
- Switched LLM provider: Gemini → Groq (`llama-3.3-70b-versatile`) — free tier with no credit card; confirmed Gemini free quota = 0 at account level across multiple keys

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

### Session 4 — Stage 1 Integration + App Redesign (Maren)
- Built `R/composite_score.R` — master Stage 1 scoring function integrating all components:
  - Sources `R/api_openmeteo.R`, `R/weather_score.R`, `R/terrain_open_score.R`, `R/fetch_caic.R`
  - Two-pass structure: Pass 1 fetches weather + raw terrain scores; Pass 2 normalizes terrain and computes composite
  - Season open/closed filter applied before API calls — closed resorts excluded from results
  - Avalanche multiplier applied for today/tomorrow only; skipped for future dates
  - Handles weather fetch failures gracefully (NA scores, error logged, pipeline continues)
- Redesigned `app.R` — minimalist dashboard:
  - Navy/blue gradient header with inline SVG mountain logo
  - White result cards with subtle shadows
  - Yellow-tinted warnings panel (conditional — only shown when warnings exist)
  - Avalanche conditions card (conditional — only shown for today/tomorrow)
  - Top 5 resorts displayed (unsafe and errored resorts excluded)
  - All outputs update only on button click to avoid unnecessary API calls
- Updated `.gitignore` to exclude `.env` (API keys)

**Next:** Wire Stage 2 route risk scoring into the app, add origin city input.

### Session 5 — Stage 2 Integration + UI Redesign (Hira + Maren)
- Merged Maren's composite scoring + UI redesign with Hira's Stage 2 pipeline
- Added `R/llm_route_summary.R` — Groq LLM call for route summary and per-resort score interpretation
- Added `data/cities.csv` — ~70 starting cities (CO, WY, UT, NM, surrounding states) with lat/lon; replaces free-text geocoding input
- Refactored `R/stage2_rerank.R` — drive-time hard exclusion + resort reranking
- Replaced `textInput` city entry with `selectizeInput` dropdown (no geocoding needed)
- Switched LLM provider from Gemini to Groq (`llama-3.3-70b-versatile`)
- Added `bslib`, `DT`, `echarts4r`, `tippy` as new UI dependencies
- Full `app.R` rewrite: Leaflet route map, weather sparkline (echarts4r), DT comparison table, score interpretation bullets

### Session 6 — UI Polish (Maren)
- **Sidebar reorder:** Drive time slider moved under Trip Details; Skier Ability section now follows below the `hr()` separator
- **Slogan:** Removed trailing period from header tagline
- **Grammar fix:** "advanceds" → "advanced skiers" (and all ability levels) throughout table and snapshot card
- **Avalanche note:** Changed "For future dates, check..." → "For further information, check the CAIC website directly."
- **Tooltip improvements** (using `tippy`):
  - Score: full composite formula explanation added
  - Weather: clarified includes snow depth, 72h snowfall, temperature, and wind speed
  - Terrain open: explains trails are matched to the user's selected ability level
  - Lifts: clarifies wind-speed derivation and why lifts close
  - Avalanche: mentions Low→Extreme scale and feed-in to composite score
  - Drive Time: new column added with tooltip
- **Drive Time column:** Added to comparison table with routing-based estimates
- **Drive time format:** Changed from raw minutes to "X hr Y min · N mi" format; text made slightly larger and bolder
- **Map pins:** Destination resort changed from circle to red `awesomeMarker` (teardrop pin); origin changed to white circle with navy border (hollow dot style)
- **Avalanche table:** Filtered to show only the top 5 ranked resorts (was showing all resorts)
- **Resort Snapshot icons:** Added minimalist inline SVG icons — blue snowflake (base depth), light-blue cloud (new snow), green mountain (open terrain), orange thermometer (avg temp)
- **New dependency:** `DT`, `echarts4r`, `tippy` (install with `install.packages(c("DT", "echarts4r", "tippy"))`)

### Session 6 — Bug Fix
- Fixed `[object Object]` rendering error in the comparison table: `format_drive_time()` is a scalar function and failed silently when passed a full vector inside `dplyr::case_when()`; replaced with `sapply(duration_mins, format_drive_time)` so it is applied element-wise

### Session 7 — Road Conditions for All Resorts + UI Fixes (Hira)
- **Road conditions for all 5 resorts:** CDOT + Groq route analysis now pre-computed for all top 5 resorts in the pipeline, not just the winner; clicking any row in the comparison table shows that resort's road conditions and route map
- **Map auto-zoom:** Leaflet map now calls `fitBounds` on the route bounding box so the full route is always visible; fixed a bug where named numeric vectors were serialized as JSON objects instead of scalars — wrapped all bbox values with `as.numeric()`
- **Drive-time exclusion fix:** Comparison table was still displaying resorts that exceeded the user's max drive time (e.g. Wolf Creek); fixed by filtering the table to `reranked$all_resorts` and ensuring `all_resorts` is populated in both return branches of `rerank_with_drive_time()`
- **Terrain column:** Changed from misleading "~X% open for [ability] skiers" label to a plain numeric score (e.g. `0.74`) consistent with the weather score display, since the value is a normalized relative score, not a literal percentage
- **Data-specific LLM interpretations:** Rewrote `call_groq_score_interpretation()` to pass actual observed values — base depth, 72h snowfall, avg temp, wind speed, estimated % of ability-preferred terrain open, and terrain rank context — so score bullets name specific factors instead of giving generic text
- **Grey map / broken resort clicking fix:** Groq API call was inside `renderUI`, blocking Shiny's single R thread and preventing `renderLeaflet` from executing; moved to a `priority = -1` observer backed by a `reactiveValues` cache so the map and UI render immediately and score bullets populate once Groq returns

### Session 8 — Terrain Score Zero Gate (Hira)
- **Bug fix — terrain score inflation when conditions are poor:** `normalize_terrain_scores()` used min-max normalization, causing the best resort to always receive a score of 1.00 even when all resorts had near-zero terrain accessibility (e.g. late-season, thin base). This produced absurd LLM output such as "0% of preferred trails accessible, terrain score: 1.00"
- **Fix:** Added `pct_preferred_open` (ability-weighted average fraction of preferred trail tiers estimated open) to the return value of `compute_terrain_open_score()` in `R/terrain_open_score.R`; stored it in Pass 1 of `composite_score.R`; in Pass 2, terrain score is forced to 0 if `pct_preferred_open == 0`, regardless of relative ranking. Otherwise the existing min-max normalization is preserved so absolute trail count comparisons across resorts remain valid

### Session 9 — Deployment + Demo Mode + UI Polish (Maren)

**Deployment**
- App deployed to shinyapps.io at https://marenroe.shinyapps.io/ski_smart/
- API keys passed via `.Renviron` at deploy time (free tier workaround — UI environment variable settings are not available on the free plan)
- Redeploy command: `rsconnect::deployApp(appDir = getwd(), appName = "ski_smart")`

**Demo Mode**
- Added `R/demo_data.R` with static peak-season (January) mock data for five Denver-area resorts
- Demo Mode checkbox in sidebar bypasses all live API calls instantly — no Find Resorts click needed
- Includes pre-written LLM score interpretations, weather sparkline data, realistic drive times, route polylines with simulated CDOT road conditions (Vail Pass and Rabbit Ears Pass show as moderate/orange), and avalanche ratings
- Score interp observer skips Groq API in demo mode and populates cache from pre-written text

**UI improvements**
- **Equal-height bottom cards:** Avalanche Forecast and Resort Snapshot cards now always align at the bottom using flexbox on the bottom row
- **Website links:** Comparison table has a new Website column with ↗ links to each resort's official website (from `resort_url` in `resorts.csv`), centered and width-constrained
- **Map markers:** Start pin changed to a solid navy filled dot; resort pin is a red awesome marker with a white dot (CSS `color: white !important` override applied to `.awesome-marker i`)
- **Route color fix:** `risk_color()` default (clear roads) changed from grey `#888888` to blue `#2E6DA4` to match the legend; legend now also shown when CDOT data is unavailable
- **DT selection decoupled:** Removed reactive `selected = selected_idx()` from `renderDataTable` options; a separate `DT::dataTableProxy` observer now updates the highlighted row without causing a full table re-render (fixes weather chart not updating on resort switch in demo mode)

---

*This document is updated continuously as the project develops.*
