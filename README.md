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
| OpenRouteService | Driving routes, GeoJSON geometry | Required | Free tier |
| Nominatim / OpenStreetMap | Geocode user origin to coordinates | None | Free |
| Anthropic / OpenAI | LLM synthesis layer | Required | ~$1–2 total |

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

Estimates what fraction of a skier's preferred terrain is accessible — distinct from weather score which measures condition quality.

```
trail_score         = sum(tier_open_pct * tier_weight)   [greens, blues, blacks, dbl blacks]
accessibility_score = trail_score * lift_pct
```

**Functions:**

| Function | Purpose |
|---|---|
| `get_terrain_pct_by_depth(depth_in)` | Snow depth → base % open per trail tier, piecewise linear |
| `apply_snowfall_bump(terrain_pcts, snowfall_72hr_in)` | Fresh snow bonus per tier, respects tier caps |
| `get_lift_pct(wind_mph)` | Wind speed → estimated % of lifts operating, step function |
| `get_terrain_weights(ability)` | Returns ability-specific tier weight vector |
| `compute_terrain_open_score(...)` | Orchestrates into final accessibility score |

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

1. **Tier caps on snowfall bump.** Blacks cap at 90%, double blacks at 70% — operational limits (patrol coverage, grooming capacity) that fresh snow alone cannot overcome.

2. **Lift pct as a multiplier.** Wind-driven closures scale down the entire accessibility score. 40% of lifts running means even fully open terrain is meaningfully less reachable.

3. **Two-step output.** Both `trail_score` (before wind adjustment) and `accessibility_score` (final) are returned so the UI can explain: *"86% of your terrain is open, but wind is cutting lift access to 70% — effective score: 0.60."*

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

### Route Risk Score (Stage 2)

For each of the top 3 candidate resorts, 20 points are sampled along the driving route. Each point is scored:

```
risk = w1*(snow forecast)
     + w2*(closure status)
     + w3*(avalanche danger)
     + w4*(elevation)
     + w5*(vehicle penalty)
```

Weights are scaled by user-specified risk tolerance.

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
