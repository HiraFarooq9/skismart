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

## Tech Stack

*[To be finalized — candidates are R/Shiny or Python/Streamlit. Decision and rationale will be recorded here once made.]*

---

## Scoring Logic

### Resort Score (Stage 1)

Each resort is scored on:
- **Snow score** — function of base depth, recent snowfall (72hr), and trip-date forecast
- **Terrain match** — alignment between trail difficulty distribution and group skill levels
- **Conditions risk** — avalanche danger rating + storm forecast
- **Estimated terrain open** — rule-based formula using base depth and recent snowfall

Top 3 resorts by weighted score advance to route scoring.

### Route Risk Score (Stage 2)

For each candidate resort, 20 points are sampled along the driving route. Each point is scored:

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

## User Inputs

Origin city/location, travel dates, group size, skier ability per person, terrain preferences, resort vibe, vehicle type, chains availability, max drive time, risk tolerance, season pass (None/Ikon/Epic), arrival/departure time window.

---

## Output Dashboard

- **Map** — route colored green/yellow/red by risk segment; avalanche zone overlays; resort pins with score badges; rejected resorts marked with reason
- **Recommended resort panel** — score breakdown, estimated terrain open (with caveat)
- **Departure timing chart** — hourly risk curve, optimal departure/return window
- **Flagged route segments**
- **LLM-generated trip briefing** — plain-language recommendation, safety advisories, gear/packing list calibrated to forecast temperatures

---

## Scope

- **Geography:** Colorado resorts (initial). Expansion to additional states if time permits.
- **Forecast horizon:** Up to 10 days out (Open-Meteo reliable range; historical climatology used as fallback beyond 14 days).

---

## Development Log

### Session 1 — Project Initialization
- Defined product scope and two-layer architecture
- Curated static resort CSV (`data/resorts.csv`) with ~30 Colorado resorts
- Established data source inventory and API access plan
- Initialized repository structure (`data/`, `R/`)

### Session 2 — Weather Score Module (`R/weather_score.R`)

**Division of labor (Stage 1):**
- This session: weather score + terrain open score
- Teammate: conditions risk score + terrain match to user ability

**Implemented functions:**

| Function | Purpose |
|---|---|
| `score_snow_depth(depth_in, snowfall_72hr_in)` | Base depth → 0–1 with thin-base rescue clause |
| `score_snowfall(snowfall_in)` | 72hr accumulation → 0–1 |
| `get_precip_multiplier(temp_f, precip_mm_hr, snowfall_mm_hr)` | Rain/mixed/snow classification → multiplier |
| `score_temperature(temp_f)` | Temperature → 0–1 |
| `score_wind(wind_mph)` | Wind speed → 0–1 |
| `get_weather_weights(ability)` | Returns ability-specific weight vector |
| `compute_weather_score(...)` | Orchestrates all components into final score |

**Key design decisions:**

1. **Precipitation as a multiplier, not an additive component.** Rain can destroy an otherwise good score regardless of depth or snowfall — a 0.2 multiplier for heavy rain is intentional and reflects real conditions.

2. **72-hour snowfall window.** Used consistently across both the fresh snowfall score and the thin-base rescue clause (>= 8" in 72hrs rescues a sub-24" base to 0.2).

3. **Piecewise linear interpolation.** Rather than hard step functions between breakpoints, scores interpolate continuously within each band. This avoids discontinuous jumps (e.g., a resort at 35" scoring identically to one at 30").

4. **Precipitation type derived from temperature + snowfall rate.** Rather than relying on a separate "precip type" API field, we classify using: temp < 30°F → snow; temp 30–34°F → mixed (unless snowfall_mm_hr > 5, then snow override); temp > 34°F → rain.

5. **Ability-weighted composite.** Experts weight fresh snowfall (0.40) much higher than base depth (0.30). Beginners reverse this (depth 0.45, snowfall 0.15) because they rely on groomed runs which need adequate base.

**Coordinate approach:**
Resort weather conditions are queried at the resort's primary coordinate (base lodge lat/lon from `resorts.csv`). Future iterations could improve accuracy by querying at mid-mountain elevation.

**Snow depth — no hard exclude:**
The original design hard-excluded resorts with < 24" base depth (score = 0.0). This was removed after live API testing showed Vail returning 0" in late March — accurate at base elevation, but the resort was still operating. Two concerns were conflated: (1) *is the resort open?* and (2) *how good are conditions?* These are now separated: resort operating status is checked via `season_open`/`season_close` dates in the CSV (feasibility check, Stage 3); snow depth purely scores condition quality on a continuous scale from 0" (0.0) to 100"+ (1.0). The rescue clause was also removed — fresh snowfall is already captured independently by `score_snowfall()`.

**Terrain Open Score Module (`R/terrain_open_score.R`):**

| Function | Purpose |
|---|---|
| `get_terrain_pct_by_depth(depth_in)` | Snow depth → base % open per trail tier |
| `apply_snowfall_bump(terrain_pcts, snowfall_72hr_in)` | Adds fresh snow bonus, respects tier caps |
| `get_lift_pct(wind_mph)` | Wind speed → estimated % of lifts operating |
| `get_terrain_weights(ability)` | Returns ability-specific tier weight vector |
| `compute_terrain_open_score(...)` | Orchestrates into final accessibility score |

**Key design decisions:**

1. **Tier caps on snowfall bump.** Blacks cap at 90% and double blacks at 70% regardless of snow — these reflect operational limits (patrol, grooming capacity) that fresh snow alone can't overcome.

2. **Piecewise linear interpolation for depth.** Same approach as weather score — smooth transitions between breakpoints rather than hard steps.

3. **Lift pct as a multiplier.** Wind-driven lift closures scale the entire trail score down. If only 40% of lifts are running, even fully open terrain is meaningfully less accessible.

4. **Two-step output.** `trail_score` (terrain availability before lift impact) and `accessibility_score` (final, after lift pct) are both returned so the UI can show users *why* the score is what it is — e.g., "great snow but wind is limiting access."

**Open-Meteo API Layer (`R/api_openmeteo.R`):**

| Function | Purpose |
|---|---|
| `fetch_weather_openmeteo(lat, lon, trip_date)` | Fetches and extracts all scoring inputs for one resort |
| `fetch_weather_all_resorts(resorts_df, trip_date)` | Iterates over all resorts, returns named list |

**Key design decisions:**

1. **`past_days = 3` + `forecast_days = 10`.** The 3-day lookback retrieves the 72hr snowfall window. 10-day forward matches the product's stated forecast horizon.

2. **Aggregation over ski hours (8am–4pm).** Temperature is averaged; wind and precipitation use the **max** over the ski window (worst-case, conservative). Snowfall rate is averaged (used only for precip type classification, not magnitude).

3. **Snow depth at midday.** A single representative snapshot at 12:00 on the trip date. Falls back to the first available hour if midday data is missing.

4. **72hr snowfall window ends at midnight of trip date.** Accumulation is the sum of hourly snowfall from 72hrs before trip_date 00:00. This captures the recent base-building period, not same-day snowfall (which is captured separately via the ski-hours snowfall rate).

5. **Error isolation per resort.** `fetch_weather_all_resorts()` catches failures per resort and returns `list(error = ...)` rather than crashing the whole pipeline if one API call fails.

**Date handling — first ski day as scoring anchor:**
When a user enters a trip date range (arrival → departure), scoring is anchored to the **first ski day** — the first day they will actually be on the mountain. Rationale: the user is choosing *where to go*, and conditions on day 1 drive that decision. Snow depth and the 72hr snowfall lookback are also naturally anchored to arrival. The `trip_date` parameter was renamed to `ski_date` throughout to make this explicit. The caller derives `ski_date` from user inputs (e.g., arrival date if arriving in the morning; arrival + 1 day if arriving in the evening).

**Next steps:**
- Integration of weather + terrain open into the Stage 1 resort ranker
- Wire `fetch_weather_openmeteo()` output directly into `compute_weather_score()` and `compute_terrain_open_score()`

---

*This document is updated continuously as the project develops.*
