# fx-dashboard

Personal macro dashboard. Python ingests public data into DuckDB; a static site
reads precomputed tables and renders them. Nothing is computed at page load.

## Layout

```
src/macropinion/
  config.py       paths and secrets, resolved from the repo root
  registry.py     >>> the file you edit to add data <<<
  store.py        DuckDB schema, upserts, read helpers
  transforms.py   named transforms for derived series
  ingest.py       orchestration: fetch raw, compute derived, write
  sources/
    fred.py       FRED client
scripts/
  run_ingest.py   entry point
data/
  warehouse.duckdb   gitignored, rebuilt by every run
```

## Setup

```bash
python -m venv .venv && source .venv/bin/activate
pip install -e .

cp .env.example .env      # then paste your FRED key into .env
python scripts/run_ingest.py
```

A free FRED key takes about a minute:
https://fredaccount.stlouisfed.org/apikeys

The run prints a freshness table — one row per series with its last observation
date and count. That table is your first line of defence against silently stale
data, so read it rather than skipping past it.

## Adding a series

Append one entry to `RAW_SERIES` in `registry.py`:

```python
RawSeries(
    key="dgs10",
    source="fred",
    source_id="DGS10",
    label="10-year Treasury yield",
    unit="percent",
)
```

Derived series are the same idea, referencing an existing key and a transform
name from `transforms.py`:

```python
DerivedSeries(
    key="dgs10_1m_chg", from_key="dgs10", transform="diff",
    label="10y yield, daily change", unit="bp",
)
```

Re-run ingestion. The schema does not change, because observations are stored in
long format.

## Adding a source

Write a module in `sources/` exposing `fetch(source_id, start) -> DataFrame` with
columns `obs_date` and `value`, then register it in `ingest.FETCHERS`. Ingestion
itself stays source-agnostic.

## Design rules

- **Raw history is replaced, not appended.** Statistical agencies revise past
  prints; appending would preserve superseded numbers forever.
- **Derived series are stored, not computed on read.** The site only ever selects
  from a table.
- **A failing source degrades to a warning.** The previous history stays in the
  store and the build still succeeds, so one outage does not blank the dashboard.

## The site

```bash
cd site
npm install
npm run dev        # http://localhost:3000
npm run build      # -> site/dist
```

Ingestion must have run first — the site reads `data/warehouse.duckdb` and will
refuse to build against an empty store rather than render a blank chart.

### Adding a page

Three files, none of which touch anything that already exists:

1. **A data loader** — copy `site/src/data/inflation.parquet.py`, change `SERIES`.
   Observable runs it at build time and captures stdout as a Parquet file.
2. **A page** — a `.md` file under `site/src/`. Its path is its URL, so
   `site/src/rates/us-curve.md` is served at `/rates/us-curve`.
3. **A sidebar entry** — one line in the `pages` array in `observablehq.config.js`.

Subpages need no new mechanism: nest a directory, nest a `pages` array.

### Adding a chart to an existing page

Call `timeSeries()` again with a different `series` array. The component takes
per-series colour, dash pattern and curve, so charts stay consistent without
copying Plot configuration between pages.

## Deployment

Cloudflare Pages, driven by `.github/workflows/build.yml`. Three repo secrets:

| Secret | Where it comes from |
| --- | --- |
| `FRED_API_KEY` | fredaccount.stlouisfed.org/apikeys |
| `CLOUDFLARE_API_TOKEN` | Cloudflare dashboard, "Cloudflare Pages: Edit" template |
| `CLOUDFLARE_ACCOUNT_ID` | Cloudflare dashboard sidebar |

**Keeping it private:** the deployment is public by default. In the Cloudflare
dashboard, add a Cloudflare Access policy on the Pages project allowing only your
email, using one-time PIN. That is the whole access-control story — no login code
in this repo.
