# US inflation

<div class="standfirst">
Realized inflation against what the market is pricing. Breakeven is the 5-year
nominal yield less the 5-year TIPS yield — the compensation investors demand for
inflation over the next five years. When breakevens fall while CPI is still
running hot, the market is telling you the disinflation is already underway.
</div>

```js
import { timeSeries, latest, spreadSeries, palette } from "../components/timeseries.js";
import { buttonGroup } from "../components/controls.js";

const table = await FileAttachment("../data/inflation.parquet").parquet();

// Arrow hands back dates as either Date objects or epoch values depending on
// how the column was encoded; normalise once here so nothing downstream cares.
const toDate = (v) => (v instanceof Date ? v : new Date(Number(v)));

const data = Array.from(table, (d) => ({
  series_key: d.series_key,
  obs_date: toDate(d.obs_date),
  value: d.value == null ? null : Number(d.value),
  label: d.label,
}));
```

```js
const SERIES = [
  { key: "cpi_yoy", label: "CPI year-over-year", color: palette.rose, curve: "linear" },
  {
    key: "breakeven_5y_m",
    label: "5-year breakeven",
    color: palette.amber,
    dash: "5 4",
    curve: "linear",
  },
];

const cpi = latest(data, "cpi_yoy");
const breakeven = latest(data, "breakeven_5y");
const spread = spreadSeries(data, "cpi_yoy", "breakeven_5y").at(-1);

const fmt = (v) => (v == null ? "—" : `${v.toFixed(2)}%`);
const fmtSigned = (v) => (v == null ? "—" : `${v > 0 ? "+" : ""}${v.toFixed(2)} pp`);
const fmtDay = (d) => (d ? d.toISOString().slice(0, 10) : "—");

const swatch = (color, dashed) =>
  html`<span class="reading-swatch" style="background:${
    dashed
      ? `repeating-linear-gradient(90deg, ${color} 0 4px, transparent 4px 7px)`
      : color
  }"></span>`;
```

<div class="readings">
  <div>
    <div class="reading-label">${swatch(palette.rose, false)}Realized CPI, y/y</div>
    <div class="reading-value" style="color:${palette.rose}">${fmt(cpi?.value)}</div>
    <div class="reading-date">${fmtDay(cpi?.obs_date)}</div>
  </div>
  <div>
    <div class="reading-label">${swatch(palette.amber, true)}5-year breakeven</div>
    <div class="reading-value" style="color:${palette.amber}">${fmt(breakeven?.value)}</div>
    <div class="reading-date">${fmtDay(breakeven?.obs_date)}</div>
  </div>
  <div>
    <div class="reading-label">Realized less implied</div>
    <div class="reading-value">${fmtSigned(spread?.value)}</div>
    <div class="reading-date">latest print vs today's pricing</div>
  </div>
</div>

```js
const years = view(
  buttonGroup(
    [
      { label: "1Y", value: 1 },
      { label: "3Y", value: 3 },
      { label: "5Y", value: 5 },
      { label: "10Y", value: 10 },
    ],
    { value: 5, label: "Time range" }
  )
);
```

```js
const cutoff = new Date(Date.now() - years * 365.25 * 864e5);
const windowed = data.filter((d) => d.obs_date >= cutoff);
```

<div class="panel">
  ${resize((w) =>
    timeSeries(windowed, {
      series: SERIES,
      rules: [{ value: 2, label: "Fed target", color: palette.blue }],
      width: w,
      height: Math.max(320, Math.min(460, w * 0.55)),
    })
  )}
</div>

```js
// Macro data is stale by nature. Say how stale, and flag it once CPI is late.
const daysOld = cpi ? Math.floor((Date.now() - cpi.obs_date) / 864e5) : null;
const isStale = daysOld != null && daysOld > 75;
```

<div class="asof ${isStale ? "stale" : ""}">
  Latest CPI print ${daysOld ?? "—"} days old · sources FRED
  <code>CPIAUCSL</code>, <code>T5YIE</code>
</div>
