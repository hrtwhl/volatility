import * as Plot from "npm:@observablehq/plot";
import * as d3 from "npm:d3";

export const palette = {
  ink: "#05161A",
  paper: "#F0F0F0",
  roseDeep: "#DA4167",
  rose: "#FF6978",
  amber: "#FF9B42",
  mint: "#45B69C",
  blueDeep: "#087CA7",
  blue: "#2892D7",
  periwinkle: "#7494EA",
  hairline: "#16323C",
  muted: "#8FA3AA",
};

const fmtPct = d3.format(".2f");
const fmtDate = d3.utcFormat("%b %Y");

/**
 * A percent-scaled time series chart.
 *
 * Written once and reused by every page, so a new chart is a call with a
 * different `series` array rather than a new block of Plot code.
 *
 * @param data   long format rows: {series_key, obs_date, value}
 * @param series [{key, label, color, dash}] — draw order is array order
 * @param rules  [{value, label, color}] — horizontal reference lines
 */
export function timeSeries(
  data,
  { series, rules = [], width = 720, height = 420, yLabel = "percent", yDomain, yTicks = 6 } = {}
) {
  const wanted = new Set(series.map((s) => s.key));
  const rows = data.filter((d) => wanted.has(d.series_key));

  const labelByKey = new Map(series.map((s) => [s.key, s.label]));

  return Plot.plot({
    width,
    height,
    marginLeft: 44,
    marginRight: 16,
    marginTop: 12,
    marginBottom: 32,
    style: {
      background: "transparent",
      color: palette.muted,
      fontFamily: "Archivo, system-ui, sans-serif",
      fontSize: "12px",
      fontVariantNumeric: "tabular-nums",
      overflow: "visible",
    },
    x: {
      type: "utc",
      label: null,
      ticks: 6,
      tickSize: 4,
      tickPadding: 8,
      tickFormat: "%b '%y",
    },
    y: {
      label: null,
      domain: yDomain,
      // No `grid: true` here — gridlines are drawn once, explicitly, as a mark
      // below. Setting both draws two overlapping grids at different opacities.
      nice: true,
      // Without inset, an auto-scaled domain ends exactly at the data maximum
      // and the line gets clipped against the frame.
      insetTop: 12,
      insetBottom: 12,
      ticks: yTicks,
      tickSize: 4,
      tickPadding: 8,
      tickFormat: (d) => `${d}%`,
    },
    marks: [
      // Reference lines sit underneath the data.
      ...rules.map((r) =>
        Plot.ruleY([r.value], {
          stroke: r.color ?? palette.blue,
          strokeWidth: 1.25,
          strokeDasharray: "6 5",
          strokeOpacity: 0.85,
        })
      ),
      ...rules
        .filter((r) => r.label)
        .map((r) =>
          Plot.text([r.label], {
            frameAnchor: "left",
            y: r.value,
            dy: 13,
            dx: 4,
            fill: r.color ?? palette.blue,
            fontSize: 11,
            textAnchor: "start",
          })
        ),

      Plot.gridY({ stroke: palette.hairline, strokeOpacity: 1, strokeDasharray: "2 4" }),
      Plot.gridX({ stroke: palette.hairline, strokeOpacity: 1, strokeDasharray: "2 4" }),

      ...series.map((s) =>
        Plot.lineY(
          rows.filter((d) => d.series_key === s.key),
          {
            x: "obs_date",
            y: "value",
            stroke: s.color,
            strokeWidth: 2.25,
            strokeDasharray: s.dash ?? null,
            curve: s.curve ?? "linear",
            strokeLinecap: "round",
          }
        )
      ),

      // Crosshair + per-series readout on hover.
      Plot.tip(
        rows,
        Plot.pointerX({
          x: "obs_date",
          y: "value",
          fill: palette.ink,
          stroke: palette.hairline,
          fontSize: 12,
          title: (d) =>
            `${fmtDate(d.obs_date)}\n${labelByKey.get(d.series_key)}: ${fmtPct(d.value)}%`,
        })
      ),
      Plot.ruleX(
        rows,
        Plot.pointerX({ x: "obs_date", stroke: palette.muted, strokeOpacity: 0.4 })
      ),
    ],
    ariaLabel: `${series.map((s) => s.label).join(" and ")}, ${yLabel}`,
  });
}

/** Latest non-null observation for a series key. */
export function latest(data, key) {
  const rows = data
    .filter((d) => d.series_key === key && d.value != null)
    .sort((a, b) => a.obs_date - b.obs_date);
  return rows.at(-1) ?? null;
}

/**
 * Carry each series forward onto a common daily grid.
 *
 * Monthly CPI and daily breakevens do not share observation dates, so a naive
 * difference would only be defined on the twelve days a year they happen to
 * coincide. Step-forward matches how the chart already draws the monthly line.
 */
export function spreadSeries(data, keyA, keyB) {
  const pick = (k) =>
    data
      .filter((d) => d.series_key === k && d.value != null)
      .sort((a, b) => a.obs_date - b.obs_date);

  const a = pick(keyA);
  const b = pick(keyB);
  if (!a.length || !b.length) return [];

  let i = 0;
  let carried = null;
  const out = [];

  for (const row of b) {
    while (i < a.length && a[i].obs_date <= row.obs_date) carried = a[i++];
    if (carried) out.push({ obs_date: row.obs_date, value: carried.value - row.value });
  }
  return out;
}
