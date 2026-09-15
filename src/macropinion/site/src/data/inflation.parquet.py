"""Data loader: US inflation.

Observable Framework executes this at build time and captures stdout as
`data/inflation.parquet`. Nothing here computes anything — it selects series
that ingestion already wrote and hands them over.

Copy this file to add a chart's data. The only lines that change are SERIES
and YEARS.
"""

from __future__ import annotations

import sys
from datetime import date
from pathlib import Path

import pyarrow as pa
import pyarrow.parquet as pq

# site/src/data/<this file> -> repo root is four levels up.
REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "src"))

from macropinion import store  # noqa: E402

# The chart plots the monthly series; the readings strip and the spread use the
# daily one, so the headline number is today's level rather than a month average.
SERIES = ["cpi_yoy", "breakeven_5y", "breakeven_5y_m"]

# Ship more history than the default view shows, so the range buttons on the
# page are free and do not need a rebuild.
YEARS = 10


def main() -> None:
    start = date.today().replace(year=date.today().year - YEARS).isoformat()

    with store.connect(read_only=True) as con:
        frame = store.read_series(con, SERIES, start=start)

    if frame.empty:
        raise SystemExit(
            "No observations found. Run `python scripts/run_ingest.py` before building the site."
        )

    # Pin the Arrow types rather than letting pandas infer them. By default this
    # yields timestamp[us] and large_string, both of which the JavaScript Arrow
    # reader handles inconsistently — microsecond timestamps in particular come
    # back as raw integers rather than dates. Millisecond timestamps and plain
    # strings are read the same way everywhere.
    schema = pa.schema(
        [
            ("series_key", pa.string()),
            ("obs_date", pa.timestamp("ms")),
            ("value", pa.float64()),
            ("label", pa.string()),
            ("unit", pa.string()),
        ]
    )
    table = pa.Table.from_pandas(frame, preserve_index=False).cast(schema)

    buffer = pa.BufferOutputStream()
    pq.write_table(table, buffer, compression="zstd")
    sys.stdout.buffer.write(buffer.getvalue().to_pybytes())


if __name__ == "__main__":
    main()
