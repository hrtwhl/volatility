"""The DuckDB store.

Two tables, long format. Adding a series never changes the schema, which is the
property that makes the rest of the project cheap to extend.

    series_meta   one row per series: label, unit, provenance, freshness
    observations  one row per (series, date): the actual numbers

Model outputs will land here too, as their own tables, following the same rule:
whatever the site displays must already exist in this file before the site builds.
"""

from __future__ import annotations

from contextlib import contextmanager
from typing import Iterator

import duckdb
import pandas as pd

from .config import DB_PATH

SCHEMA = """
CREATE TABLE IF NOT EXISTS series_meta (
    series_key  VARCHAR PRIMARY KEY,
    label       VARCHAR NOT NULL,
    unit        VARCHAR NOT NULL,
    kind        VARCHAR NOT NULL,     -- 'raw' or 'derived'
    source      VARCHAR,
    source_id   VARCHAR,
    updated_at  TIMESTAMP NOT NULL
);

CREATE TABLE IF NOT EXISTS observations (
    series_key  VARCHAR NOT NULL,
    obs_date    DATE    NOT NULL,
    value       DOUBLE,
    PRIMARY KEY (series_key, obs_date)
);
"""


@contextmanager
def connect(read_only: bool = False) -> Iterator[duckdb.DuckDBPyConnection]:
    """Open the warehouse. Creates the file and schema on first use."""
    DB_PATH.parent.mkdir(parents=True, exist_ok=True)
    con = duckdb.connect(str(DB_PATH), read_only=read_only)
    try:
        if not read_only:
            con.execute(SCHEMA)
        yield con
    finally:
        con.close()


def upsert_series(
    con: duckdb.DuckDBPyConnection,
    *,
    series_key: str,
    label: str,
    unit: str,
    kind: str,
    source: str | None = None,
    source_id: str | None = None,
) -> None:
    con.execute(
        """
        INSERT INTO series_meta
            (series_key, label, unit, kind, source, source_id, updated_at)
        VALUES (?, ?, ?, ?, ?, ?, now())
        ON CONFLICT (series_key) DO UPDATE SET
            label = excluded.label,
            unit = excluded.unit,
            kind = excluded.kind,
            source = excluded.source,
            source_id = excluded.source_id,
            updated_at = excluded.updated_at
        """,
        [series_key, label, unit, kind, source, source_id],
    )


def replace_observations(
    con: duckdb.DuckDBPyConnection, series_key: str, frame: pd.DataFrame
) -> int:
    """Replace one series' history wholesale.

    Full replacement rather than incremental append, because statistical agencies
    revise published history and an append-only table would quietly keep the
    superseded prints forever.

    `frame` needs columns: obs_date, value.
    """
    if frame.empty:
        return 0

    payload = frame.loc[:, ["obs_date", "value"]].copy()
    payload["obs_date"] = pd.to_datetime(payload["obs_date"]).dt.date
    payload["series_key"] = series_key
    payload = payload.loc[:, ["series_key", "obs_date", "value"]]

    con.register("payload", payload)
    con.execute("DELETE FROM observations WHERE series_key = ?", [series_key])
    con.execute("INSERT INTO observations SELECT * FROM payload")
    con.unregister("payload")
    return len(payload)


def read_series(
    con: duckdb.DuckDBPyConnection, keys: list[str], start: str | None = None
) -> pd.DataFrame:
    """Long-format frame of the requested series, optionally from a start date."""
    sql = """
        SELECT o.series_key, o.obs_date, o.value, m.label, m.unit
        FROM observations o
        JOIN series_meta m USING (series_key)
        WHERE o.series_key IN ({placeholders})
    """.format(placeholders=", ".join("?" for _ in keys))
    params: list[object] = list(keys)

    if start is not None:
        sql += " AND o.obs_date >= ?"
        params.append(start)

    sql += " ORDER BY o.series_key, o.obs_date"
    return con.execute(sql, params).df()


def freshness(con: duckdb.DuckDBPyConnection) -> pd.DataFrame:
    """Last observation date per series. Use it to surface stale data on the site."""
    return con.execute(
        """
        SELECT m.series_key, m.label, m.kind,
               MAX(o.obs_date) AS last_obs,
               COUNT(o.value)  AS n_obs,
               m.updated_at
        FROM series_meta m
        LEFT JOIN observations o USING (series_key)
        GROUP BY ALL
        ORDER BY m.series_key
        """
    ).df()
