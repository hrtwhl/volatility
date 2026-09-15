"""Ingestion.

Walks the registry, fetches every raw series, computes every derived series and
writes both into the store. Raw fetches are isolated: one source going down
degrades the run to a warning instead of failing the whole build.
"""

from __future__ import annotations

import logging

import pandas as pd

from . import store
from .registry import DERIVED_SERIES, RAW_SERIES
from .sources import fred
from .transforms import TRANSFORMS

log = logging.getLogger(__name__)

FETCHERS = {
    "fred": fred.fetch,
}


def ingest_raw(con) -> list[str]:
    """Fetch and store every raw series. Returns the keys that failed."""
    failed: list[str] = []

    for spec in RAW_SERIES:
        fetcher = FETCHERS.get(spec.source)
        if fetcher is None:
            log.error("no fetcher registered for source %r (series %s)", spec.source, spec.key)
            failed.append(spec.key)
            continue

        try:
            frame = fetcher(spec.source_id)
        except Exception as exc:
            # Keep going: yesterday's stored history is better than no build.
            log.warning("fetch failed for %s (%s): %s", spec.key, spec.source_id, exc)
            failed.append(spec.key)
            continue

        store.upsert_series(
            con,
            series_key=spec.key,
            label=spec.label,
            unit=spec.unit,
            kind="raw",
            source=spec.source,
            source_id=spec.source_id,
        )
        rows = store.replace_observations(con, spec.key, frame)
        log.info("%s: %d observations through %s", spec.key, rows, frame["obs_date"].max().date())

    return failed


def build_derived(con) -> list[str]:
    """Recompute every derived series from what is currently stored."""
    failed: list[str] = []

    for spec in DERIVED_SERIES:
        transform = TRANSFORMS.get(spec.transform)
        if transform is None:
            log.error("unknown transform %r for series %s", spec.transform, spec.key)
            failed.append(spec.key)
            continue

        source_frame = store.read_series(con, [spec.from_key])
        if source_frame.empty:
            log.warning("cannot build %s: source series %s has no data", spec.key, spec.from_key)
            failed.append(spec.key)
            continue

        source_frame = source_frame.loc[:, ["obs_date", "value"]].copy()
        source_frame["obs_date"] = pd.to_datetime(source_frame["obs_date"])

        result = transform(source_frame)

        store.upsert_series(
            con,
            series_key=spec.key,
            label=spec.label,
            unit=spec.unit,
            kind="derived",
            source="computed",
            source_id=f"{spec.transform}({spec.from_key})",
        )
        rows = store.replace_observations(con, spec.key, result)
        log.info("%s: %d observations", spec.key, rows)

    return failed


def run() -> int:
    """Full ingestion pass. Returns the number of series that failed."""
    logging.basicConfig(level=logging.INFO, format="%(levelname)-7s %(message)s")

    with store.connect() as con:
        failed = ingest_raw(con)
        failed += build_derived(con)

        print()
        print(store.freshness(con).to_string(index=False))

    if failed:
        log.warning("completed with %d failed series: %s", len(failed), ", ".join(failed))
    return len(failed)
