"""FRED client.

One function that turns a FRED series id into a tidy frame. Every other source
you add later (Yahoo, ECB SDW, BIS) should expose the same shape — a DataFrame
with `obs_date` and `value` — so ingestion stays source-agnostic.
"""

from __future__ import annotations

import pandas as pd
import requests

from ..config import HISTORY_START, require_fred_key

BASE_URL = "https://api.stlouisfed.org/fred/series/observations"
TIMEOUT = 30


def fetch(series_id: str, start: str = HISTORY_START) -> pd.DataFrame:
    """Full observation history for one FRED series.

    Returns columns `obs_date` (datetime64) and `value` (float). FRED encodes
    missing observations as ".", which becomes NaN here and is dropped.
    """
    params = {
        "series_id": series_id,
        "api_key": require_fred_key(),
        "file_type": "json",
        "observation_start": start,
    }

    response = requests.get(BASE_URL, params=params, timeout=TIMEOUT)
    if response.status_code == 400:
        raise RuntimeError(
            f"FRED rejected the request for {series_id!r}. "
            "Usually this means the series id is wrong or the API key is invalid. "
            f"Response: {response.text[:300]}"
        )
    response.raise_for_status()

    observations = response.json().get("observations", [])
    if not observations:
        raise RuntimeError(f"FRED returned no observations for {series_id!r}.")

    frame = pd.DataFrame(observations)
    frame["obs_date"] = pd.to_datetime(frame["date"])
    frame["value"] = pd.to_numeric(frame["value"], errors="coerce")

    return (
        frame.loc[:, ["obs_date", "value"]]
        .dropna(subset=["value"])
        .sort_values("obs_date")
        .reset_index(drop=True)
    )
