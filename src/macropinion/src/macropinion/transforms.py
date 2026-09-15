"""Named transforms for derived series.

Register a function here and you can reference it by name from registry.py.
Each takes a tidy frame (`obs_date`, `value`) and returns the same shape.
"""

from __future__ import annotations

from typing import Callable

import pandas as pd

Transform = Callable[[pd.DataFrame], pd.DataFrame]


def yoy_pct(frame: pd.DataFrame) -> pd.DataFrame:
    """Year-over-year percentage change.

    Compares each observation to the one closest to twelve months earlier rather
    than shifting a fixed number of rows, so this stays correct for series with
    gaps or mixed frequency.
    """
    current = frame.loc[:, ["obs_date", "value"]].sort_values("obs_date").reset_index(drop=True)

    lagged = current.copy()
    lagged["obs_date"] = lagged["obs_date"] + pd.DateOffset(years=1)
    lagged = lagged.rename(columns={"value": "value_lag"})

    merged = pd.merge_asof(
        current,
        lagged,
        on="obs_date",
        direction="nearest",
        tolerance=pd.Timedelta(days=20),
    )

    merged["value"] = (merged["value"] / merged["value_lag"] - 1.0) * 100.0
    return merged.loc[:, ["obs_date", "value"]].dropna(subset=["value"]).reset_index(drop=True)


def pct_change(frame: pd.DataFrame) -> pd.DataFrame:
    """Period-over-period percentage change."""
    out = frame.loc[:, ["obs_date", "value"]].sort_values("obs_date").reset_index(drop=True)
    out["value"] = out["value"].pct_change() * 100.0
    return out.dropna(subset=["value"]).reset_index(drop=True)


def diff(frame: pd.DataFrame) -> pd.DataFrame:
    """First difference in levels. Useful for rates, where bp changes matter."""
    out = frame.loc[:, ["obs_date", "value"]].sort_values("obs_date").reset_index(drop=True)
    out["value"] = out["value"].diff()
    return out.dropna(subset=["value"]).reset_index(drop=True)


def to_monthly(frame: pd.DataFrame) -> pd.DataFrame:
    """Average a higher-frequency series to monthly, dated to the first of the month.

    Matches how FRED dates its own monthly aggregates, and — more importantly —
    how CPI is dated, so a monthly breakeven and CPI year-over-year land on the
    same x positions instead of one stepping across the other.

    The current month is included while still incomplete; it is a partial-month
    average, not a final print.
    """
    out = frame.loc[:, ["obs_date", "value"]].copy()
    out["obs_date"] = pd.to_datetime(out["obs_date"])
    return (
        out.set_index("obs_date")["value"]
        .resample("MS")
        .mean()
        .dropna()
        .reset_index()
    )


TRANSFORMS: dict[str, Transform] = {
    "yoy_pct": yoy_pct,
    "to_monthly": to_monthly,
    "pct_change": pct_change,
    "diff": diff,
}
