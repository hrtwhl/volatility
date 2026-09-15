"""The series registry.

This is the file you edit to add data. Nothing else needs to change: ingestion,
storage and the site all read from these two lists.

A RawSeries is fetched from an external source verbatim. A DerivedSeries is
computed from another series by a named transform. Both end up in the same
`observations` table, so downstream code never needs to know which is which.
"""

from __future__ import annotations

from dataclasses import dataclass


@dataclass(frozen=True)
class RawSeries:
    key: str  # our internal name, stable forever
    source: str  # which client fetches it, e.g. "fred"
    source_id: str  # the id that source uses, e.g. "CPIAUCSL"
    label: str  # what a human sees on a chart legend
    unit: str  # "percent", "index", "usd", ...


@dataclass(frozen=True)
class DerivedSeries:
    key: str
    from_key: str  # which series to compute from
    transform: str  # a name registered in transforms.TRANSFORMS
    label: str
    unit: str


RAW_SERIES: list[RawSeries] = [
    RawSeries(
        key="cpi_headline",
        source="fred",
        source_id="CPIAUCSL",
        label="CPI, all items (index, SA)",
        unit="index",
    ),
    RawSeries(
        key="breakeven_5y",
        source="fred",
        source_id="T5YIE",
        label="5-year breakeven inflation",
        unit="percent",
    ),
]

DERIVED_SERIES: list[DerivedSeries] = [
    DerivedSeries(
        key="breakeven_5y_m",
        from_key="breakeven_5y",
        transform="to_monthly",
        label="5-year breakeven (monthly avg)",
        unit="percent",
    ),
    DerivedSeries(
        key="cpi_yoy",
        from_key="cpi_headline",
        transform="yoy_pct",
        label="CPI year-over-year",
        unit="percent",
    ),
]


def raw_by_source(source: str) -> list[RawSeries]:
    return [s for s in RAW_SERIES if s.source == source]


def all_keys() -> list[str]:
    return [s.key for s in RAW_SERIES] + [s.key for s in DERIVED_SERIES]
