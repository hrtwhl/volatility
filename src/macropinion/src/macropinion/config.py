"""Central configuration.

Every path and secret used anywhere in the project resolves here, so there is
exactly one place to look when something points at the wrong file.
"""

from __future__ import annotations

import os
from pathlib import Path

from dotenv import load_dotenv

load_dotenv()

# Repo root, resolved from this file rather than the working directory, so that
# scripts behave the same whether run from the repo root, from scripts/, or by
# an Observable data loader running with site/ as its cwd.
ROOT = Path(__file__).resolve().parents[2]

DATA_DIR = ROOT / "data"
DB_PATH = Path(os.getenv("MACROPINION_DB_PATH", DATA_DIR / "warehouse.duckdb"))

FRED_API_KEY = os.getenv("FRED_API_KEY", "")

# How far back to pull. Ingestion always fetches full history; the five-year
# window on the chart is applied at read time so you can widen it without
# re-ingesting anything.
HISTORY_START = os.getenv("HISTORY_START", "1990-01-01")


def require_fred_key() -> str:
    if not FRED_API_KEY:
        raise RuntimeError(
            "FRED_API_KEY is not set. Copy .env.example to .env and add your key, "
            "or export FRED_API_KEY in the shell."
        )
    return FRED_API_KEY
