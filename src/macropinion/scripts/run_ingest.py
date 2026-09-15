#!/usr/bin/env python
"""Entry point for ingestion. Run: python scripts/run_ingest.py"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "src"))

from macropinion.ingest import run  # noqa: E402

if __name__ == "__main__":
    sys.exit(0 if run() == 0 else 1)
