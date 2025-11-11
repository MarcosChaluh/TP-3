"""Utility to execute project R scripts via rpy2."""
from __future__ import annotations

import argparse
import sys
from pathlib import Path

try:
    from rpy2 import robjects
except Exception as exc:  # pragma: no cover - import failure path
    robjects = None
    _IMPORT_ERROR = exc
else:
    _IMPORT_ERROR = None


def run_r_script(script_path: Path) -> None:
    """Run an R script using rpy2."""
    if robjects is None:
        raise RuntimeError("rpy2 is not available") from _IMPORT_ERROR
    robjects.r["source"](str(script_path))


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "scripts",
        nargs="*",
        type=Path,
        default=[
            Path("scripts/1a_market_rates.R"),
            Path("scripts/1b_inequality.R"),
            Path("scripts/1c_poverty.R"),
            Path("scripts/1d_mincer.R"),
            Path("scripts/2_remote_work.R"),
        ],
        help="Paths to R scripts to execute via rpy2.",
    )
    args = parser.parse_args(argv)

    if robjects is None:
        parser.error(
            "rpy2 is not installed or failed to import: "
            f"{type(_IMPORT_ERROR).__name__}: {_IMPORT_ERROR}"
        )

    for script in args.scripts:
        if not script.exists():
            parser.error(f"R script not found: {script}")
        run_r_script(script)

    return 0


if __name__ == "__main__":
    sys.exit(main())
