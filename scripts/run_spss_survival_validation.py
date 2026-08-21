"""Run the fixed Kaplan-Meier and Cox benchmark with IBM SPSS Statistics.

Run this file with the Python launcher bundled with SPSS Statistics, for example:

  statisticspython3.bat scripts/run_spss_survival_validation.py \
      --output outputs/spss_survival_validation.xml
"""

from __future__ import annotations

import argparse
from pathlib import Path

import spss


def spss_path(path: Path) -> str:
    return path.resolve().as_posix().replace("'", "''")


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--output",
        default="outputs/spss_survival_validation.xml",
        help="OMS OXML output path, relative to the repository root by default.",
    )
    args = parser.parse_args()

    repo_root = Path(__file__).resolve().parents[1]
    fixture = repo_root / "scripts" / "fixtures" / "survival_validation.csv"
    output = Path(args.output)
    if not output.is_absolute():
        output = repo_root / output
    output.parent.mkdir(parents=True, exist_ok=True)
    if output.exists():
        output.unlink()

    syntax = f"""
SET UNICODE=ON.

GET DATA
 /TYPE=TXT
 /FILE='{spss_path(fixture)}'
 /ENCODING='UTF8'
 /DELCASE=LINE
 /DELIMITERS=","
 /QUALIFIER='"'
 /ARRANGEMENT=DELIMITED
 /FIRSTCASE=2
 /IMPORTCASE=ALL
 /VARIABLES=
  id F3.0
  time F8.0
  status F1.0
  sex F1.0
  ph_ecog F1.0
  age F3.0.

DATASET NAME SurvivalValidation.

OMS
 /SELECT TABLES
 /DESTINATION FORMAT=OXML OUTFILE='{spss_path(output)}'.

KM time BY sex
 /STATUS=status EVENT(1)
 /PRINT TABLE MEAN
 /TEST=LOGRANK.

COXREG VARIABLES=time WITH age sex
 /STATUS=status EVENT(1)
 /METHOD=ENTER age sex
 /PRINT=DEFAULT CI(95).

OMSEND.
"""

    spss.StartSPSS()
    try:
        spss.Submit(syntax)
    finally:
        spss.StopSPSS()

    if not output.exists() or output.stat().st_size == 0:
        raise RuntimeError(f"SPSS did not create the expected OXML output: {output}")
    print(output)


if __name__ == "__main__":
    main()
