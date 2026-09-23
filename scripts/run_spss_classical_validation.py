"""Run fixed classical-statistics benchmarks with IBM SPSS Statistics.

The script is intentionally cumulative: later validation stages can add new
SPSS procedures while preserving the same fixed input and OMS extraction path.
Run it with the Python launcher bundled with SPSS Statistics.
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
        default="outputs/spss_classical_validation.xml",
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

DATASET NAME ClassicalValidation.

COMPUTE age_m=age.
IF (id <= 2) age_m=$SYSMIS.
COMPUTE status_m=status.
IF (id = 1) status_m=$SYSMIS.
COMPUTE rare=0.
IF (id <= 5) rare=1.
COMPUTE item1=age/10 + MOD(id,5)*.20.
COMPUTE item2=item1*.80 + MOD(id*3,7)*.15.
COMPUTE item3=item1*1.10 + MOD(id*5,6)*.10.
COMPUTE item4=item1*.90 + MOD(id*7,8)*.12.
COMPUTE rm1=age + MOD(id,4)*.25.
COMPUTE rm2=rm1 + 2.5 + MOD(id*2,5)*.20.
COMPUTE rm3=rm1 + 5.0 + MOD(id*3,7)*.15.
EXECUTE.

OMS
 /SELECT TABLES
 /DESTINATION FORMAT=OXML OUTFILE='{spss_path(output)}'.

FREQUENCIES VARIABLES=time age age_m
 /FORMAT=NOTABLE
 /STATISTICS=MEAN STDDEV MEDIAN MINIMUM MAXIMUM SKEWNESS KURTOSIS
 /PERCENTILES=25 75
 /ORDER=ANALYSIS.

FREQUENCIES VARIABLES=status sex ph_ecog status_m
 /ORDER=ANALYSIS.

CROSSTABS
 /TABLES=sex BY status
 /FORMAT=AVALUE TABLES
 /STATISTICS=CHISQ PHI
 /CELLS=COUNT EXPECTED ROW COLUMN TOTAL.

CROSSTABS
 /TABLES=rare BY status
 /FORMAT=AVALUE TABLES
 /STATISTICS=CHISQ PHI
 /CELLS=COUNT EXPECTED ROW COLUMN TOTAL.

CORRELATIONS
 /VARIABLES=time age
 /PRINT=TWOTAIL NOSIG FULL
 /MISSING=PAIRWISE.

NONPAR CORR
 /VARIABLES=time age
 /PRINT=SPEARMAN TWOTAIL NOSIG
 /MISSING=PAIRWISE.

RELIABILITY
 /VARIABLES=item1 item2 item3 item4
 /SCALE('Fixed four-item scale') ALL
 /MODEL=ALPHA
 /STATISTICS=DESCRIPTIVE SCALE CORR
 /SUMMARY=TOTAL.

T-TEST GROUPS=sex(1 2)
 /MISSING=ANALYSIS
 /VARIABLES=age
 /CRITERIA=CI(.95).

ONEWAY age BY ph_ecog
 /STATISTICS DESCRIPTIVES HOMOGENEITY
 /MISSING ANALYSIS
 /POSTHOC=TUKEY ALPHA(.05).

NPAR TESTS
 /M-W=age BY sex(1 2)
 /K-W=age BY ph_ecog(0 2)
 /MISSING ANALYSIS.

REGRESSION
 /MISSING LISTWISE
 /STATISTICS COEFF OUTS R ANOVA CI(95)
 /DEPENDENT time
 /METHOD=ENTER age sex ph_ecog.

LOGISTIC REGRESSION VARIABLES status
 /METHOD=ENTER age sex ph_ecog
 /CONTRAST (sex)=INDICATOR(1)
 /CONTRAST (ph_ecog)=INDICATOR(0)
 /PRINT=CI(95) GOODFIT
 /CRITERIA=PIN(.05) POUT(.10) ITERATE(20) CUT(.5).

UNIANOVA time BY ph_ecog WITH age
 /METHOD=SSTYPE(3)
 /INTERCEPT=INCLUDE
 /PRINT=DESCRIPTIVE ETASQ PARAMETER HOMOGENEITY
 /EMMEANS=TABLES(ph_ecog) WITH(age=MEAN)
 /CRITERIA=ALPHA(.05)
 /DESIGN=age ph_ecog.

GLM rm1 rm2 rm3
 /WSFACTOR=timepoint 3 Polynomial
 /METHOD=SSTYPE(3)
 /PRINT=DESCRIPTIVE ETASQ
 /CRITERIA=ALPHA(.05)
 /WSDESIGN=timepoint.

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
