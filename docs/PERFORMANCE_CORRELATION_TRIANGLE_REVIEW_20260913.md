# Correlation display triangle review — 2026-09-13

**Candidate rejected; application source is unchanged from the start of this review.**

The two display-table helpers scan every matrix cell but fill only entries below the diagonal. The candidate restricted each row's column loop to `seq_len(min(ncol(matrix), row - 1L))`, preserving row-major formatting order and leaving the diagonal/upper triangle blank. It removed unnecessary loop visits but did not produce a consistent measured speedup.

## Results

Bundled R 4.5.3 with preferences/modules loaded; baseline and candidate closures use the same global helper environment. Five rounds alternate variant order; each sample averages three display-table conversions. These timings exclude UI HTML rendering, statistical fitting and process startup.

| Matrix | Output | Baseline median (s) | Candidate median (s) |
| --- | --- | ---: | ---: |
| 50 × 50 finite | Correlation | 0.0100 | 0.0100 |
| 100 × 100 finite | Correlation | 0.0400 | 0.0367 |
| 200 × 200 finite | Correlation | 0.1500 | 0.1567 |
| 100 × 100 finite | p/CI | 0.0400 | 0.0400 |
| 200 × 200 finite | p/CI | 0.1567 | 0.1567 |
| 200 × 200 all missing | Correlation | 0.0033 | 0.0033 |
| 200 × 200 all missing | p/CI | 0.0467 | 0.0467 |

The 100-variable correlation conversion improved slightly, but the 200-variable conversion was slower. Other measured cases were equal at this timing resolution. These small differences do not support a consistent improvement; the reduced iteration count alone is insufficient performance evidence. The candidate was removed from application code.

## Verification and preserved artifacts

- 225 exact comparisons cover rectangular/empty matrices, nonfinite values, absent or undersized CI matrices, formatter call order, warnings and RNG state. Candidate outputs/conditions match the baseline.
- Complete screens, saved HTML bytes, Excel sheet names and cell contents match for a ten-variable Pearson result and a mixed-variable result using the latent-correlation option. Both display helpers are swapped together for each full rendering/export path.
- The restored application file is byte-identical to the snapshot taken at the start of this turn, preserving preexisting work. `git diff --check` passed for the relevant files.
- No installer was rebuilt; no application optimization from this candidate remains installed.

`output/correlation-triangle-display-20260913/` contains baseline/candidate snapshots, shared setup, benchmark, raw timings, summary and export artifacts. The benchmark column named `current` denotes the **saved candidate**, not the currently installed application. Shared setup explicitly loads that candidate snapshot so future reproduction remains valid after rejection.

Run `scripts/validate_correlation_triangle_display.R` and the artifact directory's `benchmark.R` sequentially from the repository root using bundled Rscript `--vanilla`, the bundled library, English UTF-8 Windows locale, `STATEDU_NO_PACKAGE_INSTALL=true` and an isolated module cache.
