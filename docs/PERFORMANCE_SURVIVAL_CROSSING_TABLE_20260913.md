# Build the crossing comparison table once — 2026-09-13

Full-analysis timing audit: `PERFORMANCE_SURVIVAL_TIMING_AUDIT_20260913.md` aligns both helper environments while holding other current helpers fixed. It measured 0.4400 to 0.3767 seconds for twenty groups (about 14%). Use those controlled current-context times in preference to the historical full-analysis times below; original artifacts and result-equivalence evidence are retained.

`survival_km_crossing_diagnostics()` now collects each pair's results as a list and constructs the data frame once from the completed columns. Previously every pair constructed a one-row data frame, followed by `rbind`. The pair order, calculations, column names/types and row names are preserved. Early empty-result returns remain unchanged.

## Validation

- `scripts/validate_survival_crossing_table.R`: all 96 exact table/condition/RNG comparisons passed against both the immediate baseline and a standalone reference restoring row-wise data frames.
- Existing `scripts/validate_survival_crossing_followup.R`: all 96 comparisons passed against the immediate baseline.
- Benchmark helper and complete analysis objects matched exactly (`identical(..., num.eq=FALSE)`).
- Complete result/RNG, screen markup, HTML bytes and Excel sheet names/cells matched exactly for four groups; reproduced by `output/survival-crossing-table-20260913/verify-output.R`.

## Timing

Bundled R 4.5.3 and application preferences loaded. Five alternating before/after rounds, each batching three calls; median elapsed seconds per call. Helper inputs are fitted models from 20,000 rows:

| Groups | Before | After |
| ---: | ---: | ---: |
| 2 | 0.0100 | 0.0100 |
| 10 | 0.0400 | 0.0367 |
| 20 | 0.1933 | 0.1167 |

The twenty-group crossing helper improved about 40% in this run. A separate complete-analysis benchmark with 2,000 rows and twenty groups measured **0.4833 to 0.4167 seconds**, about 14% faster. Complete analysis includes preflight, fitting, diagnostics, life table and pairwise tests; it excludes UI rendering and export. Compare within this run, not with absolute timings from earlier sessions.

Baseline and reproduction scripts are in `output/survival-crossing-table-20260913/`. No displayed/report content changed. No installer was rebuilt.
