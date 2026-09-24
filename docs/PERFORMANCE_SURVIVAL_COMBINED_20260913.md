# Combined survival performance validation — 2026-09-13

This pass validates the accumulated life-table, rank-test, crossing-diagnostic and posthoc-table optimizations together. No additional application code was changed.

Measurement correction: a later audit aligned the baseline helper closures with the application global environment and switched all four helpers there together. All 27 complete-result checks passed again. The corrected measurements below supersede the initial approximately 23% twenty-group estimate with approximately 21%. Reproduction: `output/survival-posthoc-index-review-20260913/benchmark-combined-corrected.R` and its CSV.

## Baseline and method

The baseline is the preserved source from `output/survival-life-table-build-20260913/baseline.R`, before the recent life-table row-construction work. Four helpers are switched together: `survival_life_table`, `survival_weighted_rank_test`, `survival_km_crossing_diagnostics`, and `survival_km_posthoc_table`. All other current application modules and preferences are shared. This comparison does not cover every optimization made since the beginning of the project.

`output/survival-combined-20260913/validate-benchmark.R` compares full prepared results, diagnostic messages and RNG state exactly (`identical(..., num.eq=FALSE)`). All 27 cases passed: 2/4/20 groups, logrank/Breslow/Tarone–Ware weights, mixed/all events and tied times. All three benchmark result comparisons also passed.

## End-to-end timing

Bundled R 4.5.3, 2,000 rows, preferences loaded. Five alternating before/after rounds, three calls per round, median elapsed seconds per call. Includes preflight, fitting, crossing diagnostics, life table and pairwise tests. Excludes program startup, UI rendering and export.

| Groups | Before | After | Reduction |
| ---: | ---: | ---: | ---: |
| 2 | 0.0400 | 0.0433 | small increase, about 8% |
| 10 | 0.2200 | 0.2000 | about 9% |
| 20 | 0.4667 | 0.3700 | about 21% |

The two-group difference is small and sensitive to timing resolution. Individual improvement percentages from earlier sessions cannot be added, and absolute timings should not be compared across separate benchmark runs.

## Output verification

`output/survival-combined-20260913/verify-output.R` passed exact full result/RNG, screen markup, HTML bytes and Excel sheet names/cells comparisons for four groups with all four helpers switched together.

Artifacts include the copied baseline, validation/benchmark script, CSV and export verifier in `output/survival-combined-20260913/`. No displayed/report content changed. No installer was rebuilt.
