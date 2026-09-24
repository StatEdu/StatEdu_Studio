# Correlation matrix name-lookup candidate review — 2026-09-13

No application code was changed. A candidate replaced per-pair `match()` calls in the numeric, CI and method matrix helpers with a precomputed named index vector. It was evaluated as an isolated artifact and rejected.

## Timing

Bundled R 4.5.3, preferences loaded, 300 rows, explicit Pearson with normality testing disabled. All three candidate helper closures and current helper closures resolve dependencies in `.GlobalEnv`; helpers are switched together. Five alternating rounds batch three complete analyses per round. Validation workloads execute after the benchmark, not concurrently.

| Variables | Current seconds | Candidate seconds |
| ---: | ---: | ---: |
| 10 | 0.0067 | 0.0067 |
| 50 | 0.1767 | 0.1800 |
| 100 | 0.7333 | 0.7500 |

The candidate showed no full-analysis speedup. Name indexing and removal of result names did not improve over `match()` in these fixtures. Timing includes preparation, tests and table/matrix assembly; startup, rendering and export are excluded.

## Equivalence and decision

All three complete benchmark result comparisons and 27 ordinary matrix/result/diagnostic/RNG comparisons passed exactly. Matrix fixtures include duplicates, unknown names, empty pairs, spaces and Korean names. However, explicit `NA` variable names produced different results in all three candidate helpers: vector name indexing did not preserve `match()` behavior. This would require additional fallback handling before adoption, with no measured speed benefit to justify it.

The candidate remains only in `output/correlation-index-review-20260913/candidate.rds`. Reproduction: `benchmark.R`, `benchmark.csv` and `validate.R` in that directory. Existing application source is byte-identical to the start-of-pass snapshot. No displayed/report content changed. No installer was rebuilt.
