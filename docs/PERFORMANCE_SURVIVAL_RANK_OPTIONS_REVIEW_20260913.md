# Survival rank-test option hoisting review — 2026-09-13

No application code was changed. An isolated candidate moved entry-presence and weight-method normalization outside the event-time loop in `survival_weighted_rank_test()`.

## Correctness

The existing rank validator passed all 384 standard-option comparisons, including exact test results, covariance matrices, score vectors, diagnostics and RNG state. However, a targeted custom-option probe preserved the numerical result while changing warning counts from ten to two. Thus unconditional hoisting is not a behavior-preserving replacement. Adopting it would require a restricted fast path with the original evaluation behavior retained for other inputs.

## Measurement method

Both function closures use `.GlobalEnv`; the baseline and candidate are installed in the same global helper slot for complete-analysis calls. Inputs, preferences and seeds are fixed. Bundled R 4.5.3, 2,000 rows, 2/20 groups, all three weight methods. Each of five alternating rounds batches three analysis calls, reporting median elapsed seconds per call. Includes full analysis preparation but excludes UI rendering and export.

The final benchmark was rerun alone after validation completed. The earlier exploratory run is excluded: its script was edited while R was still reading it, and it ended with a trailing parse/evaluation failure. Do not edit a running R script or run validation workloads alongside timing runs.

| Groups | Method | Before | Candidate |
| ---: | --- | ---: | ---: |
| 2 | Logrank | 0.0400 | 0.0433 |
| 2 | Breslow | 0.0400 | 0.0400 |
| 2 | Tarone–Ware | 0.0433 | 0.0400 |
| 20 | Logrank | 0.4300 | 0.4067 |
| 20 | Breslow | 0.3767 | 0.4167 |
| 20 | Tarone–Ware | 0.3700 | 0.4100 |

The candidate did not consistently improve elapsed time across methods and changed custom-option diagnostics. It was rejected. These short local measurements do not support a general speedup claim; no further fallback complexity was added to application code.

Artifacts and reproduction scripts: `output/survival-rank-options-review-20260913/`. The candidate remains an artifact only. No displayed/report content changed. No installer was rebuilt.
