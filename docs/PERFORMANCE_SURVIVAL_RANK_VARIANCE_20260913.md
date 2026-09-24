# Survival rank-test covariance — 2026-09-13

Full-analysis timing audit: see `PERFORMANCE_SURVIVAL_TIMING_AUDIT_20260913.md`. Rebinding both compared helpers to the same application environment and keeping other current helpers fixed measured 0.3967 to 0.3667 seconds for twenty groups (about 8%). Use that controlled current-context figure rather than the historical 5% full-analysis figure below. The original artifacts and numerical validation remain available.

`survival_weighted_rank_test()` now updates the covariance matrix with elementwise vector arithmetic when there are at least twenty groups. Previously, each event time ran a nested R loop over every group pair. Smaller comparisons retain the scalar loop.

The change preserves multiplication/division order for each diagonal and off-diagonal entry, and the chronological order of matrix accumulation. `outer()` uses an explicit multiplication function, avoiding its default matrix-product shortcut. No statistical approximation or changed summation scheme is used.

## Validation

`scripts/validate_survival_rank_variance.R` compares 384 scenarios against the immediate source baseline and a restored scalar reference. It checks the complete test result, intermediate covariance matrix and score vector, warnings/messages and RNG exactly (`identical(..., num.eq=FALSE)`). Cases span 2/3/4/10/19/20/21/30 groups, all three test weights, delayed entry, tied times, all events and no events.

The end-to-end benchmark compares complete prepared analysis objects before timing. `output/survival-rank-variance-20260913/verify-output.R` passed complete result/RNG, screen markup, HTML bytes, and Excel sheet names/cells comparisons for twenty groups.

The initial threshold of three groups was rejected: batched measurements showed overhead at ten groups (rank test 0.032 to 0.047 seconds). Its results are retained in `benchmark-rejected-threshold3.csv`. The final threshold conservatively enables the new path only at the measured twenty-group case and above.

## Reproduction and scope

Artifacts are in `output/survival-rank-variance-20260913/`. Benchmarks use bundled R 4.5.3 with application preferences loaded and 2,000 observations. Each of five alternating timing rounds batches ten rank-test calls or three complete analysis calls; elapsed time is divided by the batch size before taking the median. This reduces timer quantization for short cases.

Final median elapsed seconds:

| Groups | Rank before | Rank after | Full before | Full after |
| ---: | ---: | ---: | ---: | ---: |
| 2 | 0.024 | 0.024 | 0.040 | 0.040 |
| 10 | 0.033 | 0.033 | 0.220 | 0.220 |
| 20 | 0.060 | 0.033 | 0.457 | 0.433 |

The twenty-group case improved about 45% in the rank-test helper and 5% in complete analysis preparation. Smaller cases showed no measured change. Both baseline and standalone-reference runs passed all 384 comparisons.

The full-analysis measurement includes preflight, fitting, diagnostics, life table and pairwise tests, but excludes UI rendering and export. The initial `profile.R` records a cold exploratory run including compilation/package initialization; its sampling percentages are not used as steady-state timing evidence.

Review also confirmed that preflight selects analysis-role columns before fitting. Therefore the previous wide-column life-table microbenchmark does not establish equivalent whole-analysis acceleration.

No displayed/report content was changed. No installer was rebuilt.
