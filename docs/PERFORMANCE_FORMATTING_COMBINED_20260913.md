# Combined number and p-value formatting validation — 2026-09-13

No application code changed in this pass. It measures recent shared-formatting improvements together against the utilities snapshot preceding the p-value prefix change, while keeping the current correlation engine and CI-label reuse unchanged.

## Controls

Five helpers are switched together in `.GlobalEnv`: `format_p`, `format_decimal3`, `format_decimal2`, `normalize_output_decimal_digits`, and `normalize_p_value_format`. Both before/after helper closures use the same environment, ensuring nested formatter/normalizer lookup resolves the intended set. No isolated improvement percentages are added.

All 24 complete-result/diagnostic/RNG comparisons passed: Pearson/Spearman/Kendall, missing and complete data, two actual precisions (2 and 5) and both p-value styles. The benchmark checks complete-result equivalence for each timing fixture as well.

## Measurements

Bundled R 4.5.3, application preferences loaded, deterministic 300-row normal data, 50/100 continuous variables. Explicit methods with normality testing disabled; three alternating rounds after warming and equality checks, median elapsed seconds. Normal timing uses three decimals and APA p values. Validation and export workloads are sequential with timing.

Full analysis means preparation, pairwise tests, confidence limits and table/matrix assembly. Startup, UI rendering and export are excluded. Differences between methods reflect their statistical computation costs, so formatter gains should not be extrapolated equally to every method.

Output verification switches the entire baseline/current helper sets for both analysis and the corresponding screen/HTML/Excel generation, in both p styles. Reproduction and artifacts: `output/formatting-combined-20260913/` (`common.R`, `benchmark.R`, `benchmark.csv`, `verify-output.R`).

Both p-style output checks passed: full analysis objects, screen markup, HTML bytes and Excel sheet names/cells matched exactly. All six benchmark result comparisons also passed.

| Variables | Method | Before | After |
| ---: | --- | ---: | ---: |
| 50 | Pearson | 0.19 | 0.14 |
| 50 | Spearman | 0.23 | 0.20 |
| 50 | Kendall | 1.28 | 1.25 |
| 100 | Pearson | 0.72 | 0.61 |
| 100 | Spearman | 0.96 | 0.83 |
| 100 | Kendall | 5.20 | 5.11 |

At 100 variables, the measured reductions are about 15% for Pearson and 14% for Spearman. Kendall improves only about 2%, consistent with its time being dominated by statistical computation rather than formatting. These are within-run cumulative measurements for the shared formatting work, not total project-wide speedups.

No displayed/report content changed. No installer was rebuilt.
