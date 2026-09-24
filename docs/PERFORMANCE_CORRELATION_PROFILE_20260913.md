# Correlation scaling and profile — 2026-09-13

This pass measures the current complete correlation-analysis preparation path. No application code was changed.

## Method

Bundled R 4.5.3, application modules and preferences loaded. Deterministic normal data with 300 rows and 10/50/100 continuous variables. Complete inputs and inputs with ten percent missing values independently assigned in each column are tested for Pearson, Spearman and Kendall. Automatic method selection and normality testing are disabled so that the three requested methods can be compared directly.

Each of the eighteen scenarios is warmed once and timed three times; reported elapsed seconds are medians. The entire returned result and RNG state are compared exactly after each repeated run. Profiling runs for each hundred-variable scenario execute separately from timing, with another exact complete-result comparison. No other validation workload runs alongside these measurements.

The path includes preprocessing, pairwise tests, confidence intervals and output matrix/table assembly. It excludes file import, startup, UI rendering and export. Missing inputs use the existing pairwise complete-case behavior, so they generally have fewer observations per test; shorter execution with missingness is not an optimization gain.

The number of pairs is 45, 1,225 and 4,950 respectively. Sampling profiles identify candidate hotspots; their sampled percentages are not exact wall-clock component timings, especially for short runs.

Artifacts and reproduction: `output/correlation-profile-20260913/profile.R`, `timing.csv`, `profile.csv` and the raw profiling files. No installer was rebuilt.

## Results

All 54 timed-repeat full-result/RNG comparisons and six profiled full-result comparisons passed exactly.

| Variables | Missing per column | Pearson seconds | Spearman seconds | Kendall seconds |
| ---: | ---: | ---: | ---: | ---: |
| 10 | 0% | 0.01 | 0.01 | 0.05 |
| 10 | 10% | 0.01 | 0.02 | 0.04 |
| 50 | 0% | 0.20 | 0.25 | 1.30 |
| 50 | 10% | 0.20 | 0.25 | 0.98 |
| 100 | 0% | 0.81 | 1.03 | 5.26 |
| 100 | 10% | 0.80 | 1.04 | 4.01 |

Hundred-variable complete-data sampling profiles:

- Pearson: `correlation_pair_rows_and_matrices` accounted for 47.37% inclusive samples; `cor.test.default` for 21.05%. Prioritize the output assembly/formatting path for further inspection.
- Spearman: `cor.test.default` accounted for 46.48%, including `rank` at 25.35%; output assembly accounted for 36.62%. Pairwise missingness means ranks cannot generally be reused across every pair without additional equivalence checks.
- Kendall: `cor.test.default` accounted for 87.33%; `cor` had 70.25% self samples. Output assembly accounted for only 7.44%. Most cost is in the statistical calculation, not the application's preprocessing.

Inclusive percentages overlap and must not be summed. These samples guide the next investigation; they are not precise component elapsed times. No alternate correlation formula or rank reuse was implemented in this profiling pass.
