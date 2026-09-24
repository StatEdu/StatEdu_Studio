# Current correlation and survival profile refresh — 2026-09-14

No runtime code changed. Re-profiled the current application modules after the recent data-processing work. These are analysis-stage measurements on synthetic, already-prepared numeric/factor inputs; they do not measure or attribute a benefit to the recent CSV/prepare-data changes.

## Current timings

Bundled Windows R 4.5.3 and existing module cache, package installation disabled. Three fresh processes for correlation and three for survival, sequential. Each condition has one warm-up, a separate non-profiled timed run and a 10 ms Rprof run. Times include result/diagnostic/stdout capture; survival capture also normalizes environment attributes as described below. Input creation, bootstrap, UI rendering, exports and application startup are excluded.

| Analysis and fixture | Median elapsed |
| --- | ---: |
| Pearson, 10,000 rows × 20 variables | 0.13 s |
| Spearman, same size | 0.19 s |
| Latent ordinal correlation, complete three-category data | 2.73 s |
| Latent ordinal correlation, independently missing 10% per variable | 2.70 s |
| Kaplan–Meier, 2,000 rows, three groups | 0.12 s |
| Cox, 2,000 rows, six continuous covariates | 0.02 s |

Correlation fixtures use seed 33, 190 variable pairs, normality output enabled, and ordinal cut points -0.5/0.5. Missingness uses a different random subset in each variable. Survival uses seed 778, exponential times, Bernoulli events, three balanced groups and six normally distributed covariates. KM requests rates at 50/100/200 with otherwise default options. Cox uses default settings with no adjusted-group bootstrap, time-varying terms or competing-risk model. Cox elapsed runs were 0.02/0.02/0.04 seconds; these short results must not be generalized to larger or more elaborate survival analyses.

## Priority from profiles

The `mvtnorm::pmvnorm` path accounts for **79.46–80.11%** of sampled total time in complete ordinal correlation and **77.08–79.78%** with missingness. Its callees include `checkmvArgs`, `mvt` and `probval.GenzBretz`; nested percentages must not be added. This is the clearest measured remaining bottleneck in these fixtures. A next bounded review can inspect repeated input validation/setup around probability calculations. No probability approximation, convergence setting, RNG sequence or estimation method has been changed or proposed for automatic substitution.

KM profiles point toward weighted rank tests, but contain only 8–9 total samples per run. Cox has only 1–2 samples per run. Those short profiles do not support precise internal rankings or a new optimization. Pearson/Spearman are also short and should not be overinterpreted from individual samples. The present measurements do not quantify improvement over older reports, which predate several changes and were not rerun as matched baselines here.

## Result checks

- For each of the six conditions in all three processes, warm-up, timed and profiled results matched exactly, including captured diagnostics/stdout and RNG.
- Results also matched across the three processes for every condition. All captured warning/message collections were empty.
- Correlation results were checked for 190 pairwise rows and 20-variable matrices.
- KM returned a `survfit` with 2,000 observations. Cox returned a `coxph` with six finite coefficients and 2,000 model-data rows, confirming successful model fitting rather than an early error path.
- Survival result normalization removes `.Environment` attributes recursively before comparison; numerical values, codes, levels, model outputs and other attributes are retained. Correlation results need no environment normalization. These are repeatability checks, not validation of an unimplemented change or re-evaluation of stripped formula environments.

No memory or export benchmark was run, no analysis/output code changed, and no installer was rebuilt. Snapshots and scripts are in `output/analysis-current-refresh-20260914/`, including `correlation.R`, `survival.R`, `compare.R`, timing tables, complete results and 10 ms profiles.

Measured source SHA-256:

- `R/analysis_correlation.R`: `BAF9F3DAB55739F0178931B31D7D35F1F4AE421FEB501606C5FE14BE511ED85A`
- `R/analysis_survival.R`: `1DD8B328C7D0A35C43A7F467A0A6A896D4B7C489A6D29C713D086483CEB0797C`
- `R/data_io.R`: `E03AF49AFD600192204738B8BCEDAF55DB69C7177BB595842DF59A2B7A51A088`
