# Ordinal correlation matrix-check cache prototype — 2026-09-14

An isolated prototype shows a measurable full-analysis benefit. **Not yet integrated into production.** Production `R/analysis_correlation.R` remains identical to the saved baseline.

## Candidate

The factory clones polycor/mvtnorm functions into local environments and substitutes only the matrix-validation `chkcorr()` lookup. A single retained input/result can be reused for an exactly identical, non-object matrix, using `identical(..., num.eq=FALSE)`. Warning/message-producing checks are not retained, errors propagate, and the entry is reset before and after each polychor fit. Quiet FALSE validation results may also be reused; the caller still rejects them normally.

The cloned `checkmvArgs()` still performs bounds, mean and other validation. `pmvnorm()`, its RNG operations, probability integration settings and every probability evaluation remain in the original order. `binBvn()` is cloned only to reference the local probability function. Installed package namespaces are not modified. The application wrapper is replaced only inside experimental R processes; no `R/` source changes were made.

## Full-analysis timing

Bundled Windows R 4.5.3, mvtnorm 1.3.3 and polycor 0.8.2. Three fresh processes per version, sequential with reversed order in set two. Each condition receives a warm-up and a separately timed full `prepare_correlation_results()` call. Both process types construct the experimental factory before timing, but only the candidate uses it. Source loading, factory construction, bootstrap, input generation, UI and exports are excluded.

Fixtures use 10,000 rows, 20 ordinal variables, three categories and 190 pairs, with normality output enabled. Missingness is independently 10% per variable. Seeded fixtures/options match the preceding correlation profiling harness. Time includes result/diagnostic/stdout capture.

| Condition | Baseline runs, seconds | Candidate runs, seconds | Median before → after |
| --- | --- | --- | --- |
| Complete ordinal data | 2.60 / 2.60 / 2.60 | 2.21 / 2.25 / 2.22 | 2.60 → 2.22 |
| Independently missing data | 2.53 / 2.56 / 2.57 | 2.22 / 2.23 / 2.18 | 2.56 → 2.22 |

Median improvements are approximately **14.6% and 13.3%**, with all pairs faster. Across each candidate process's two warm-ups and two measured analyses, 172,608 check-cache hits and 19,344 original matrix checks were observed. These counts are not removed probability calls. No general improvement outside these fixtures or first-use latency is established.

## Equivalence checks

- All six full-analysis baseline/candidate result pairs matched exactly, including all returned values, diagnostics, stdout and RNG. Each result contains 190 pairwise rows.
- Warm-up and measured results also matched within every process/condition.
- 48 direct matrix-check comparisons cover repeated valid/invalid matrices, dimensions, names, numeric limits, NA/NaN/Inf and nonnumeric inputs; outputs/errors/conditions/stdout/RNG matched.
- 15 complete direct polychor comparisons cover two/three/five categories, independent/associated inputs, ML FALSE/TRUE with standard errors, perfect association, zero marginal cells and insufficient dimensions. Full outputs and conditions/RNG matched exactly.
- Production correlation source matches the baseline snapshot; package namespaces and installed files were not patched.

## Remaining integration work

This is a performance/equivalence prototype, not a production-ready cache. It depends on private functions (`binBvn`, `checkmvArgs`, `chkcorr`) and currently lacks a version/body compatibility guard and tested fallback for incompatible packages. Before adoption, add those guards, validate setup failures and fallback behavior, measure first-use and memory costs, and verify full application presentation with the integrated path. No approval question is required for that follow-up; these are implementation/validation tasks.

No memory measurements, production output/export validations or installer build were performed in this prototype review. Artifacts: `output/ordinal-check-cache-20260914/` contains `engine.R`, complete-analysis timing, direct validation, saved comparisons and the production baseline snapshot.
