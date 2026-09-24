# Guarded ordinal matrix-check cache — 2026-09-14

Added compatibility/fallback protection to the isolated candidate and measured setup, first analysis, warmed analysis and memory. **The candidate remains outside production pending final application integration/presentation verification.** No permission or approval is needed for that remaining work.

## Guards and fallback

Factory enables caching only with polycor 0.8.2 and mvtnorm 1.3.3 and exact SHA-256 fingerprints of function formals/bodies for `polychor`, `binBvn`, `pmvnorm`, `checkmvArgs` and `chkcorr`. Setup uses local clones; installed namespaces/files remain untouched. Unknown versions, changed functions, setup errors or setup warnings return the original `polycor::polychor` quietly. Function fingerprints use digest's serialization; a serialization/body mismatch disables the optimization safely.

Fallback is for setup/compatibility only. A failure during a selected fit is propagated once, not retried through a second fit/RNG path. Per-fit cached state is cleared on exit, including errors. The cache still changes only repeated matrix checks, not bounds/mean checks, probability evaluations, integration settings or RNG calls.

Tests passed for the enabled path, nine fallback cases (two version mismatches, five function-body changes, setup error and warning), and one injected numerical-fit failure confirming no retry and cache cleanup. Forty-eight direct matrix checks and fifteen direct polychor comparisons also passed exactly for values/errors/conditions/stdout/RNG.

## Timing with guards

Bundled Windows R 4.5.3; three fresh processes per version, sequential, order reversed in set two. Same 10,000-row, 20-variable, three-category, 190-pair fixtures and options as the prototype. Normality output enabled; missingness is independently 10% per variable.

| Complete analysis condition | Baseline median | Guarded candidate median |
| --- | ---: | ---: |
| Complete ordinal data, warmed | 2.58 s | 2.21 s |
| Missing ordinal data, warmed | 2.59 s | 2.20 s |

All pairs improved. Candidate factory setup took 0.05/0.04/0.05 seconds. The first complete-data analysis, with candidate setup added per run, had a median of **2.49 seconds**, versus **2.82 seconds** for the baseline first analysis. This sum includes measured factory initialization but excludes bootstrap/source loading, input creation, UI and export. Both experimental modes construct the factory before timing; baseline setup is omitted from this sum because its fit never uses the factory. This is not a literal cold application launch benchmark.

All six before/after full-analysis results matched exactly, including values, conditions, stdout and RNG; warm-up/measured results also matched. A generated harness assertion initially omitted `$cached` because of PowerShell interpolation and stopped before timing. It was corrected and all reported runs completed with the proper enabled-path assertion.

## Memory

A separate three-process-per-version test ran one complete ordinal analysis. Baseline does not construct the candidate engine; current includes construction. PowerShell samples the actual R PID's `PeakWorkingSet64` approximately every 20 ms, including module/package loading, fixture creation and result serialization.

Median peak working set was **275,615,744 before and 278,568,960 bytes after**: **+2,953,216 bytes (2.95 MB, 1.1%)**. Individual pairs vary, including one lower candidate peak. This is modest measured overhead in the test, not zero cost or a prediction for all sessions. All three saved memory-result pairs matched exactly. Electron was not measured.

## Decision and remaining work

The guard and memory results support proceeding to a final integrated implementation. Remaining tasks are lazy production hookup, persistent guard/fallback tests and verification of screen/saved presentation with that exact integrated path. They were not completed in this guard review, so production remains unchanged and no general availability is claimed.

No analysis/export output changed and no installer was rebuilt. Production `R/analysis_correlation.R` matches the baseline snapshot. Artifacts: `output/ordinal-check-guard-20260914/` (`engine.R`, `guards.R`, direct validation, first/warm timings, setup times, memory results and full comparisons).
