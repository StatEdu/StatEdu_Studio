# First Kaplan–Meier execution profile — 2026-09-13

The full-server upload/selection/run harness now supports optional profiling. No application code or analysis/output behavior changed in this review.

## Measured bottleneck

Three fresh bundled R 4.5.3 processes ran the existing survival fixture after a first Pearson analysis, matching the preceding startup benchmark. Each run used JIT level 3 and a valid module cache. The KM run includes its real handler and reactive flush, including plot generation in `MockShinySession`.

| Operation | Median elapsed seconds |
| --- | ---: |
| Entire first KM handler/flush | 2.06 |
| Prepare KM result, including first-use dependencies | 0.53 |
| Build result panel | 0.44 |
| Draw survival curve with risk table, including argument construction | 1.00 |

The plotting path is about 49% of this measured run; result-panel construction is about 21%. The preparation time includes initialization and reporting calculations, so it must not be described as pure survival fitting time.

Nested measurements are **not additive**: the plot path includes risk-table plot construction (0.58 seconds) and curve construction (0.07 seconds), because R evaluates those arguments lazily inside the drawing function. The result-panel time includes 12 calls to `survival_simple_table`, totaling a median 0.28 seconds. These costs must not be added to their parent timings again.

The first instrumented profile sampled 0.74 seconds despite about 2.06 seconds of elapsed run time. Its inclusive sample shares include namespace loading (35.81%) and first-use compilation (32.43%). These overlap with analysis/plot paths and with each other; they are diagnostic sample shares, not wall-time fractions or independent savings estimates. Lightweight `proc.time()` wrappers provide the elapsed measurements above. The R profiler uses a 5 ms sampling interval; Windows timing granularity and first-use effects limit precision.

## Implementation decisions

- Do not preload graph/statistical packages merely to shorten the run-button timer: that can shift waiting to startup without improving the complete workflow.
- The graph drawing helper builds the main plot to obtain its actual horizontal range, then applies matching coordinates and aligns grob widths for the curve and risk table. Removing this build requires proving identical scale limits, axes and panel alignment; no shortcut was applied based on this profile alone.
- A smaller, concrete next candidate is repeated table-cell presentation work in `survival_simple_table`: each cell recalculates its column CSS class and body style. Its measured total gives a bounded target; any reuse must preserve classes, footnote markers, localization and export content exactly.

These findings prioritize graphical first-use work and result-table construction over changing numerical KM formulas for this small fixture. They do not generalize to large datasets or other survival methods.

## Verification and reproduction

`scripts/validate_survival_first_profile.R` verifies all three profiled sessions against the previous unprofiled current session using `identical(..., num.eq=FALSE)`: complete correlation/KM result objects, live result HTML/dependencies, notifications and warnings all match exactly. No captured warnings occurred. Both paths emit the existing ggplot coordinate-replacement informational message. This check covers result data and HTML, not a new pixel comparison of plots; plotting code is unchanged.

Use the same bundled runtime, locale/library/cache environment as `PERFORMANCE_SERVER_FIRST_ANALYSIS_20260913.md`. From the repository root, supply the optional third argument to the measurement script:

```text
scripts/measure_server_first_analysis.R current <fresh-id> output/survival-first-profile-20260913/<fresh-name>.Rprof
scripts/validate_survival_first_profile.R
```

The validation script audits the preserved runs `survival-profile-2` through `survival-profile-4`. It writes helper medians and selected profile rows. Run 1 is an exploratory sampling-only profile and is excluded from helper medians. Use fresh IDs for new measurements to keep settings isolated. Omitting the profiler argument preserves the ordinary measurement path.

Artifacts: `output/survival-first-profile-20260913/`; session snapshots remain in `output/server-first-analysis-20260913/`. No installer was rebuilt.
