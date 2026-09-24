# First analysis after server creation — 2026-09-13

Extended the server-factory compilation check through CSV upload, variable selection, first Pearson correlation and first Kaplan–Meier analysis in a full `MockShinySession`. This turn adds measurement and validation scripts; it does not change application code or numerical methods.

## Measurement

Bundled R 4.5.3, JIT level 3, existing valid module cache, a fresh R process and separate settings/result-store paths per run. Three baseline/current pairs ran sequentially, baseline then current in each pair. A separate current pilot is excluded. The server function is invoked directly, so function-call compilation is included.

| Stage | Baseline median (s) | Current median (s) |
| --- | ---: | ---: |
| Server factory | 0.36 | 0.00 |
| Server invocation and initial flush | 2.79 | 2.75 |
| CSV upload and variable selection | 0.57 | 0.56 |
| Correlation menu and setup | 0.36 | 0.38 |
| First correlation run and reactive flush | 0.60 | 0.42 |
| Survival menu and setup | 0.96 | 0.97 |
| First KM run and reactive flush | 2.04 | 2.04 |
| Per-run total | 7.66 | 7.13 |

The measured sequence improved by 0.53 seconds (about 7%). Totals are medians of each run's sum, not sums of stage medians. No first-analysis slowdown appeared in this fixture. The correlation-stage difference is a whole handler/flush measurement, not evidence that the numerical correlation algorithm itself became faster.

This is a small server-side benchmark. It excludes process launch, bootstrap/package/module loading, browser/Electron rendering and final explicit output reads. It does not measure desktop launch time or prove improvement for other datasets, menu orders or cold disk conditions. Both paths have identical lightweight result-capture wrappers. The fixed ordering of pairs and warm OS cache limit generalization.

## Exact verification

The existing `scripts/fixtures/survival_validation.csv` is uploaded through the real file observer. The real variable-selection request selects all six columns and sets measurement levels. Actual menu transfer inputs and run-button inputs execute Pearson correlation for time/age and grouped KM for time/status/sex, with event value 1 and rate times 100/200/400.

`scripts/validate_server_first_analysis.R` passes `identical(..., num.eq=FALSE)` for all three baseline/current pairs, including both complete analysis result objects, both live result HTML/dependency objects, captured warnings and notifications. Successful result capture and nonempty output HTML are required, so a silently skipped analysis cannot pass. No captured warnings occurred. Both variants emitted the existing ggplot coordinate-replacement informational message.

No displayed/export content changed. No installer was rebuilt.

## Reproduction

From the repository root, use the bundled Rscript with `--vanilla`, `LC_ALL`/`LANG=English_United States.utf8`, the bundled library as `R_LIBS_USER`, `STATEDU_NO_PACKAGE_INSTALL=true`, and a valid isolated `STATEDU_MODULE_CACHE_DIR`. Run:

```text
scripts/measure_server_first_analysis.R baseline <fresh-id>
scripts/measure_server_first_analysis.R current <fresh-id>
scripts/validate_server_first_analysis.R <fresh-id> [other-ids...]
```

The baseline factory comes from `output/server-compilation-review-20260913/baseline.R`; all other application helpers are current in both variants. Use fresh IDs to avoid reusing settings. Artifacts: `output/server-first-analysis-20260913/`. The validation script defaults to trial IDs 1–3.
