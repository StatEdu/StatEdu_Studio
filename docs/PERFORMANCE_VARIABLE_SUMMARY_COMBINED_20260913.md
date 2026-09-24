# Combined variable-summary verification — 2026-09-13

This pass checks the cumulative measurement-type, range, unique-count and category-value reuse changes together. The reference is the actual `R/data_io.R` saved before the measurement-type optimization (`output/measurement-type-performance-20260913/baseline.R`). No additional application runtime code was changed in this pass.

## Integrated timing and equivalence

Bundled R 4.5.3, 300,000 rows and eight columns per case. Five alternating before/after runs, median complete variable-summary seconds, timing diagnostics disabled:

| Data | Before | After | Approximate reduction |
| --- | ---: | ---: | ---: |
| Mixed types | 0.16 | 0.11 | 31% |
| Continuous numeric | 0.11 | 0.06 | 45% |
| All missing numeric | 0.07 | 0.03 | 57% |

The mixed fixture includes numeric, small integer categories, Korean/blank character values, logical, factor, ordered factor, Date and haven-labelled values, plus a variable label. Full variable tables (including metadata, measurement levels, ranges, counts and value-label slots) and RNG state matched the pre-series baseline exactly in all three cases.

These timings isolate variable-table preparation. They are not full import/startup timings, and should not be added to or compared arithmetically with benchmarks from earlier sessions.

## Regression checks

All four focused validation scripts were run against the same pre-series baseline:

- `validate_measurement_type_fastpath.R`: 19 type/condition/RNG cases and three full tables.
- `validate_variable_range_reuse.R`: 34 helper/table/condition/RNG comparisons.
- `validate_variable_unique_reuse.R`: 31 measurement/full-table scenarios.
- `validate_category_unique_reuse.R`: 56 category-label/table/condition/RNG comparisons.

All passed. `validate_data_io.R` also passed all existing checks, with its existing haven `write_sas()` deprecation warning. Counts across scripts overlap and are not a count of distinct real-world datasets.

Artifacts: `output/variable-summary-combined-20260913/benchmark.R` and `benchmark.csv`. No installer was rebuilt.
