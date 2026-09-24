# Measurement inference from standard types — 2026-09-13

`infer_measurement()` now returns directly for standard logical, factor and ordered-factor inputs. Their existing measurement rules depend on type and factor levels, not observed values, so full-vector conversion, missing-value filtering and unique-value counting were unused work. Custom subclasses retain the original conversion path and diagnostics. Character/numeric/date and other inference rules are unchanged.

## Verification

- `scripts/validate_measurement_type_fastpath.R`: 19 exact type/result/condition/RNG cases and three complete variable-summary tables, against both the original function body and the actual pre-change source.
- Includes empty/all-missing inputs, unused factor levels, ordered factors, logical matrices, numeric/character/date/list values and custom subclasses whose conversion emits a warning.
- A separate million-row, ten-factor-column variable summary also matched exactly.
- `scripts/validate_data_io.R`: all existing checks passed, with the existing haven `write_sas()` deprecation warning.
- `git diff --check` passed.

## Timing

Bundled R 4.5.3, alternating before/after runs. Seven timed batches of ten inference calls on a million-value column gave medians of 0.06 seconds before for logical and 0.14 seconds before for factor/ordered-factor; all after medians were below timer resolution.

For the complete variable-summary table on one million rows and ten factor columns, five-run medians were **0.64 seconds before and 0.50 seconds after**, about 22% lower. Summary timing diagnostics were disabled for measurement. This is table preparation, not total file import or application launch time.

Baseline, benchmarks and results: `output/measurement-type-performance-20260913/`. No analysis formulas or report contents were changed. No installer was rebuilt.
