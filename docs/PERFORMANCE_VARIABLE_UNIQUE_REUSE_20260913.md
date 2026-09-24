# Reuse unique counts in variable summaries — 2026-09-13

For unclassed inputs, `variable_summary_table()` now computes the unique count once and passes it with the already prepared non-missing values to `infer_measurement()`. The displayed unique count reuses that same result. The optional helper arguments default to the original computation; classed inputs retain the original conversion/counting path. Binary/category/continuous thresholds and integer-like tolerance are unchanged.

## Verification

- `scripts/validate_variable_unique_reuse.R`: 31 scenarios compare both measurement inference and full variable tables, including conditions and RNG. Covers 1/2/3/12/13 unique-value boundaries, integer tolerance, missing values, infinities, extreme scales, character/logical/factor/ordered/date inputs.
- Both the actual pre-change source and an independent reference with repeated preparation/counting passed.
- `scripts/validate_variable_range_reuse.R` against the actual baseline: 34 additional helper/table/condition/RNG comparisons passed, including custom conversion diagnostics.
- `scripts/validate_data_io.R`: all existing checks passed, with the existing haven `write_sas()` deprecation warning.
- The complete million-row benchmark table matched exactly; `git diff --check` passed.

## Timing

Bundled R 4.5.3, one million rows and ten numeric columns, every tenth value missing. Five alternating before/after runs with timing diagnostics disabled gave median complete variable-summary times of **0.69 seconds before and 0.34 seconds after**, about 51% lower in this run.

The before median differs from earlier turns' timings; compare the paired measurements within this run, not timings from separate sessions. This measures variable-table preparation, not total import or application launch. Gains depend on the unique count, type and size of input columns.

Baseline and benchmark artifacts: `output/variable-unique-performance-20260913/`. No analysis formulas or report content changed. No installer was rebuilt.
