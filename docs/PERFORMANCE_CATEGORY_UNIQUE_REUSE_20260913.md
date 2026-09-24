# Reuse unique values for category labels — 2026-09-13

`variable_summary_table()` retains the unique values already calculated for unclassed columns and passes them to `value_label_pairs()`. This avoids repeating character conversion, missing-value filtering and unique-value computation when generating category slots. The helper still uses its existing sort and truncation operations. Explicit value labels retain precedence; continuous variables and classed inputs retain their existing behavior. The new helper argument is optional.

## Validation

- `scripts/validate_category_unique_reuse.R`: 56 exact category-label/full-table/condition/RNG comparisons against both the original expression and actual pre-change file. Covers sorted character/Korean/blank values, numeric/logical/factor/ordered values, empty/missing/infinite values, explicit label order and slot limits of 1/2/5.
- `scripts/validate_variable_unique_reuse.R` against the actual baseline: 31 measurement/full-table scenarios passed.
- The complete million-row benchmark table matched exactly.
- `scripts/validate_data_io.R`: all existing checks passed, with the existing haven `write_sas()` deprecation warning.
- `git diff --check` passed.

## Timing

Bundled R 4.5.3, one million rows and ten character columns repeating three categories and a missing value. Five alternating before/after runs of complete variable-table construction, timing diagnostics disabled, yielded medians of **0.22 seconds before and 0.11 seconds after**, about 50% lower. This is not total import or startup time. The unique-value vector is retained temporarily while each column's summary is built; no persistent data cache was introduced.

Artifacts and baseline: `output/category-unique-performance-20260913/`. No analysis formulas or report contents were changed. No installer was rebuilt.
