# Reuse unique values for integer-like classification — 2026-09-13

When a numeric column has at most 12 unique values, measurement inference tests whether all values are sufficiently close to integers. Repeated values do not affect that predicate. The variable-summary path now passes its already computed unique values to the existing `numeric_integer_like()` calculation. No tolerance, rounding operation, threshold or classification rule changed. Classed inputs retain the existing full-value path, and direct helper callers retain the original behavior by default.

## Validation

- `scripts/validate_variable_unique_reuse.R`: 40 measurement/full-table scenarios, conditions and RNG matched both the actual baseline and a reference using full-value checks. Added positive/negative cases below/at/above the existing integer tolerance and mixed infinities/NaN.
- `scripts/validate_measurement_type_fastpath.R` against the baseline: 19 type/condition/RNG cases and three full variable tables passed, including custom conversion diagnostics.
- Both complete benchmark tables matched exactly.
- `scripts/validate_data_io.R`: all existing checks passed, with the existing haven `write_sas()` deprecation warning.
- `git diff --check` passed.

## Timing

Bundled R 4.5.3; one million rows and ten numeric columns repeating five values and NA. Five alternating before/after runs of complete variable-summary construction, timing diagnostics disabled:

| Values | Before median | After median |
| --- | ---: | ---: |
| Integers 1–5 | 0.32 s | 0.14 s |
| Values 1.25–5.25 | 0.56 s | 0.14 s |

These fixture-specific reductions are about 56% and 75%. They apply to variable-table preparation with repeated low-cardinality numeric values, not all numeric data or total file import. The latter fixture retains its continuous classification despite few distinct values.

Artifacts and baseline: `output/integer-category-performance-20260913/`. Analysis formulas and report content were not changed. No installer was rebuilt.
