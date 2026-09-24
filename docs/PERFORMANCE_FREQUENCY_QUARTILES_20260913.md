# Descriptive quartile reuse — 2026-09-13

`descriptive_table_for_variable()` now calculates Q1 and Q3 together with one type-7 `quantile()` call and reuses their difference for IQR. Previously it called `quantile()` separately for each quartile, followed by `IQR()`, which computes those quartiles again. Median, moments, formatting and missing-value handling remain unchanged.

## Verification

- `scripts/validate_frequency_quartiles.R`: 52 exact table/condition/RNG comparisons against both the pre-change source and an independent reference path. Covers empty input, missing values, singletons, ties, infinities, factors, invalid numeric text, odd/even sample sizes and very small/large numeric scales.
- Mixed categorical/continuous full results and RNG state matched the baseline exactly. Rendered screen HTML, saved HTML bytes and Excel sheet names/cell data matched. Output content and presentation were not changed.
- Existing `scripts/validate_frequencies_screen_table_contract.R` checks passed.
- Benchmark and output verification script: `output/frequency-quartiles-performance-20260913/verify.R`; pre-change source is stored alongside it.

## Timing

Bundled R 4.5.3, application modules and preferences loaded. Five alternating before/after runs, median elapsed seconds for one variable's complete descriptive table:

| Rows | Before | After |
| ---: | ---: | ---: |
| 10,000 | 0.00 | 0.00 |
| 100,000 | 0.02 | 0.02 |
| 1,000,000 | 0.15 | 0.14 |

The measured benefit is small (about 7% in the largest case) and timing resolution limits interpretation. Zero does not mean zero execution time. This is removal of redundant work, not a major overall speedup; startup and installer loading were not measured. No installer was rebuilt.
