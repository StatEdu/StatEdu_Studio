# Krippendorff eligibility aggregation — 2026-09-13

`interrater_krippendorff_alpha()` uses `rowSums(!is.na(values)) >= 2` in place of `apply(!is.na(values), 1L, sum) >= 2`. This counts the same logical indicators without an R callback per row. It changes only selection of rows contributing to the pooled expected disagreement. Pair order, summation order for distances, weighting and formulas remain unchanged.

## Validation

- `scripts/validate_krippendorff_eligibility.R`: 192 exact comparisons of coefficient/error text, warnings, messages and RNG state, against the original expression and the actual pre-change source. Includes zero rows/columns, single rows/raters, all-missing/constant inputs, matrices/data frames and nominal/ordinal/continuous measurement.
- `scripts/validate_krippendorff_pair_reuse.R` against the actual pre-change source: another 93 exact comparisons, including factors, missing ratings and extreme continuous scales.
- `scripts/validate_interrater.R`: existing reference checks passed.
- Complete nominal/ordinal result objects and RNG matched the baseline. Screen HTML, saved HTML bytes and Excel sheet names/cell values matched. Presentation content was not changed.

## Timing

Bundled R 4.5.3; five raters, five categories with missing values; median elapsed seconds from five alternating before/after runs of the complete alpha helper.

| Rows | Measurement | Before | After |
| ---: | --- | ---: | ---: |
| 10,000 | Nominal | 0.09 | 0.08 |
| 10,000 | Ordinal | 0.09 | 0.08 |
| 100,000 | Nominal | 0.86 | 0.80 |
| 100,000 | Ordinal | 0.87 | 0.78 |

The 100,000-row cases improved by about 7% and 10%, respectively. These timings are specific to the alpha helper, not full analysis or startup. Continuous alpha still constructs all pooled rating pairs; its quadratic memory/time cost is not addressed by this change.

Baseline, benchmark and output verification artifacts: `output/kripp-eligibility-performance-20260913/`. No installer was rebuilt.
