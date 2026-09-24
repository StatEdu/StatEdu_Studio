# Kappa contingency table performance — 2026-09-13

`interrater_pair_table()` retains its original missing-value filtering and factor conversion, then counts the factor cell codes with `tabulate()` instead of asking `table()` to process the factors again. Integer counts, table class, dimensions, labels, unused categories and category order are retained. Tables exceeding the integer cell-code range use the original implementation.

This affects Cohen, Light and weighted kappa. Their formulas and floating-point calculation order are unchanged. Previous performance changes in this file remain intact.

## Validation

- `scripts/validate_kappa_pair_table.R`: 261 exact comparisons of tables, kappa results, warning/message/error text and RNG state. Includes missing values, empty input, unused/reordered/duplicate levels, explicit NA levels, factors, numeric values, blank and Korean labels. Passed against both the reference implementation and the actual pre-change file.
- `scripts/validate_interrater.R`: passed existing agreement reference checks.
- Full nominal/ordinal prepared results and RNG state are identical to the pre-change version. Rendered screen HTML, saved HTML bytes and Excel sheet names/cell data are identical. Presentation content was not changed.

## Timing

Bundled R 4.5.3; five alternating before/after runs, median elapsed seconds; five categories; Light kappa helper only.

| Rows | Raters | Before | After |
| ---: | ---: | ---: | ---: |
| 1,000 | 10 | 0.00 | 0.00 |
| 1,000 | 30 | 0.03 | 0.02 |
| 1,000 | 50 | 0.09 | 0.08 |
| 10,000 | 10 | 0.01 | 0.01 |
| 10,000 | 30 | 0.19 | 0.14 |
| 10,000 | 50 | 0.50 | 0.39 |

The largest measured case improved by about 22%; the small cases have little absolute benefit. A reported zero is below timer resolution, not zero execution time. These figures do not measure full analysis, startup or installer launch time. The integer-range fallback was added after timing and revalidated for result equivalence.

Reproduction artifacts, the pre-change source and export checks are in `output/kappa-table-performance-20260913/`. No installer was rebuilt.
