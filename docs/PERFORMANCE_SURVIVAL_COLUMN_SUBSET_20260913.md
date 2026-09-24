# Life-table group column selection — 2026-09-13

`survival_life_table()` previously copied every input column when selecting each group, although its interval loop only reads time and event. It now selects these two columns once before group row selection for ordinary data frames containing unclassed columns or standard factors. Custom data frames and other classed columns retain the original path, including subsetting diagnostics. The existing special handling of the `All` group remains unchanged.

No numerical arithmetic, row order, interval labels or output content changed.

## Verification

- `scripts/validate_survival_column_subset.R`: 48 exact result/diagnostic/RNG comparisons against the immediate baseline and the standalone full-column reference. Includes empty data, missing groups, factors with unused levels, an `All` level, duplicate column names, custom frame/column classes, dates and matrix columns.
- `scripts/validate_survival_life_table_build.R`: 30 exact comparisons against the immediate baseline passed.
- Full result/RNG, screen markup, HTML bytes and Excel sheet names/cells matched exactly: `output/survival-column-subset-20260913/verify-output.R`.

## Measurements

Bundled R 4.5.3, application preferences loaded, 10,000 rows, 20 factor groups and 50 intervals. Median elapsed seconds from five alternating before/after runs; benchmark outputs matched exactly.

| Additional columns | Before | After |
| ---: | ---: | ---: |
| 0 | 0.03 | 0.03 |
| 100 | 0.04 | 0.03 |
| 500 | 0.14 | 0.03 |

The wide fixture improved about 79%. This measures the life-table helper, not total analysis or program startup. Benefits depend on how many unused columns reach this function; narrow input showed no measured speedup.

Baseline and reproduction scripts are in `output/survival-column-subset-20260913/`. No installer was rebuilt.
