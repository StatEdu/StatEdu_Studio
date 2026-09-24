# Fleiss kappa count aggregation — 2026-09-13

`interrater_fleiss_kappa()` now constructs the subject/category count matrix with one `match()` and `tabulate()` pass, instead of an R callback for each subject. The original `as.matrix()` conversion, exclusion of missing ratings, integer counts, eligibility rules and subsequent statistical formulas are preserved. Empty input and cell counts exceeding the integer index range retain the original path.

## Validation

- `scripts/validate_fleiss_counts.R`: 600 exact comparisons of results, warning/message/error text and RNG state against the actual pre-change source. Cases include 0/1/2/30/1,000 rows, 2/3/5/20 raters, matrices, data frames, factors, missing/infinite values, reordered/unused/duplicate/NA categories and empty levels.
- Existing `scripts/validate_interrater.R` reference checks passed.
- Full nominal and ordinal prepared results and RNG state matched the baseline exactly. Screen HTML, saved HTML bytes and Excel sheet names/cell values matched. No displayed or exported content was changed.

## Measurements

Bundled R 4.5.3; five categories and ten raters; median elapsed seconds from five alternating before/after runs of the Fleiss helper.

| Rows | Before | After |
| ---: | ---: | ---: |
| 1,000 | 0.00 | 0.00 |
| 10,000 | 0.03 | 0.00 |
| 100,000 | 0.26 | 0.02 |

The largest measured case was about 13 times faster (92% less elapsed time). Zero means below timer resolution. These are helper timings, not whole-analysis or startup timings. The vectorized path allocates full rating-code/index vectors, trading temporary memory proportional to the number of ratings for fewer R callbacks; peak memory was not measured.

Baseline, benchmarks and output comparison artifacts: `output/fleiss-counts-performance-20260913/`. The installer was not rebuilt.
