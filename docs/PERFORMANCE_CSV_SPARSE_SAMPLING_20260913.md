# CSV sample scanning with leading missing values — 2026-09-13

The previous bounded sampling helper expanded a prefix when it needed more non-missing values, repeatedly scanning earlier values. It now scans non-overlapping chunks of increasing size and retains at most 20 non-missing values. Sample order, character conversion, encoding score and CSV reader selection remain unchanged. This addresses the sparse-column overhead identified in the previous review.

## Validation

`scripts/validate_csv_score_sampling.R` now covers 64 score/import/condition/RNG comparisons and direct sample checks. Both the immediately preceding implementation and the original full-column sampling reference passed. Added cases exercise lengths and non-missing positions around chunk boundaries, scattered values, trailing valid values and an already sufficient initial sample. CSV cases retain UTF-8/CP949, multiline fields, header options and empty files. readr parser-pointer comparisons use actual problem tables, as documented in the prior review.

`scripts/validate_data_io.R` passed all existing checks, with the existing haven `write_sas()` deprecation warning. No statistical or report output code changed.

## Timing

Bundled R 4.5.3. Each timed batch invokes the helper ten times; medians of seven alternating before/after batches, in seconds:

| Values per column | Pattern | Before | After |
| ---: | --- | ---: | ---: |
| 100,000 | No missing values | 0.00 | 0.00 |
| 100,000 | Only last 20 valid | 0.01 | 0.00 |
| 100,000 | All missing | 0.01 | 0.00 |
| 1,000,000 | No missing values | 0.00 | 0.00 |
| 1,000,000 | Only last 20 valid | 0.13 | 0.04 |
| 1,000,000 | All missing | 0.12 | 0.06 |

The million-value trailing-valid case improved about 69% and the all-missing case about 50% for this helper batch. Zero indicates timer resolution. These are not full CSV import timings; the absolute per-call saving is much smaller than the batch values. Scanning a fully missing column still requires examining all values.

Baseline and benchmark: `output/csv-sparse-sampling-20260913/`. No installer was rebuilt.
