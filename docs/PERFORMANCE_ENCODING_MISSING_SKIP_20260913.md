# Skip encoding-repair attempts for missing values — 2026-09-13

`repair_text_encoding()` previously included `NA` in the broken-text mask because `iconv(NA)` returns `NA`. It then attempted up to four encoding conversions for those missing entries, all of which leave them missing. The mask now excludes missing inputs after the initial validity checks. Actual text still follows the same conversion order, repair assignments and fallback rules.

## Validation

- `scripts/validate_encoding_missing_skip.R`: 30 exact repaired-vector/normalized-data/condition/RNG comparisons against both the original expression and actual pre-change source. Includes empty/all-missing inputs, Korean/accented text, replacement characters, invalid byte sequences, CP949 bytes, encoding marks, factors, numeric and date inputs.
- `scripts/validate_csv_score_sampling.R` against the actual baseline: 64 score/import/condition/RNG comparisons and sample checks passed.
- `scripts/validate_data_io.R`: all existing checks passed, with the existing haven `write_sas()` deprecation warning.
- Million-value benchmark outputs matched exactly; `git diff --check` passed.

## Timing

Bundled R 4.5.3; five alternating before/after runs; median seconds for `repair_text_encoding()` on one million values:

| Input | Before | After |
| --- | ---: | ---: |
| Valid text, no missing values | 0.13 | 0.13 |
| Half valid text, half missing | 0.08 | 0.06 |
| All missing | 0.05 | 0.02 |

The missing-input cases improved by about 25% and 60% respectively in this run. These are repair-helper timings, not total file import or startup timings. No full-import speedup is claimed from this measurement.

Artifacts and baseline: `output/encoding-missing-performance-20260913/`. Statistical calculations and report content were not changed. No installer was rebuilt.
