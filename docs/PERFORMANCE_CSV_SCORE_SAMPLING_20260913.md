# Bounded CSV encoding sample — 2026-09-13

Follow-up: `PERFORMANCE_CSV_SPARSE_SAMPLING_20260913.md` documents removal of repeated prefix scans for sparse columns. Measurements below describe the initial implementation.

`csv_encoding_score()` consumes only the first 20 non-missing values of each character column. Previously it removed missing values from the entire column before taking the sample. `csv_encoding_text_sample()` now examines a prefix and doubles its length only when more non-missing values are needed. Conversion to character, sample order, scoring and encoding-selection rules are unchanged.

## Validation

- `scripts/validate_csv_score_sampling.R`: 45 exact score/import/condition/RNG comparisons plus direct sample checks, against both the actual baseline and a reference using the original sampling expression. Cases include sample-size boundaries, leading/all missing values, blank/replacement/Korean/accented strings, UTF-8/CP949 CSVs, multiline fields, header options and empty files.
- A separate 150,000-row, ten-character-column CSV above 10 MiB exercised the readr path. Imported values, attributes, parser problem tables, conditions and RNG matched.
- readr creates a different external parser pointer per invocation. Tests compare its actual problem table and all remaining attributes instead of pointer identity; the initial raw identity check failed only on that pointer.
- `scripts/validate_data_io.R`: all existing CSV, Excel, SPSS-related helper, Stata, SAS and DAT checks in the suite passed. The suite emitted its existing haven `write_sas()` deprecation warning.
- `git diff --check` passed.

## Timing and limits

Bundled R 4.5.3, five alternating before/after runs, median elapsed seconds for scoring ten character columns:

| Rows | Before | After |
| ---: | ---: | ---: |
| 10,000 | 0.00 | 0.00 |
| 100,000 | 0.01 | 0.00 |
| 1,000,000 | 0.08 | 0.00 |

Zero indicates timer resolution, not zero execution time. The full 150,000-row CSV import median was **0.61 seconds both before and after**; no full-import speedup is established by that fixture. This change avoids unnecessary full-column sampling work, with greater relevance to long character columns. Sparse/all-missing columns still require scanning to the end and repeated prefix processing can add overhead; no universal speedup is claimed.

Artifacts and baseline: `output/csv-score-sampling-20260913/`. Analysis formulas and report content were not changed. No installer was rebuilt.
