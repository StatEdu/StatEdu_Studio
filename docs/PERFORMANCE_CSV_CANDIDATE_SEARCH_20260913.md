# CSV candidate character counting — 2026-09-13

`csv_encoding_candidates_from_bytes()` now decodes each converted UTF-8 string to code points once and counts the same Latin-1 supplement and Korean character ranges. Previously it obtained every matching position through two regular-expression searches, then counted positions. Input coverage, candidate encodings, weights, invalid/replacement checks, tie order and BOM/empty handling are unchanged.

An initial `perl = TRUE` regex candidate passed small equivalence cases but stalled in the representative full-size benchmark and was interrupted. It was discarded; `rejected-perl.R` records that candidate. The retained implementation uses `utf8ToInt()`, not that regex change.

## Validation

- `scripts/validate_csv_candidate_search.R`: 125 byte scenarios compare exact intermediate scores, final rankings, conditions and RNG against both the actual pre-change file and a reference with the original regex counts. Includes supported encodings, BOM/NUL/invalid/random bytes, range boundaries, supplementary characters, and every nonzero nonsurrogate BMP code point.
- Four full CSV imports matched for values, attributes, actual readr problem tables, conditions and RNG. External parser pointers are compared through their problem tables.
- `scripts/validate_data_io.R`: all existing checks passed, with the existing haven `write_sas()` deprecation warning.
- `git diff --check` passed.

## Full import timing

Bundled R 4.5.3, five alternating before/after runs per fixture, median seconds for complete `read_csv_robust()` calls. Ten columns; local file/namespace caches reused.

| Fixture | Rows | Before | After |
| --- | ---: | ---: | ---: |
| Korean UTF-8, 2.6 MB | 20,000 | 0.83 | 0.55 |
| Korean UTF-8, 19.5 MB | 150,000 | 0.47 | 0.37 |
| UTF-8, half missing, 12 MB | 150,000 | 0.36 | 0.28 |
| Korean CP949, 1.8 MB | 20,000 | 0.88 | 0.57 |

The small-file fixtures improved by about 34–35%, and large-file fixtures by about 21–22%. Small and large files retain their different readers at the existing 10 MiB threshold. Measurements do not establish application-launch improvement or performance on every input/OS.

Artifacts and baseline: `output/csv-candidate-search-20260913/`. Statistical formulas and report content were unchanged. No installer was rebuilt.
