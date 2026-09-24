# Track pending encoding-repair positions — 2026-09-13

`repair_text_encoding()` now computes the positions requiring repair once. Each conversion round updates the repaired values at those positions and retains only unresolved positions. This replaces repeated whole-column logical indexing, duplicate `which()` calls and whole-mask completion checks. Encoding order and the use of original text for each attempted conversion are unchanged.

## Validation

- `scripts/validate_encoding_pending.R`: 53 scenarios compare both repaired vectors and normalized data, including conditions and RNG. Includes single-byte values 1–255, CP949/UTF-8 text, replacement characters, empty/all-missing input and random mixtures. Both the actual baseline and original-loop reference passed.
- `scripts/validate_encoding_missing_skip.R` against the actual baseline: 30 additional exact comparisons passed.
- Million-value benchmark outputs matched exactly.
- `scripts/validate_data_io.R`: all existing checks passed, with the existing haven `write_sas()` deprecation warning.
- `git diff --check` passed.

## Timing

Bundled R 4.5.3; five alternating before/after runs; median seconds for a million-value vector. Corrupt entries contain the same invalid byte sequence; other entries are ASCII text.

| Corrupt entries | Before | After |
| --- | ---: | ---: |
| One | 0.14 | 0.13 |
| 10% | 0.14 | 0.14 |
| All | 0.25 | 0.24 |

The observed absolute improvement is small and near timer resolution. The change simplifies tracking of outstanding repairs and avoids repeated full-vector searches; it does not establish a substantial import speedup. Peak memory was not measured.

Artifacts and baseline: `output/encoding-pending-performance-20260913/`. Statistical formulas and report contents were not changed. No installer was rebuilt.
