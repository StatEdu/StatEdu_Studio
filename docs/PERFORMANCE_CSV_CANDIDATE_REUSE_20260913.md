# Reuse scores for identical decoded CSV text — 2026-09-13

Candidate encodings can decode the same bytes into identical UTF-8 strings. `csv_encoding_candidates_from_bytes()` now retains only the last scored string and its score within the current invocation. If a subsequent successful conversion is identical, its score is reused. Conversion attempts, candidate order, invalid-conversion handling, score formulas and ranking rules remain unchanged. No state is shared between files/calls.

## Validation

- `scripts/validate_csv_candidate_search.R`: 125 exact intermediate-score/rank/condition/RNG scenarios passed against both the actual previous implementation and an independent regex/no-reuse reference. The reference transformer was adjusted to preserve literal NULL arguments when reconstructing R expressions.
- `scripts/validate_csv_score_sampling.R` against the actual baseline: 64 score/import/condition/RNG comparisons and sample checks passed.
- Four complete Korean CSV imports matched in data, remaining attributes, actual readr parser problems, conditions and RNG; the ASCII fixture's complete imported table also matched exactly.
- `scripts/validate_data_io.R`: all existing checks passed, with the existing haven `write_sas()` deprecation warning.
- `git diff --check` passed.

## Full import timing

Bundled R 4.5.3; five alternating before/after runs per fixture; median complete `read_csv_robust()` seconds; caches/namespaces reused.

| Fixture | Before | After |
| --- | ---: | ---: |
| ASCII, 20,000 rows, ten columns | 0.76 | 0.49 |
| Korean UTF-8, 20,000 rows | 0.55 | 0.56 |
| Korean UTF-8, 150,000 rows | 0.38 | 0.37 |
| UTF-8 half missing, 150,000 rows | 0.27 | 0.26 |
| Korean CP949, 20,000 rows | 0.58 | 0.50 |

The ASCII case improved about 36% and CP949 about 14%. UTF-8 changes were small and include a slight increase; no universal speedup is claimed. Savings depend on different encodings producing identical strings. Keeping the previous converted string temporarily can increase peak memory while the next candidate is evaluated; this is bounded to the current invocation and was not measured separately.

Artifacts and baseline: `output/csv-candidate-reuse-20260913/`. No statistical formulas or report content changed. No installer was rebuilt.
