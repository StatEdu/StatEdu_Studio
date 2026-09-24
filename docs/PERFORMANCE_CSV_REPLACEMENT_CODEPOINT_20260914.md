# Replacement-character codepoint reuse review — 2026-09-14

**Not applied.** The isolated candidate replaces the scalar replacement-character test `sum(grepl("\uFFFD", converted, fixed=TRUE))` with `as.integer(any(codepoints == 0xFFFD))`, using the integer vector already calculated for Unicode range counts. The result remains a presence flag, not the number of replacement characters. All other encoding scoring and parsing logic is unchanged.

The candidate removes a string scan, but creates an integer-vector comparison and logical reduction. Measurements did not show a convincing overall benefit for the main small-file cases, so production code is retained.

## Validation

- 125 byte scenarios passed exact intermediate-score, ranking, condition and RNG comparisons against the actual pre-change snapshot. Coverage includes supported encodings, replacement characters, malformed/random bytes, BOM/NUL, Unicode range boundaries and all nonzero nonsurrogate BMP codepoints.
- All 18 measured full-import result pairs matched exactly in data, attributes, actual readr problem tables, warnings/messages and RNG. External parser pointers were normalized to actual problem tables.
- Production `R/data_io.R` matches the baseline snapshot. This is evidence for covered inputs, not a general proof for every locale or invalid text sequence.

## Full import timing

Bundled Windows R 4.5.3; three fresh processes per version, sequential with reversed version order in set two. readr namespace loaded before timing. Six fixtures read in fixed order; OS caches not cleared. Median elapsed seconds for `read_csv_robust()`, excluding R/source startup, UI and subsequent analysis.

| Ten-column fixture | Current production | Candidate |
| --- | ---: | ---: |
| UTF-8, 20,000 rows | 0.55 | 0.55 |
| CP949, 20,000 rows | 0.44 | 0.44 |
| UTF-8 repetitive, 150,000 rows | 0.32 | 0.33 |
| UTF-8 half missing, 150,000 rows | 0.21 | 0.22 |
| UTF-8 high cardinality, 150,000 rows | 0.64 | 0.61 |
| High cardinality with repetitive prefix | 0.62 | 0.61 |

The small UTF-8 candidate runs were 0.55/0.55/0.58 versus 0.55/0.55/0.55 seconds. Mixed large-file differences are small and do not establish a general speedup. No further memory benchmark or production validation was run after deciding against adoption.

Artifacts: `output/csv-replacement-codepoint-20260914/` contains baseline/candidate snapshots, validation and timing scripts, all timing records and saved comparisons. No runtime code, analysis/export content or installer changed.
