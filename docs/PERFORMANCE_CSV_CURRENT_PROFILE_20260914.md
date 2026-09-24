# Current CSV import profile — 2026-09-14

This review measures the current implementation after the previous candidate-character counting, decoded-score reuse and sampling improvements. No runtime code was changed. The next candidate should target repeated encoding conversion/checking rather than the already optimized character-range counts.

## Method and timings

Bundled Windows R 4.5.3; three fresh R processes, sequential. Each process imported four existing fixtures in the same order. Each fixture had one initial and three repeated full imports; the table shows medians across processes (the repeated column uses each process's median). These are `read_csv_robust()` elapsed times in seconds, excluding R/package startup, application UI and subsequent `prepare_data()`. OS file caches were not cleared. The first large UTF-8 import also encounters the first readr use in each process, so its initial time is not a comparable cold-start measurement for every fixture.

| Fixture, ten columns | Bytes | Initial import | Repeated import | Ranking | Parsing | Text normalization |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| UTF-8, 20,000 rows | 2,600,031 | 0.61 | 0.55 | 0.42 | 0.09 | 0.02 |
| CP949, 20,000 rows | 1,800,031 | 0.52 | 0.50 | 0.36 | 0.11 | 0.03 |
| UTF-8, 150,000 rows | 19,500,031 | 0.68 | 0.39 | 0.12 | 0.08 | 0.19 |
| UTF-8, 150,000 rows, half missing | 12,000,031 | 0.28 | 0.28 | 0.14 | 0.06 | 0.09 |

Stages were timed separately with the first ranked encoding; they are diagnostics, not an additive decomposition of the full call. Scoring rounded to 0.00 seconds at this clock resolution, which does not mean no cost. Small files retain the base reader and whole-file ranking; files over 10 MiB retain readr and sampled ranking. Fixture text is highly repetitive and does not represent all real datasets.

## Profiling findings

A separate 10 ms Rprof run repeated each fixture five times. `iconv()` self-time was approximately 76% and 79% of sampled time for the small UTF-8 and CP949 files, respectively. It was approximately 59% and 64% for the two large fixtures. Text normalization represented about 44% of sampled total time for the large UTF-8 file and 35% for the half-missing file. These percentages are sampled CPU attribution, not wall-clock time savings or mutually exclusive totals to add together.

The initial 1 ms profiler outputs in `profile.R` were unreliable on this runtime (empty output in some cases and implausible sampling durations in others). They are excluded from conclusions. `verify.R` reran profiling at 10 ms and asserted nonempty sampling; only `*-10ms-*.csv` profiles are used above. Full-call timing was outside profiling.

## Preservation checks and next scope

All 36 repeated imports matched their corresponding initial results exactly, including data values, attributes, actual readr problem tables, captured warnings/messages and RNG state. All four initial snapshots also matched across the three processes. readr external problem pointers were replaced with their actual problem tables for comparison. This establishes repeatability of these fixtures, not equivalence of an unimplemented optimization.

The source snapshot and production `R/data_io.R` have identical SHA-256 `91CE7BC95A1BB19176B33BF051033415969712BB4DEAA726CF3D8B647954B0FB`.

The next bounded experiment is reducing repeated checks of identical character values within text normalization while preserving encoding marks, names, missing values and invalid-byte handling. Unique-value conversion must first prove equivalence for mixed encodings and malformed strings; high-cardinality input and peak memory must also be measured. Merely skipping normalization because a parser was given UTF-8 is not justified by this profile. No candidate has been applied in this review, no analysis/export output changed, and no installer was rebuilt.

Artifacts: `output/csv-current-profile-20260914/` (`current.R`, `profile.R`, `verify.R`, per-process timings/snapshots and 10 ms profiles).
