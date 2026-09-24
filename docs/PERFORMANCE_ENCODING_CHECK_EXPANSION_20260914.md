# Defer repeated-text flag expansion — 2026-09-14

Applied a small follow-up to `repair_text_encoding()` in `R/data_io.R`: missing values are excluded from the compact checked vector first, and an entirely valid vector returns immediately. Broken-text flags are expanded to original row positions only when repairs are necessary. The previous version expanded flags and allocated a full-size missing-value mask even when every distinct string was valid. Encoding selection, original string values/marks, duplicate eligibility and the repair loop are unchanged.

## Measurements

Bundled Windows R 4.5.3, three fresh processes per version, sequential with reversed order in set two. readr loaded before timing; each process reads six fixtures in fixed order. Median elapsed seconds for full `read_csv_robust()` calls, excluding process startup, UI and subsequent analysis. OS caches were not cleared.

| Ten-column CSV | Before | After |
| --- | ---: | ---: |
| UTF-8, 20,000 rows | 0.61 | 0.59 |
| CP949, 20,000 rows | 0.49 | 0.50 |
| UTF-8 repetitive, 150,000 rows | 0.35 | 0.32 |
| UTF-8 half missing, 150,000 rows | 0.22 | 0.20 |
| UTF-8 high cardinality, 150,000 rows | 0.63 | 0.62 |
| Same high-cardinality fixture with first 128 rows repeated | 0.75 | 0.75 |

Timing differences are small and include a 0.01-second CP949 increase; no universal speedup is claimed. The late-high-cardinality case remains slower than the ordinary high-cardinality fixture in this harness. Its repetitive prefix still triggers a full duplicate search before falling back. This change does not address that eligibility cost; sampling elsewhere in the vector is a possible separate experiment, not an implemented feature.

A separate three-process-per-version memory test imported the 150,000-row repetitive UTF-8 fixture. The actual R PID reported by `Sys.getpid()` was monitored for `PeakWorkingSet64` approximately every 20 ms. Median peak working set fell from **182,247,424 to 177,131,520 bytes**, a reduction of **5,115,904 bytes (5.12 MB, 2.8%)**. All three candidate peaks were lower than their baseline pairs. This includes reader initialization and result serialization; it is not an isolated helper allocation measure or Electron/session memory. These absolute values should not be directly compared with earlier runs to estimate the combined optimization.

## Verification

- Persistent `scripts/validate_encoding_distinct.R`: all 516 comparisons against the pre-reuse independent algorithm passed for repaired strings/data, explicit character encoding marks, conditions and RNG, including malformed text, missing values, names and eligibility boundaries.
- All **18** full-import before/after snapshot pairs matched exactly in values, attributes, parser problem tables, warnings/messages and RNG. External readr pointers were normalized to actual problem tables.
- All three memory-result pairs matched exactly.
- Existing `scripts/validate_data_io.R` passed all checks. The existing haven `write_sas()` deprecation warning remains.
- Production `R/data_io.R` matches the tested candidate snapshot; `git diff --check` passed.

Artifacts: `output/encoding-check-expansion-20260914/`, including baseline/current snapshots, scripts, six-fixture timings, memory readings and saved comparisons. No analysis/export content changed and no installer was rebuilt.
