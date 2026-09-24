# Reuse encoding checks for repeated text — 2026-09-14

Applied to `repair_text_encoding()` in `R/data_io.R`. Long, repetitive character vectors now check each byte-distinct value once and expand the broken-text flags back to their original positions. The repair loop, encoding priority, missing-value handling and returned original strings remain unchanged. No cache survives the call.

Eligibility requires at least 1,024 elements, no more than 16 distinct values in the first 128 positions, uniform encoding marks across the full vector, and at most 64 byte-distinct values overall. A temporary bytes-marked copy supplies exact byte keys; actual checks use the original strings with their original encoding marks. Mixed marks and ineligible inputs use the original full-vector checks. The sample only selects an optimization path; it never determines whether unsampled text is valid.

## Full import measurements

Bundled Windows R 4.5.3. Three fresh processes per version, sequential, with version order reversed in the second set. readr namespace loaded before timing. Each process reads the five fixtures in table order once. Median elapsed seconds for complete `read_csv_robust()` calls; OS caches were not cleared. Source loading, R startup, application UI and later analysis are excluded.

| Ten-column CSV fixture | Before | After |
| --- | ---: | ---: |
| UTF-8, 20,000 rows | 0.62 | 0.59 |
| CP949, 20,000 rows | 0.51 | 0.49 |
| UTF-8, 150,000 rows | 0.45 | 0.32 |
| UTF-8, 150,000 rows, half missing | 0.30 | 0.22 |
| UTF-8, 150,000 distinct values per column | 0.63 | 0.62 |

The large repetitive fixtures improved approximately 29% and 27%; every corresponding pair was faster. Small-file differences are modest. The distinct-value fixture showed no observed regression, but this does not establish universal performance, especially where a repetitive prefix is followed by high-cardinality text. The four repetitive fixtures are reused from the earlier CSV review; their highly repetitive values favor this optimization.

## Memory tradeoff

A separate set of three fresh processes per version imported the 150,000-row repetitive UTF-8 file. PowerShell observed the actual R process identified by `Sys.getpid()`, reading `PeakWorkingSet64` approximately every 20 ms. Median peak working set increased from 164,003,840 to 181,010,432 bytes: **+17,006,592 bytes (about 17.0 MB, 10.4%)**. This includes reader/package initialization and snapshot serialization, not just the helper; it is not Electron or whole-session memory. Temporary byte keys and index vectors trade memory for lower conversion cost. All three saved memory-run results matched exactly.

## Validation

- New persistent `scripts/validate_encoding_distinct.R`: 516 exact repair/normalized-data comparisons against an independent copy of the previous algorithm, including explicit encoding marks for direct character results, warnings, messages and RNG. Cases cover malformed bytes, CP949, UTF-8/Latin-1/bytes/unknown/mixed marks, missing values, names, factors/dates, size thresholds, distinct-count boundaries and randomized inputs.
- All 15 before/after full-import snapshot pairs matched exactly in values, attributes, readr parser problem tables, warnings/messages and RNG. External readr problem pointers are compared through their actual problem tables.
- Existing pending-index checks passed 53 repair/data scenarios; missing-value checks passed 30 comparisons, both against the actual pre-change snapshot.
- Existing `scripts/validate_data_io.R` passed all checks, including CSV, Excel, Stata, SAS and DAT. The existing haven `write_sas()` deprecation warning remains.
- `git diff --check` passed for the edited runtime file. The benchmark candidate and production helper have identical parsed bodies; production adds one explanatory comment.

No statistical formulas or analysis/export content changed, and no installer was rebuilt. These tests provide evidence for the covered inputs; they are not an exhaustive proof for all possible character vectors or operating-system locales.

Artifacts: `output/encoding-distinct-20260914/` contains the pre-change and candidate source snapshots, direct validation, timing scripts/results, memory scripts/results and full-result comparisons.
