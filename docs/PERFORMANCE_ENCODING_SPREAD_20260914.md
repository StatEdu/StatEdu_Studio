# Screen dispersed values before full duplicate search — 2026-09-14

Applied to `repair_text_encoding()` in `R/data_io.R`. After the existing 128-element prefix check passes, an additional 128 positions spaced from first to last element are checked. More than 64 distinct sampled values bypasses the full duplicate search and uses the original full-vector encoding checks. This avoids building byte keys and scanning all distinct values for many high-cardinality columns with repetitive prefixes.

The 64-value cutoff matches the existing full-vector reuse limit. The additional sample only chooses an optimization path: it never determines whether the original text is valid, never skips text repair, and consumes no RNG. If the sample misses high cardinality, the existing full duplicate-count check still rejects reuse. Uniform encoding marks and byte-exact duplicate matching remain required. The existing size/prefix gates still short-circuit before the new check.

## Full-import timings

Bundled Windows R 4.5.3, three fresh processes per version, sequential with reversed version order in set two. readr loaded before timing; six fixtures run in fixed order per process. Median seconds for `read_csv_robust()`; OS caches not cleared. R startup, application UI and subsequent analysis are excluded.

| Ten-column CSV fixture | Before | After |
| --- | ---: | ---: |
| UTF-8, 20,000 rows | 0.60 | 0.61 |
| CP949, 20,000 rows | 0.50 | 0.50 |
| UTF-8 repetitive, 150,000 rows | 0.33 | 0.33 |
| UTF-8 half missing, 150,000 rows | 0.22 | 0.22 |
| UTF-8 high cardinality, 150,000 rows | 0.62 | 0.63 |
| High cardinality with first 128 rows repeated | 0.77 | 0.61 |

The targeted repetitive-prefix fixture improved approximately **21%**, with all three pairs faster (0.77/0.77/0.73 to 0.60/0.63/0.61 seconds). Other medians were equal or 0.01 seconds slower, so no universal improvement is claimed. The dispersed sample remains a heuristic: it can miss unusual arrangements and leave the original duplicate-search cost in place.

## Memory

A separate three-process-per-version test imported the same repetitive-prefix/high-cardinality file. PowerShell sampled the actual R PID's `PeakWorkingSet64` approximately every 20 ms. Median peak working set fell from **192,352,256 to 174,866,432 bytes**, a reduction of **17,485,824 bytes (17.49 MB, 9.1%)**. Every candidate peak was lower than its paired baseline. This includes reader initialization and saved-result serialization, not just helper allocations; it is not an Electron or whole-session measurement. These runs use a different fixture from the previous memory report and must not be added to its savings.

## Preservation checks

- Extended persistent `scripts/validate_encoding_distinct.R` passed **560** exact repair/data comparisons against the independent pre-reuse algorithm, including values, direct character encoding marks, warnings/messages and RNG. New cases deliberately hide high cardinality, malformed bytes and CP949 text outside both prefix and dispersed sample positions, with distinct-count boundaries at 16/17/64/65/256.
- All **18** full-import before/after snapshot pairs matched exactly in data, attributes, parser problems, warnings/messages and RNG. readr external problem pointers were normalized to actual problem tables.
- All three memory-result pairs matched exactly.
- Existing `scripts/validate_data_io.R` passed CSV, Excel, Stata, SAS and DAT checks. Existing haven `write_sas()` deprecation warning remains.
- Production source matches the tested candidate snapshot; `git diff --check` passed for edited runtime/test files.

Artifacts: `output/encoding-spread-20260914/` contains source snapshots, scripts, timings, memory readings and comparison results. No statistical formulas or analysis/export content changed. No installer was rebuilt.
