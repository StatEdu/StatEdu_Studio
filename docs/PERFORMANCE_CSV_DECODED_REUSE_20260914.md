# Reuse the winning decoded CSV text — 2026-09-14

Applied in `R/data_io.R`. For the existing small-file base-reader path (at most 10 MiB), encoding ranking optionally retains the highest-scoring converted string in a fresh per-import environment. Parsing reuses that string when its encoding matches the candidate being tried. Other encoding attempts still convert the original bytes normally. The cache retains one winner, preserves the existing first-candidate tie rule, and does not persist across imports. BOM/empty/failed conversion paths continue without a cached string. Large files continue to use the existing readr path without a decoded cache.

All encoding candidates, score formulas, ranking order, parser arguments, result scoring and normalization remain unchanged. This removes a repeated conversion of the selected small-file text; it does not shorten encoding coverage or skip parsing fallback.

## Full import timings

Bundled Windows R 4.5.3, three fresh processes per version, sequential, reversed version order in set two. readr loaded before timing; six fixtures read in fixed order per process. Median elapsed seconds for `read_csv_robust()`, excluding source/R startup, application UI and subsequent analysis. OS caches were not cleared.

| Ten-column CSV fixture | Before | After |
| --- | ---: | ---: |
| UTF-8, 20,000 rows, 2.6 MB | 0.61 | 0.54 |
| CP949, 20,000 rows, 1.8 MB | 0.49 | 0.44 |
| UTF-8 repetitive, 150,000 rows | 0.33 | 0.35 |
| UTF-8 half missing, 150,000 rows | 0.20 | 0.22 |
| UTF-8 high cardinality, 150,000 rows | 0.64 | 0.61 |
| High cardinality with repetitive prefix | 0.62 | 0.63 |

Both targeted small fixtures were faster in every paired run, with median reductions of approximately **11.5% and 10.2%**. Large-file results varied by -0.03 to +0.02 seconds and do not establish a speedup there. The added optional cache test runs during scoring, but large-file parsing has no cached-text path. No universal improvement is claimed.

## Memory

A separate three-process-per-version test imported the small UTF-8 fixture. The actual R process was identified by `Sys.getpid()` and monitored for `PeakWorkingSet64` approximately every 20 ms. Median peak working set was **176,480,256 before and 173,875,200 bytes after**, a decrease of **2,605,056 bytes (2.61 MB, 1.5%)**. All three candidate peaks were lower. Measurement includes reader work and result serialization; it is not an isolated cache-allocation measure or Electron/session memory. Retaining a decoded string can still have a different memory tradeoff for other files or near the 10 MiB limit; those cases were not separately memory-benchmarked.

## Verification

- Existing candidate-search validation against the actual pre-change snapshot: **125** byte scenarios with exact intermediate scores, ranks, conditions and RNG.
- Existing score-sampling/import validation against the actual snapshot: **64** exact score/import/condition/RNG checks.
- New persistent `scripts/validate_csv_decoded_reuse.R`: **82** rank/cache scenarios and **164** complete import/error/condition/RNG comparisons with caching disabled as the reference. Covers supported encodings, ASCII ties, empty/BOM/NUL/malformed bytes, replacement characters, quoted newlines, unclosed quotes, uneven rows, headers on/off and randomized bytes. Every retained winner matches the first-ranked encoding and a fresh conversion of the original bytes.
- All **18** measured full-import snapshot pairs matched exactly in values, attributes, readr problem tables, warnings/messages and RNG. readr external pointers were normalized to their problem tables.
- All three memory-result pairs matched exactly.
- Existing data IO validation passed CSV, Excel, Stata, SAS and DAT checks; the existing haven `write_sas()` deprecation warning remains.
- Production source matches the tested candidate; `git diff --check` passed for edited runtime/test files.

Artifacts: `output/csv-decoded-reuse-20260914/` includes source snapshots, timing/memory scripts and results, saved comparisons and candidate validation copies. No statistical formulas or analysis/export content changed. No installer was rebuilt.
