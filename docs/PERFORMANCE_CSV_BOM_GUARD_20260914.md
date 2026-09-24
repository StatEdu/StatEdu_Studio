# Skip BOM substitution when absent — 2026-09-14

Applied in `read_csv_robust()` in `R/data_io.R`. After successful UTF-8 conversion on the small-file base-reader path, `startsWith(converted, "\ufeff")` now guards the existing leading-BOM substitution. Files without a leading BOM avoid the substitution call. When present, the original `sub()` removes exactly one leading BOM. Interior BOMs and whitespace before a BOM retain the original treatment. Encoding selection, parser arguments, the 10 MiB boundary and the readr path are unchanged.

## Full import timings

Bundled Windows R 4.5.3; three fresh processes per version, sequential with reversed order in set two. readr loaded before timing, six fixtures read in fixed order, OS caches not cleared. Median elapsed seconds for `read_csv_robust()`, excluding R/source startup, application UI and subsequent analysis.

| Ten-column CSV fixture | Before | After |
| --- | ---: | ---: |
| UTF-8, 20,000 rows | 0.56 | 0.51 |
| CP949, 20,000 rows | 0.42 | 0.39 |
| UTF-8 repetitive, 150,000 rows | 0.34 | 0.33 |
| UTF-8 half missing, 150,000 rows | 0.20 | 0.20 |
| UTF-8 high cardinality, 150,000 rows | 0.62 | 0.63 |
| High cardinality with repetitive prefix | 0.62 | 0.62 |

The targeted small fixtures contain no BOM and improved in all three pairs, with median reductions of approximately **8.9% and 7.1%**. The large-file path is unchanged; its differences of at most 0.01 seconds in median are not attributed to this change. BOM-present files were checked for correctness but not separately benchmarked for speed. No universal speedup is claimed.

## Memory

Separate three-process-per-version imports of the small UTF-8 fixture monitored the actual R PID's `PeakWorkingSet64` approximately every 20 ms. Median peaks were **173,899,776 before and 173,441,024 bytes after**, a difference of **458,752 bytes (0.46 MB)**. Treat this small difference as essentially unchanged memory, not a meaningful memory optimization. Measurement includes reader work and snapshot serialization; it is not Electron/session memory.

## Verification

- New persistent `scripts/validate_csv_bom_guard.R`: **44** complete import/error/condition/RNG comparisons, including no BOM, one/two leading BOMs, BOM-only input, interior BOM, leading whitespace, quoted multiline fields, malformed quotes, replacement characters, supported encodings and headers on/off. Passed against the actual pre-change snapshot and again after production integration against the unconditional-substitution reference.
- Existing score-sampling/import checks passed **64** comparisons against the actual baseline.
- Existing decoded-reuse validation passed **82** ranking/cache scenarios and **164** full import/error/condition/RNG comparisons after integration.
- All **18** measured full-import result pairs matched exactly for values, attributes, parser problem tables, warnings/messages and RNG. readr external pointers were normalized to actual problem tables.
- All three memory-result pairs matched exactly.
- Existing data IO validation passed CSV, Excel, Stata, SAS and DAT checks. The existing haven `write_sas()` deprecation warning remains.
- Production source matches the measured candidate snapshot; `git diff --check` passed for runtime/test edits.

Artifacts: `output/csv-bom-guard-20260914/` contains baseline/candidate source, timing and memory scripts/results, saved comparisons and candidate validation. No statistical formulas or analysis/export content changed. No installer was rebuilt.
