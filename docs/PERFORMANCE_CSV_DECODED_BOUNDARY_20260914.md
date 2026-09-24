# Decoded CSV reuse at the reader boundary — 2026-09-14

Follow-up validation of the applied decoded-text reuse change. No runtime code was changed in this review. The baseline is the snapshot immediately before decoded-text reuse, including the previously applied repeated-text checks; the current snapshot is production `R/data_io.R`.

## Fixtures and method

Generated ten-column CSV files containing repeated Korean text. UTF-8 fixtures are exactly 10 MiB minus one byte, 10 MiB, and 10 MiB plus one byte; a CP949 fixture is exactly 10 MiB. Padding is appended to the first field of the final record without changing the column count. The UTF-8 files have 80,659 data rows and the CP949 file 116,508. These are distinct files with slightly different final-field contents; comparisons are before/after for each same file, not equality between different files.

Bundled Windows R 4.5.3, three fresh processes per version, sequential with reversed version order in set two. readr loaded before timing, four fixtures read in fixed order. Median elapsed seconds for `read_csv_robust()` exclude R/source startup, application UI and subsequent analysis. OS caches were not cleared.

| Fixture | Bytes | Before reuse | Current |
| --- | ---: | ---: | ---: |
| UTF-8 below boundary | 10,485,759 | 2.35 | 2.14 |
| UTF-8 at boundary | 10,485,760 | 2.17 | 1.95 |
| UTF-8 above boundary | 10,485,761 | 0.27 | 0.27 |
| CP949 at boundary | 10,485,760 | 2.91 | 2.63 |

All three paired runs were faster for each file using the small-file path. Median improvement at exactly 10 MiB was approximately 10.1% for UTF-8 and 9.6% for CP949. Above the boundary, the unchanged large-file path showed no median change.

The large discontinuity across the boundary already exists in the reader design: files at or below 10 MiB use whole-file encoding ranking and the base reader, while larger files use sampled ranking and readr. This observation does not establish that switching the smaller files to the other path would preserve parsing, attributes, diagnostics or encoding choice. The boundary remains unchanged. Fixed fixture order also means that the first below-boundary measurement is not interchangeable with subsequent at-boundary timing.

## Memory

A separate three-process-per-version run imported the exact-boundary CP949 file, whose UTF-8 decoded text expands relative to its original bytes. PowerShell monitored the actual R PID's `PeakWorkingSet64` approximately every 20 ms. Median peak working set was **360,296,448 before and 360,218,624 bytes after**, a difference of **77,824 bytes (0.08 MB)**. Treat this as essentially unchanged, not meaningful memory savings. The measured peak includes reader work and snapshot serialization and is not whole-application/Electron memory. Other text distributions and invalid-input paths may have different peaks.

## Verification and scope

- All **12** full-import before/after snapshot pairs matched exactly for values, attributes, actual readr problem tables, warnings/messages and RNG. External parser pointers were normalized to their actual problem tables.
- All **three** memory-result pairs matched exactly.
- Production source and the measured current snapshot have identical SHA-256: `C0315BE23752F8DE30690911875B50F8E3D5B3DCE7AE23258F3E80A04E1E2B07`.
- No new implementation, statistical formulas, analysis/export output or installer changes were made.

Artifacts: `output/csv-decoded-boundary-20260914/` contains fixture generation, source snapshots, timing and memory scripts, saved comparisons and results. The existing optimization remains in place. A future review of the reader boundary must explicitly compare both parsers and encoding-ranking coverage; this report alone does not authorize treating those outputs as interchangeable.
