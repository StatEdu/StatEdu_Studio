# Combined recent CSV optimizations — 2026-09-14

This review measures the combined effect of the five recently applied CSV changes: byte-exact repeated-text checks, deferred flag expansion, dispersed eligibility screening, decoded winning-text reuse, and conditional BOM substitution. The baseline is `output/encoding-distinct-20260914/baseline.R`, immediately before these changes; it already contains the older CSV sampling and candidate-scoring optimizations. This is not a comparison against an original release or against every previous optimization.

No additional runtime code was changed in this review. Individual reported gains are not added together.

## Full-import measurements

Bundled Windows R 4.5.3. Three fresh processes per version, sequential; version order reversed in set two. readr loaded before timing, six fixtures read in fixed order, OS caches not cleared. Median elapsed seconds for `read_csv_robust()`; excludes R/source startup, application UI, subsequent `prepare_data()` and statistical analysis.

| Ten-column fixture | Before recent changes | Current | Median reduction |
| --- | ---: | ---: | ---: |
| UTF-8, 20,000 rows | 0.61 | 0.53 | 13.1% |
| CP949, 20,000 rows | 0.51 | 0.40 | 21.6% |
| UTF-8 repetitive, 150,000 rows | 0.46 | 0.33 | 28.3% |
| UTF-8 half missing, 150,000 rows | 0.28 | 0.20 | 28.6% |
| UTF-8 high cardinality, 150,000 rows | 0.64 | 0.61 | 4.7% |
| High cardinality with first 128 rows repeated | 0.64 | 0.61 | 4.7% |

All paired runs improved for the first five fixtures. The repetitive-prefix/high-cardinality fixture improved in two runs and increased from 0.63 to 0.64 seconds in the third. Its small median reduction is not a universal performance guarantee. Repetitive fixture values favor the duplicate-check optimization. No actual Electron launch, cold disk access, XLSX import or downstream analysis timing is established by these measurements.

## Combined memory tradeoff

A separate three-process-per-version run imported the large repetitive UTF-8 fixture. PowerShell observed the actual R PID's `PeakWorkingSet64` approximately every 20 ms. Median peak working set increased from **163,061,760 to 178,683,904 bytes**: **+15,622,144 bytes (15.62 MB, 9.6%)**. All three candidate peaks exceeded their respective baseline peaks.

This is the directly measured combined cost for this fixture, not the sum of earlier reports. Temporary byte keys and lookup vectors still trade memory for fewer encoding conversions despite later allocation reductions. Peaks include reader initialization and result serialization, not only helper allocations or Electron/session memory. The memory result should not be extrapolated to all files or concurrent sessions.

## Combined preservation checks

- All **18** full-import baseline/current snapshot pairs matched exactly for data, attributes, actual readr problem tables, warnings/messages and RNG. External parser pointers were normalized to actual problem tables.
- All **three** memory-result pairs matched exactly.
- Candidate-search validation against the actual combined baseline: **125** byte scenarios with identical intermediate scores, ranks, conditions and RNG.
- Score-sampling/import validation against the actual combined baseline: **64** checks passed.
- BOM/import validation against the actual combined baseline: **44** checks passed.
- Repeated-text validation against the independent original repair algorithm: **560** exact string/data/encoding/condition/RNG checks passed.
- Source-difference review confirms the snapshots differ only in the three intended functions: `repair_text_encoding()`, `csv_encoding_candidates_from_bytes()` and `read_csv_robust()`.
- Production source matches the measured current snapshot, SHA-256 `2692758B927C18B7834862AEC6DC8BBBAD0DAC8B73EAA7E4DB0731B654E8F860`.

The 10 MiB reader boundary, statistical formulas and analysis/export content remain unchanged. No new optimization or installer build was made in this turn. The parser-substitution and replacement-character-codepoint candidates remain rejected.

Artifacts: `output/csv-combined-20260914/` contains both source snapshots, fixtures, timing and memory scripts/results, and complete saved comparisons. Prior persistent validation scripts were invoked against this baseline for the combined checks above.
