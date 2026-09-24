# Reuse factor conversion for identical columns — 2026-09-14

Applied to `prepare_data()` in `R/data_io.R`. Within one preparation call, retain the most recent eligible, quietly converted character vector and its factor. Reuse requires exact vector equality plus identical encoding marks. The original `factor()` still determines codes, levels and their locale-sensitive order; no sorting rule is changed. Haven missing-value/label handling still runs before the character conversion decision.

Eligibility is limited to attribute-free character vectors of at least 1,024 elements with more than 16 distinct values in the first 128 positions. This sample selects a possible reuse path, never the resulting factor levels. Ineligible columns use the original conversion. A nonmatching eligible vector is converted normally and replaces the single retained entry only if no warnings/messages occurred. Errors propagate normally. No cache survives the preparation call.

## Candidate adjustment

The initial candidate lacked the distinct-count gate. It improved repeated high-cardinality columns but slowed repeated low-cardinality input (initial medians 0.02 to 0.04 seconds). Those initial measurements and candidate are retained under `initial/`. The final measurements below were rerun in fresh processes after adding the eligibility gate.

## Final preparation timing

Bundled Windows R 4.5.3, configured UTF-8 locale. Three fresh processes per version, sequential with reversed version order in set two. Four synthetic fixtures have 150,000 rows and ten character columns each. Timings cover `prepare_data()` only; input generation, CSV reading, process startup, UI and subsequent analysis are excluded.

| Input columns | Baseline median seconds | Final median seconds |
| --- | ---: | ---: |
| Same vector in all columns, 150,000 distinct strings | 3.69 | 0.43 |
| Different vectors, 150,000 distinct strings each | 4.06 | 3.93 |
| Same low-cardinality vector in all columns | 0.03 | 0.02 |
| Different low-cardinality vectors | 0.03 | 0.02 |

The targeted identical/high-cardinality case improved approximately **88.3%**, in all three pairs. This is a deliberately favorable repeated-column fixture, not a claim about all datasets or full import/application loading. The different-column path still performs ten factor conversions; its small timing difference is not evidence of a faster sorting algorithm. Low-cardinality timings are near clock resolution and support only that the earlier observed slowdown was not repeated in the final harness.

## Memory

Separate three-process-per-version runs prepared the identical/high-cardinality fixture. The actual R PID's `PeakWorkingSet64` was observed approximately every 20 ms. Median peak working set fell from **152,178,688 to 137,064,448 bytes**: **15,114,240 bytes (15.11 MB, 9.9%)**. All candidate peaks were lower. This includes input creation and result serialization and excludes Electron; it is not an isolated helper allocation measurement. Distinct-column peak memory and concurrent sessions were not measured. Retaining one input/result pair can have different memory effects when reuse does not occur.

## Verification

- New persistent `scripts/validate_prepare_factor_reuse.R`: **150** preparation/attribute/condition/RNG comparisons plus **48** high-cardinality/eligibility cases. Covers empty/short vectors, thresholds, missing values, names/labels, factors/ordered factors, dates, SPSS missing definitions, invalid bytes, changed final elements, differing attributes and encoding marks.
- Four instrumented quiet/warning/message/error cases confirm one conversion for an eligible quiet duplicate, repeated conversion and identical conditions when warning/message-producing, and unchanged error behavior.
- Editing one reused output factor leaves the other output column and input unchanged in the regression test.
- All **12** final measured preparation result pairs matched exactly in factor codes/levels, dataframe attributes, warnings/messages and RNG. All three memory-result pairs also matched exactly.
- Existing data IO validation passed; existing haven `write_sas()` deprecation warning remains.
- Production matches the final tested candidate; `git diff --check` passed for edited runtime/test files.

No statistical formulas or analysis/export content changed. No installer was rebuilt. Artifacts: `output/prepare-factor-reuse-20260914/` contains source snapshots, initial and final timings, memory readings, comparison scripts and saved results.
