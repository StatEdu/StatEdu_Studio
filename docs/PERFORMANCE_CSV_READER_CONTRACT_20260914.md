# CSV reader substitution contract review — 2026-09-14

**Not applied.** Forcing small CSV files through the existing large-file/readr path changes data and metadata. The performance discontinuity at 10 MiB does not justify moving that boundary under the user's exact-result preservation requirement.

The isolated candidate changes only the initial `use_base_reader` assignment to `FALSE`, thereby using the existing large-file encoding-ranking and readr parsing path for all test files. Production `R/data_io.R` remains unchanged. This experiment tests substitution of the complete existing path, not a tuned readr compatibility implementation or an isolated parser microbenchmark.

## Method and results

Bundled Windows R 4.5.3. Fourteen small UTF-8 fixtures, each with header handling enabled and disabled, yielded 28 comparisons. Fixtures include integers, leading zeros, dates, logicals, whitespace, missing values, duplicate/empty names, excess/missing fields, invalid quotes, quoted newlines, decimals and Korean text. Both implementations use the same input file per comparison. Captured results include values, attributes, actual readr problem tables, warnings/messages and RNG; readr external pointers are normalized to actual problem tables. Both results also pass through their unchanged `prepare_data()` functions for a second exact comparison.

- Exact full-import snapshots matched in **0/28** cases. This includes ordinary dataframe/tibble, parser-spec and integer/double differences; it does not mean every cell changed.
- After converting each column to character and ignoring column names, cell lists matched in **18/28** cases. The remaining **10/28** cases changed cell content or layout/error outcome even under this relaxed comparison.
- Exact snapshots after `prepare_data()` matched in **0/28** cases. Metadata and column-type differences can remain, so this is not a count of numerical-analysis differences.
- RNG states matched in all 28 cases. Warning/message collections matched in 26; duplicate and empty header names produced additional name-repair messages with headers enabled. Parser problem tables were recorded separately and are included in full snapshot comparisons.

## Concrete header-enabled examples

| Input | Current small-file path | Forced readr path |
| --- | --- | --- |
| `001`, `003` ID column | Integer `1`, `3` | Character `"001"`, `"003"` |
| `2026-01-01` date text | Character column; becomes factor in `prepare_data()` | `Date` column remains a date |
| `  hello  ` | Spaces retained | Trimmed to `hello` |
| Duplicate headers `x,x` | Both names remain `x` | Names repaired to `x...1`, `x...2` |
| Header `a,b` with row `1,2,3` | First field becomes row name; values `a=2`, `b=3` | In the tested malformed fixture, first row becomes `a=1`, `b=23`, with parser problems recorded |

Neither interpretation of malformed input is selected as a new requirement here. The important result is that substitution does not preserve the existing behavior. Preserving IDs, grouping labels, dates and variable names matters to downstream statistical analysis; no analyses were rerun to quantify those consequences.

## Decision and scope

Keep the existing 10 MiB boundary and the optimizations already applied within each reader path. A reader replacement would need explicit compatibility work for type inference, whitespace, missing values, names, malformed rows, parser diagnostics and encoding ranking, followed by broad equivalence validation. It is not a safe one-line speed optimization.

No additional timing or memory benchmark was run after these contract mismatches were established. No runtime code, statistical formulas, analysis/export content or installer was changed. Production source matches the baseline snapshot.

Artifacts: `output/csv-reader-contract-20260914/` contains baseline/candidate source, 14 input files, `compare.R`, `comparison.csv` and complete `results.rds` snapshots.
