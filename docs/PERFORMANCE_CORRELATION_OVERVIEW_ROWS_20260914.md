# Build correlation overview style metadata once — 2026-09-14

`correlation_model_overview_matrix_display_table()` previously constructed a one-row data frame for each styled cell and combined the frames with `rbind`. It now collects plain row records and builds the three-column style data frame once. Matrix values, traversal order, row/column references, CSS strings, metadata types and row names remain identical. No correlation formula or numerical formatting changed.

## Timing

Bundled R 4.5.3, initialized modules/preferences, sequential benchmark. Baseline and current helpers are rebound to the same global environment and swapped in the same slot. Five rounds alternate which variant runs first. Each sample measures one call after both paths have been exercised for equivalence. Datasets have 150 rows and the stated number of continuous variables, using Pearson with p/CI output.

| Variables | Workload | Baseline median (s) | Current median (s) |
| --- | --- | ---: | ---: |
| 10 | Overview table and metadata | 0.01 | 0.00 |
| 30 | Overview table and metadata | 0.06 | 0.03 |
| 50 | Overview table and metadata | 0.19 | 0.07 |
| 10 | Full result HTML rendering | 0.19 | 0.19 |
| 30 | Full result HTML rendering | 1.43 | 1.38 |
| 50 | Full result HTML rendering | 4.67 | 4.53 |

For 50 variables, overview construction improved about 63%, while the relevant whole-screen HTML improvement was about 3% (0.14 seconds). The 30-variable full screen improved about 3.5%; the 10-variable screen showed no change. A recorded zero is below timer resolution, not zero execution cost. These are warm server-side display timings; they exclude statistical fitting, browser rendering and desktop startup.

## Verification

- `scripts/validate_correlation_overview_rows.R`: 75 exact table/attribute/condition/RNG comparisons covering empty, square and rectangular matrices, blank/unknown/missing method labels and duplicate column names.
- Complete rendered screens, saved HTML bytes, accumulated-result table extraction, Excel sheet names and cell values match for a ten-variable observed-correlation result and a mixed continuous/ordinal/binary result with the latent-correlation option. The respective baseline/current helper remains active throughout each render/export path.
- A fresh full mock session uploads the existing fixture, selects variables and runs Pearson and KM through the actual handlers. Complete result objects, live HTML/dependencies, notifications and warnings exactly match the preceding version (`identical(..., num.eq=FALSE)`). No captured warnings occurred. The existing ggplot coordinate-replacement informational message is unchanged.
- `git diff --check` passed for changed application/test files.

Displayed/export content is unchanged. PDF/Word/HWPX binaries were not regenerated; saved HTML and accumulated-content inputs are identical. No installer was rebuilt.

## Reproduction

Baseline, shared setup, benchmark, HTML/Excel artifacts and raw/summary timings: `output/correlation-overview-rows-20260914/`. Full-session snapshot: `output/server-first-analysis-20260913/current-overview-rows-verified.rds`.

Run the validation script and the artifact directory's `benchmark.R` sequentially from the repository root with bundled Rscript `--vanilla`, `LC_ALL`/`LANG=English_United States.utf8`, bundled `R_LIBS_USER`, `STATEDU_NO_PACKAGE_INSTALL=true` and an isolated module-cache directory. The full-HTML benchmark is substantially slower than the metadata-only microbenchmark.
