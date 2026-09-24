# Filter merge coverage checks to the current row — 2026-09-14

`result_cell_covered_by_span()` previously visited every merge record for each rendered cell, even when the record belonged to another row. For ordinary data frames with nonmissing numeric row values and an ordinary scalar numeric row index, it now filters candidate records to the current row first. The original per-record start/end-column checks remain intact and retain their order.

Missing or unusual row types use the original path. The no-merge early return is unchanged. No cell value, merge definition, numerical calculation or display/export rule changed.

## Benchmark

Bundled R 4.5.3, loaded preferences/modules, sequential HTML rendering benchmark. Five rounds alternate baseline/current order after equivalent output is verified; each sample measures one complete `coefficient_html_table()` render. Both closures share the same global helper environment. Synthetic tables have six columns; merged cases have a B–D merge on every row.

| Rows | Merges | Baseline median (s) | Current median (s) |
| --- | --- | ---: | ---: |
| 20 | None | 0.05 | 0.03 |
| 200 | None | 0.37 | 0.38 |
| 20 | One per row | 0.04 | 0.04 |
| 200 | One per row | 0.53 | 0.34 |

The 200-row merged table improved about 36% (0.19 seconds). The unmerged early-return code is unchanged, and its mixed timing differences should be treated as measurement variation rather than benefit or regression attributable to this change. Small merged tables showed no improvement. These are scoped synthetic rendering results, not an overall analysis or startup benchmark.

## Verification

- `scripts/validate_span_row_filter.R`: 3,206 exact value/error/condition comparisons cover missing row values, character rows, custom data frames, unknown/reversed endpoints, overlap, out-of-range rows, and unusual row-index shapes/types.
- Twelve Korean/English merged/unmerged table cases preserve complete HTML (including colspan/content/styles), saved HTML, accumulated table extraction, Excel sheet names and cell values exactly.
- `validate_screen_table_export.R` passes both REML variants: 36 screen/report tables retain their content and orientations.
- A fresh full mock session uploads/selects the fixture and runs Pearson/KM via actual handlers. Complete result objects, live HTML/dependencies, notifications and warnings exactly match the preceding version (`identical(..., num.eq=FALSE)`). No captured warnings occurred; the existing ggplot informational message remains.
- `git diff --check` passes for the relevant application/test files.

Displayed/export content is unchanged. PDF/Word/HWPX binaries were not regenerated; HTML and accumulated document inputs were verified. No installer was rebuilt.

## Artifacts and reproduction

Baseline, shared setup, benchmark, Excel artifacts, raw timings and medians: `output/span-row-filter-20260914/`. Full-session snapshot: `output/server-first-analysis-20260913/current-span-row-verified.rds`.

Run validation and benchmark sequentially from the repository root with bundled Rscript `--vanilla`, English UTF-8 Windows locale, bundled `R_LIBS_USER`, `STATEDU_NO_PACKAGE_INSTALL=true` and an isolated module cache.
