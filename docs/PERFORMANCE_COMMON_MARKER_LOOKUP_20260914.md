# Avoid data-frame copies in shared footnote lookup — 2026-09-14

The shared `result_cell_note_marker()` now locates the first matching marker directly for ordinary marker tables with a complete logical selection and an unclassed marker column. It previously copied all matching records to obtain only the first marker. Duplicate precedence and the no-marker early return are unchanged. Missing selection values, classed marker columns, custom data-frame types and unusual selection shapes use the original subsetting path.

This applies the previously verified survival-specific approach to the shared result-table renderer. The bold-cell helper already uses a vectorized condition and was left unchanged. No numerical calculation, footnote content or formatting rule changed.

## Benchmark

Bundled R 4.5.3, initialized preferences/modules, sequential complete `coefficient_html_table()` rendering to HTML. Five rounds alternate baseline/current order; each sample averages three renders. Both closures are rebound to the same global helper environment and swapped in the same global slot. Six-column synthetic tables have either no markers or one marker per row on column V2.

| Rows | Markers | Baseline median (s) | Current median (s) |
| --- | --- | ---: | ---: |
| 20 | None | 0.0433 | 0.0400 |
| 200 | None | 0.3800 | 0.3800 |
| 20 | One per row | 0.0533 | 0.0500 |
| 200 | One per row | 0.4933 | 0.4433 |

The 200-row marked table improved about 10% (0.05 seconds). Small-table differences are close to timing granularity; the unchanged no-marker path showed no large-table difference. These are scoped server-side table-rendering measurements, not an overall analysis or startup speedup.

## Verification

- `scripts/validate_common_marker_lookup.R`: 6,005 exact marker-value/condition comparisons covering missing values, absent/empty metadata, duplicate first-match precedence, factor markers, custom data frames and missing marker fields.
- English/Korean marked coefficient-table HTML, saved document HTML, accumulated table extraction, Excel sheet names and cells match exactly with the respective helper active throughout each path.
- Complete saved KM HTML and report-mode HTML match in both languages.
- `validate_screen_table_export.R`: two REML variants pass, retaining 36 table contents and their screen/report orientations.
- A fresh full mock session uploads/selects the fixture and runs Pearson/KM through actual handlers. Complete result objects, live HTML/dependencies, notifications and warnings exactly match the preceding version (`identical(..., num.eq=FALSE)`). No captured warnings occurred. The existing ggplot informational message remains unchanged.
- `git diff --check` passes for the relevant application/test files.

Displayed/export content is unchanged. PDF/Word/HWPX binaries were not regenerated; HTML/report and accumulated document inputs were checked. No installer was rebuilt.

## Reproduction

Baseline, benchmark, raw/summary timings and Excel artifacts: `output/common-marker-lookup-20260914/`. Full-session snapshot: `output/server-first-analysis-20260913/current-common-marker-verified.rds`.

Run validation and benchmark sequentially from the repository root using bundled Rscript `--vanilla`, English UTF-8 Windows locale, bundled `R_LIBS_USER`, `STATEDU_NO_PACKAGE_INSTALL=true` and an isolated module cache.
