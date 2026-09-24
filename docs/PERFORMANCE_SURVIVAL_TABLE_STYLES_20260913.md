# Survival table formatting reuse — 2026-09-13

`survival_simple_table()` now computes the normal/final-row body styles once per column and reuses each column's CSS class calculated by its header. All reuse is local to one table render, so later tables and language changes get fresh formatting. If class formatting emits a warning or message, the body retains its original per-cell calls and diagnostics.

Numerical formatting, cell values, footnote marker lookup, table order, labels, orientation and export contracts are unchanged. This reduces repeated string/regular-expression work without changing statistical calculations.

## Measurements

Bundled R 4.5.3, loaded preferences and modules, one sequential benchmark process. Both baseline and current closures use the same global helper environment; the active table function is swapped for full-panel measurements. Five rounds alternate which variant runs first; each table sample averages three render-to-HTML calls, each KM panel sample five. Medians follow.

| Workload | Baseline (s) | Current (s) |
| --- | ---: | ---: |
| 10 rows × 6 columns | 0.0167 | 0.0167 |
| 100 rows × 6 columns | 0.110 | 0.100 |
| 1,000 rows × 6 columns | 1.440 | 1.340 |
| Full fixture KM result panel | 0.104 | 0.102 |

The measured larger-table improvements are about 9% and 7%. The small-table/full-panel differences do not establish a meaningful overall analysis speedup. These are warm table/panel HTML rendering measurements, not cold startup or first-analysis benchmarks. A fresh full mock session was also run for correctness; its single timing is not used as performance evidence.

## Verification

- `scripts/validate_survival_table_styles.R`: 44 table comparisons cover empty, one-row, multirow, zero-column and duplicate-name cases; Korean/English; main/appendix roles; Unicode headers; escaped strings; missing/nonfinite numbers; and repeated footnote markers. Rendered output, conditions and RNG match exactly.
- A warning-producing class formatter verifies that per-cell diagnostics are preserved.
- Both languages' complete fixture KM screens, saved HTML, report-mode HTML, accumulated-result table extraction, Excel sheet names and cell contents match exactly, using the respective baseline/current formatter for the entire render/export path.
- A full `MockShinySession` uploads the fixture, selects variables, runs Pearson and KM through the actual handlers, and produces complete result objects, live HTML/dependencies and notifications exactly matching the pre-change session (`identical(..., num.eq=FALSE)`). No captured warnings occurred.
- `git diff --check` passed. The existing ggplot coordinate-replacement informational message occurs in both export paths.

Displayed/export content is unchanged. PDF/Word/HWPX binaries were not regenerated; saved/report HTML and accumulated content feeding the document paths are identical. No installer was rebuilt.

## Artifacts and reproduction

Baseline snapshot, benchmark script, raw timings, summary and Excel artifacts: `output/survival-table-style-20260913/`. Full-session snapshot: `output/server-first-analysis-20260913/current-table-style-verified.rds`.

Run the validation and `output/survival-table-style-20260913/benchmark.R` from the repository root with bundled Rscript `--vanilla`, `LC_ALL`/`LANG=English_United States.utf8`, bundled `R_LIBS_USER`, `STATEDU_NO_PACKAGE_INSTALL=true`, and an isolated module-cache directory. Run workloads sequentially to avoid timing interference.
