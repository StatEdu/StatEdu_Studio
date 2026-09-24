# Combined result-table rendering audit — 2026-09-14

This review measures five recent optimizations together. It adds reproducible audit artifacts and does not change application code. Individual percentage improvements must not be added together.

The combined baseline restores the pre-optimization versions of `result_cell_style_extra`, `result_cell_covered_by_span`, `result_cell_span_start`, `result_cell_note_marker` and `correlation_model_overview_matrix_display_table`. All five current/baseline closures are rebound to the same global helper environment and swapped together for both timing and export verification. All other helpers, including earlier survival optimizations and numerical routines, are current in both variants.

## Measurements

Bundled R 4.5.3 with initialized modules/preferences. Three sequential rounds alternate which variant runs first. Every timed call includes rendering the complete selected table/panel into HTML, after both paths pass exact equivalence checks. Export work runs afterward and does not interfere with timing.

| Workload | Baseline median (s) | Combined current median (s) |
| --- | ---: | ---: |
| Ordinary 200-row, six-column table | 0.38 | 0.37 |
| 200-row table with merges, markers and styles | 0.64 | 0.39 |
| Complete 30-variable correlation result HTML | 1.42 | 1.36 |
| Complete 50-variable correlation result HTML | 4.62 | 4.38 |
| Complete fixture KM result panel HTML | 0.10 | 0.10 |

The annotated table improved about 39%; the 30- and 50-variable correlation screens improved about 4% and 5%, respectively. The 0.01-second ordinary-table difference is too small to establish a meaningful benefit; KM is unchanged. These measurements describe warm server-side HTML creation, not statistical fitting, graph drawing, Excel saving, browser rendering or desktop startup.

The annotated synthetic table merges B–D on every row and places a marker and style on E. Correlation datasets have 150 rows and use Pearson with p/CI output. KM uses the previously verified fixture result. This benchmark scope is distinct from each earlier microbenchmark and from earlier survival-only changes.

## Correctness

- All five workloads produce exactly identical rendered HTML/dependency objects, warning/message sequences and RNG states under the two complete helper sets (`identical(..., num.eq=FALSE)`).
- The annotated table, 30-variable correlation result and KM result also have exactly matching saved HTML, accumulated-result table extraction, Excel sheet names and cell contents, with all five baseline/current helpers kept active throughout each path.
- The existing ggplot coordinate-replacement informational message appears during both KM export paths.
- `git diff --check` passes for the new audit script.

No runtime/output change was made in this audit. PDF/Word/HWPX binaries were not regenerated. No installer was rebuilt.

## Reproduction

Run `scripts/audit_table_render_combined.R` from the repository root with bundled Rscript `--vanilla`, English UTF-8 Windows locale, bundled `R_LIBS_USER`, `STATEDU_NO_PACKAGE_INSTALL=true`, and an isolated module cache. Keep it sequential; saved-HTML/Excel verification can take longer than rendering benchmarks.

Shared setup, raw timings, medians and export artifacts: `output/table-render-combined-20260914/`. Baseline snapshots are read from `output/cell-style-lookup-20260914/baseline.R` and `output/correlation-overview-rows-20260914/baseline.R`.
