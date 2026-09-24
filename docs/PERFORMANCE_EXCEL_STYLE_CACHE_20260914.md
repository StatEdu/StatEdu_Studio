# Reuse identical Excel cell styles within each sheet — 2026-09-14

`add_screen_excel_table()` now reuses an `openxlsx` style object when all variable style properties match: header/body font size, bold/italic decoration, horizontal/vertical alignment, first-row borders and dark/light border treatment. Constant properties remain explicit in style creation. The cache is local to a single sheet-writing call, and styles are still applied to cells in their original order with the original stacking behavior.

Row batching from the preceding change remains enabled in both measured variants. Cell values, merge operations, widths/heights, notes and print settings are unchanged.

## Full-save timings

Same saved 30-variable correlation HTML, three Excel sheets, bundled R 4.5.3, fresh processes, modules/preferences loaded, no profiler. Three sequential pairs reverse variant order in the middle pair. Package verification is outside timing. Baseline/current closures share the same global helper environment.

| Trial | Baseline (s) | Current (s) |
| --- | ---: | ---: |
| 1 | 16.28 | 14.48 |
| 2 | 16.38 | 14.55 |
| 3 | 16.58 | 14.53 |
| Median | 16.38 | 14.53 |

The complete save improved by 1.85 seconds, about **11%**, for this workbook. This is additional to the installed row-batching change; the percentage must not be added to the preceding 63% figure. It is not a measured startup, numerical-analysis or other-workbook improvement.

## Verification

- `scripts/validate_excel_style_cache.R`: seven individual exports and one accumulated collection pass exact package comparison. Cases cover the preceding string/merge/empty-cell fixtures and 24 horizontal/vertical alignment × bold × italic combinations, rendered with and without headers.
- Individual export warning sequences and post-save RNG states match between variants.
- All sheet names and read-back cells match. Every non-core package part is byte-identical, including styles, worksheet contents, merges, dimensions, relationships and print settings.
- Core metadata also matches after normalizing only created/modified timestamps.
- All six full-size measured exports pass the same package comparison against the earlier workbook; timed saves produce no warnings.
- `git diff --check` passes for the relevant application/test files.

No numerical calculation or displayed/export content changed. HTML/PDF/Word/HWPX writers were not modified or rebuilt. No installer was rebuilt.

## Artifacts and reproduction

Baseline, shared setup, measurements, workbooks, extracted package parts and summary: `output/excel-style-cache-20260914/`. Validation: `scripts/validate_excel_style_cache.R`.

Run scripts sequentially from the repository root with bundled Rscript `--vanilla`, English UTF-8 Windows locale, bundled `R_LIBS_USER`, `STATEDU_NO_PACKAGE_INSTALL=true` and an isolated module cache. `measure.R` takes `baseline|current` and a fresh trial ID; `summarize.R` summarizes preserved trials 1–3. Input HTML is the same `output/table-render-combined-20260914/current-correlation.html` used for row-batching validation.
