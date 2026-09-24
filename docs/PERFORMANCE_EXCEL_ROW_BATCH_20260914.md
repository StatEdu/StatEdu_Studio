# Batch contiguous Excel source cells by row — 2026-09-14

`add_screen_excel_table()` now writes contiguous source cells from the same row in one `openxlsx::writeData()` call. Runs stop at a row change or a gap in source-cell columns, so merged continuation cells remain unwritten. Row-by-row source order preserves shared-string registration order. Merge operations and per-cell style creation/application retain their previous order and logic.

For the measured 30-variable correlation export, each of three sheets has 961 source cells and 31 contiguous row runs. Source-cell write calls fall from 2,883 to 93. Leading headings and trailing notes keep their existing write paths. Numerical computation and HTML parsing are unchanged.

## Whole-save measurement

Bundled R 4.5.3, identical saved correlation HTML, fresh processes with modules/preferences loaded, no profiler. Three baseline/current pairs execute sequentially, reversing order in the middle pair. Timings cover the complete Excel save and finalization; package comparison follows outside timing. Both helper closures use the same global environment, and every other helper is current in both variants.

| Trial | Baseline (s) | Current (s) |
| --- | ---: | ---: |
| 1 | 44.69 | 16.39 |
| 2 | 44.72 | 16.41 |
| 3 | 44.51 | 16.33 |
| Median | 44.69 | 16.39 |

The complete save improved by 28.30 seconds, about **63%**, for this three-sheet, 30-variable correlation result. This is an Excel export improvement; it does not measure statistical fitting, browser rendering, application startup or other workbook sizes. The earlier profiled pilot (16.92 seconds) is excluded from these medians.

## Exact verification

- `scripts/validate_excel_row_batch.R` compares five individual exports and one accumulated collection. Cases include row/column spans, holes and empty rows, single-column tables, header-only input, empty strings, Unicode, multiline text, leading zeros, decimal precision, p-value thresholds, date-like strings and formula-like text.
- Both variants preserve sheet names and every read-back cell value. All package parts outside `docProps/core.xml` are byte-identical, covering shared strings, worksheets, styles, merges, row/column sizing, relationships and print settings.
- Core XML is also identical after normalizing **only** its created/modified timestamps; other core metadata is compared unchanged.
- All six full-size measured exports pass the same package comparison against the preceding implementation's workbook: 16 non-core package parts match exactly, plus normalized core metadata. No warnings occurred during those timed saves.
- `git diff --check` passes for the changed application/test files.

No displayed content, numerical result or document-format rule changed. HTML/PDF/Word/HWPX writers were not modified or rebuilt. No installer was rebuilt.

## Reproduction and artifacts

Baseline, common setup, per-process measurement script, workbooks, extracted package parts, raw timing files, medians and write counts: `output/excel-row-batch-20260914/`.

From the repository root, run with bundled Rscript `--vanilla`, English UTF-8 Windows locale, bundled `R_LIBS_USER`, `STATEDU_NO_PACKAGE_INSTALL=true`, and an isolated module cache:

```text
scripts/validate_excel_row_batch.R
output/excel-row-batch-20260914/measure.R baseline <fresh-id>
output/excel-row-batch-20260914/measure.R current <fresh-id>
output/excel-row-batch-20260914/summarize.R
```

The summary audits preserved trials 1–3. Keep workloads sequential; use fresh IDs for new trials. The measured input is `output/table-render-combined-20260914/current-correlation.html`.
