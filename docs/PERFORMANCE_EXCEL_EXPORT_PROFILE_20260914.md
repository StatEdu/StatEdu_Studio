# Excel export bottleneck profile — 2026-09-14

This audit separates table extraction from workbook writing for an already saved 30-variable correlation HTML result. It adds profiling/verification tools and makes no application-code change.

## Elapsed time

Two fresh bundled R 4.5.3 processes, initialized modules/preferences, the same approximately 1.97 MB HTML input and three resulting Excel sheets. Each run combines lightweight elapsed-time wrappers with 10 ms R sampling profiling.

| Stage | Run 1 (s) | Run 2 (s) | Median (s) |
| --- | ---: | ---: | ---: |
| Entire Excel save | 45.82 | 45.42 | 45.62 |
| Result-table extraction | 1.57 | 1.55 | 1.56 |
| Writing the three sheets | 39.12 | 38.97 | 39.045 |

Sheet writing occupies about 86% of elapsed save time in this fixture. HTML extraction is about 3%. The remaining time includes workbook serialization/finalization and surrounding work. These are diagnostic measurements, not before/after optimization results. They exclude original analysis and HTML generation.

Nested helper totals must not be added again: `result_html_table_cells`, `result_docx_table_payload` and document/paragraph extraction overlap with parent extraction work. Windows R sampling recorded less time than elapsed time; its percentages are not wall-time fractions.

## Next optimization target

Run 1's inclusive sampling shares were approximately 59.8% for `openxlsx::writeData`, 20.4% for `openxlsx::addStyle`, 10.3% for workbook saving and 4.0% for `openxlsx::createStyle`. These shares are sampled, nested observations, not independent savings estimates.

Source inspection confirms that `add_screen_excel_table()` calls `writeData()` and creates/applies a style for every source cell. The strongest next candidate is writing eligible cell values in batches, followed by investigating repeated style application. Any implementation must preserve intentional strings (display precision and p-value thresholds), source-cell placement, blank merged continuations, merge geometry, styles, notes, widths/heights and print settings. No batching or style change was installed in this audit.

The HTML parser also reads table cells once for screen data and again for the Word payload, but extraction is a much smaller part of this workload; this profile does not justify prioritizing that duplication over workbook writing.

## Verification

Both profiled exports match the preceding unprofiled workbook:

- All three sheet names and read-back cell contents are exactly identical.
- All 16 package parts outside `docProps/core.xml` are byte-identical after extraction, covering worksheet/style/relationship content and workbook settings.
- The entire core metadata file is excluded because it contains time-dependent creation/modification metadata; no equivalence claim is made for that excluded part.
- No warnings were captured during either save.

Application code and content are unchanged. No installer or PDF/Word/HWPX outputs were rebuilt.

## Reproduction

Run `scripts/profile_excel_export.R <fresh-id>` from the repository root with bundled Rscript `--vanilla`, English UTF-8 Windows locale, bundled `R_LIBS_USER`, `STATEDU_NO_PACKAGE_INSTALL=true` and an isolated module cache. Run sequentially. It reads `output/table-render-combined-20260914/current-correlation.html` and compares against that directory's `current-correlation_30.xlsx`.

Artifacts: `output/excel-export-profile-20260914/`, including helper elapsed times, full/self sample profiles, saved workbooks and extracted package parts. `summarize.R` summarizes preserved runs 1–2.
