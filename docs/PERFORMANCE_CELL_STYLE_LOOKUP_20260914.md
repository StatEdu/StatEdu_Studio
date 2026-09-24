# Avoid temporary data frames in cell style lookup — 2026-09-14

The shared `result_cell_style_extra()` helper previously filtered the entire style data frame for each cell. For ordinary character style vectors and ordinary logical selections, it now selects the style vector directly and concatenates the selected strings. Duplicate-match order and missing-value behavior remain unchanged. Custom data-frame classes, classed/matrix style columns and unusual selection shapes keep the original subsetting path. No numerical or output-formatting rule changed.

## Measurement

Bundled R 4.5.3, loaded modules/preferences, 150 data rows, Pearson with p/CI display. Three rounds alternate variant order; each sample renders the complete correlation result to HTML. Both helper closures use the same global environment and are swapped in the same slot.

| Variables | Baseline median (s) | Current median (s) |
| --- | ---: | ---: |
| 10 | 0.17 | 0.18 |
| 30 | 1.36 | 1.36 |
| 50 | 4.60 | 4.51 |

All three 50-variable comparisons favored the candidate, with about 2% median improvement. The small-table results show no demonstrated improvement; the 0.01-second difference is close to timer granularity. This is a modest server-side HTML rendering optimization, not a measured startup or numerical-analysis speedup. Previous overview-row construction improvements are present in both variants.

## Verification

- `scripts/validate_cell_style_lookup.R`: 3,003 exact style-value/condition comparisons covering missing selectors/styles, duplicate concatenation order, empty metadata, factor styles and custom data-frame classes.
- Complete observed/latent-option correlation screens, saved HTML bytes, accumulated table extraction, Excel sheet names and cell values match exactly with the respective helper active throughout rendering/export. The script reuses the export portion of `validate_correlation_overview_rows.R` while swapping this turn's helper.
- `validate_screen_table_export.R`: both REML variants passed, 18 unchanged tables each, identical screen/report table content and orientations, no added cover.
- A fresh full mock session uploads/selects the fixture and runs Pearson/KM through actual handlers. Complete result objects, live HTML/dependencies, notifications and warnings exactly match the preceding version (`identical(..., num.eq=FALSE)`). No captured warnings occurred.
- `git diff --check` passed for relevant application/test files.

**Existing test failure:** `validate_result_table_contract.R` stops at an assertion expecting `Note. Values are estimates. CI = confidence interval. * p < .05.` The same assertion fails with this turn's saved baseline helper file. The repository's current output contract prohibits the `Note.` prefix. This unrelated test expectation was not changed; the suite is not reported as passing.

Displayed/export content is unchanged. PDF/Word/HWPX binaries were not regenerated; HTML and accumulated document inputs were checked. No installer was rebuilt.

## Artifacts

Baseline source, shared setup, benchmark, raw/summary timings, exports and the baseline-contract reproduction script are in `output/cell-style-lookup-20260914/`. Full-session snapshot: `output/server-first-analysis-20260913/current-cell-style-verified.rds`.

Run validation/benchmark scripts sequentially from the repository root with bundled Rscript `--vanilla`, English UTF-8 Windows locale, bundled `R_LIBS_USER`, `STATEDU_NO_PACKAGE_INSTALL=true` and an isolated module cache.
