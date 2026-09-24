# Survival cell footnote lookup — 2026-09-13

`survival_cell_note_marker()` previously filtered the full marker data frame for each table cell, then read only the first matching marker. For ordinary marker tables with a complete logical selection and an unclassed marker column, it now locates the first match and reads that element directly. It still scans the matching conditions, but avoids copying a data frame per cell.

Duplicate matches retain the first marker. Missing selection values, custom data-frame subclasses, classed marker columns and unusual selection shapes keep the original subsetting path. The no-marker early return is unchanged. Numerical calculations, cell formatting, note text and export content are unchanged.

## Measured effect

Bundled R 4.5.3, initialized preferences/modules, sequential render-to-HTML benchmark. Five rounds alternate baseline/current order; each sample averages three renders. Both helper closures use the same global environment and are swapped into the same global slot. Six-column tables have either no marker attribute or one marker per row on column V2.

| Rows | Markers | Baseline median (s) | Current median (s) |
| --- | --- | ---: | ---: |
| 20 | None | 0.0267 | 0.0267 |
| 200 | None | 0.1933 | 0.1933 |
| 20 | One per row | 0.0267 | 0.0267 |
| 200 | One per row | 0.2567 | 0.2300 |

The 200-row marked table rendered about 10% faster. Small tables and unmarked tables showed no measurable improvement. This is a scoped rendering improvement, not a measured reduction in overall statistical analysis or startup time.

## Verification

- `scripts/validate_survival_marker_lookup.R`: 6,005 exact value/condition comparisons covering first-match precedence, absent markers, missing values, factor markers, custom data-frame subclasses and missing marker fields.
- Korean/English marked-table HTML, saved document HTML, accumulated table extraction, Excel sheet names and cell values match exactly with the respective helper active throughout each path.
- Complete fixture KM saved HTML and report-mode HTML also match exactly in both languages.
- A fresh full mock session uploads/selects the fixture and executes Pearson and KM through the real handlers. Complete results, live HTML/dependencies, notifications and warnings exactly match the preceding table-style version (`identical(..., num.eq=FALSE)`). No captured warnings occurred. The existing ggplot coordinate-replacement informational message is unchanged.
- `git diff --check` passed.

No displayed/export content changed. PDF/Word/HWPX binaries were not regenerated; HTML/report and accumulated content checks cover the unchanged document inputs. No installer was rebuilt.

## Reproduction

Use bundled Rscript `--vanilla` from the repository root, `LC_ALL`/`LANG=English_United States.utf8`, the bundled library as `R_LIBS_USER`, `STATEDU_NO_PACKAGE_INSTALL=true`, and an isolated module cache. Run the validation script and `output/survival-marker-lookup-20260913/benchmark.R` sequentially.

Baseline source, Excel artifacts and raw/summary timings are in `output/survival-marker-lookup-20260913/`. The full-session snapshot is `output/server-first-analysis-20260913/current-marker-lookup-verified.rds`.
