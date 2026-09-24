# Read the first matching merge record directly — 2026-09-14

`result_cell_span_start()` previously copied every matching merge record and then selected its first row. For ordinary data frames with unclassed columns and a complete ordinary logical selection, it now finds the first matching index and copies only that row. No match still returns NULL. Missing selectors, classed columns and custom data-frame types retain the original path. First-match precedence, returned metadata and merge behavior remain unchanged.

## Measurement

Bundled R 4.5.3, initialized modules/preferences, sequential complete table-to-HTML rendering. Five rounds alternate baseline/current order after equality checks. Both helper closures share the same global environment and are swapped in the same slot. Synthetic tables have six columns, with B–D merged on each row in merged cases. The preceding row-filter improvement is enabled in both variants.

| Rows | Merges | Baseline median (s) | Current median (s) |
| --- | --- | ---: | ---: |
| 20 | None | 0.05 | 0.03 |
| 200 | None | 0.37 | 0.37 |
| 20 | One per row | 0.03 | 0.03 |
| 200 | One per row | 0.35 | 0.32 |

The 200-row merged table improved about 9% (0.03 seconds). Small merged tables showed no change. The no-merge early return is unchanged, so the small unmerged timing difference is not attributed to this change. This is a scoped rendering benchmark, not an overall statistical-analysis or startup speedup.

## Verification

- `scripts/validate_span_start_lookup.R`: 3,221 exact return-value/error/condition comparisons, including returned data-frame metadata, duplicate first-match precedence, empty metadata, missing values, classed values, custom data frames and unusual row indices.
- Twelve Korean/English merged/unmerged HTML comparisons preserve colspan, values and styles. Saved HTML, accumulated table extraction, Excel sheet names and cell contents match exactly with the respective helper active throughout rendering/export.
- `validate_screen_table_export.R`: both REML variants pass, retaining all 36 table contents and screen/report orientations.
- A fresh full mock session uploads/selects the fixture and executes Pearson/KM through actual handlers. Complete result objects, live HTML/dependencies, notifications and warnings exactly match the previous version (`identical(..., num.eq=FALSE)`). No captured warnings occurred; the existing ggplot informational message remains.
- `git diff --check` passes for application/test files.

No numerical calculation or displayed/export content changed. PDF/Word/HWPX binaries were not regenerated; HTML and accumulated document inputs were checked. No installer was rebuilt.

## Artifacts

Baseline, shared setup, benchmark, raw/summary timings and Excel artifacts: `output/span-start-lookup-20260914/`. Full-session snapshot: `output/server-first-analysis-20260913/current-span-start-verified.rds`.

Run scripts sequentially from the repository root using bundled Rscript `--vanilla`, English UTF-8 Windows locale, bundled `R_LIBS_USER`, `STATEDU_NO_PACKAGE_INSTALL=true` and an isolated module cache.
