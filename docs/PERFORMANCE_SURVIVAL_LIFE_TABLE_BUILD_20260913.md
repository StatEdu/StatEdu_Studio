# Bulk life-table construction — 2026-09-13

`survival_life_table()` now stores each completed interval row as a list and builds the final data frame once. Previously each interval created a small data frame, followed by row binding. Every count, probability, multiplication and interval-label formatting call remains in the original loop/order. Final names, column types and row order are preserved, including the original NULL result when no group rows are produced.

## Validation

- `scripts/validate_survival_life_table_build.R`: 30 exact table/condition/RNG scenarios against the actual pre-change file and a reference using row-by-row data-frame construction. Includes empty/single-row inputs, empty groups, factors with unused levels and invalid/duplicate breaks.
- `scripts/validate_survival_group_reuse.R` against the actual baseline: 72 exact life-table/condition/RNG comparisons passed.
- Complete grouped life-table analysis results and RNG matched. Screen HTML, saved HTML bytes and Excel sheet names/cell data matched. Both output paths emitted the existing ggplot coordinate-system replacement messages.
- All benchmark tables matched exactly; `git diff --check` passed.

## Timing

Bundled R 4.5.3 with application modules and preferences loaded; 10,000 observations and 20 groups. Five alternating before/after runs, median complete life-table helper seconds:

| Intervals | Before | After |
| ---: | ---: | ---: |
| 10 | 0.04 | 0.02 |
| 50 | 0.20 | 0.05 |
| 100 | 0.39 | 0.09 |

The 100-interval case improved about 77%. These timings cover the life-table helper, not all survival analysis or startup. An initial probe without application preferences was stopped and replaced by this session-equivalent benchmark; its timings are not reported.

Artifacts and baseline: `output/survival-life-table-build-20260913/`. No displayed/report content changed. No installer was rebuilt.
