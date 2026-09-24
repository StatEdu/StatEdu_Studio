# Reuse life-table risk comparisons — 2026-09-13

For unclassed time values, each life-table interval now computes its lower-bound comparison once and reuses it for interval membership and the at-risk count. The initial interval still includes time zero; later intervals retain their strict lower bound and inclusive upper bound. Counts, NA handling and survival-probability arithmetic are unchanged. Classed time values retain the original comparison calls, preserving method dispatch and diagnostics.

## Validation

- `scripts/validate_survival_risk_mask.R`: 36 exact table/condition/RNG comparisons against both the actual baseline and original-comparison reference. Covers zero/negative times, exact boundaries, duplicate breaks, empty/missing/infinite values and a custom comparison method emitting warnings.
- `scripts/validate_survival_life_table_build.R` against the actual baseline: 30 additional comparisons passed.
- Complete grouped life-table analysis results and RNG matched; screen HTML, saved HTML bytes and Excel sheet names/cell values matched. Existing ggplot coordinate-system replacement messages occurred on both output paths.
- Benchmark tables matched exactly; `git diff --check` passed.

## Final timing

Bundled R 4.5.3 with application modules/preferences loaded; 100,000 observations, 20 groups; five alternating before/after runs, median complete life-table helper seconds:

| Intervals | Before | After |
| ---: | ---: | ---: |
| 10 | 0.03 | 0.03 |
| 50 | 0.13 | 0.11 |
| 100 | 0.22 | 0.20 |

The final 100-interval case improved about 9%. An earlier candidate without the class fallback measured 0.19 seconds after; the table above is the final guarded implementation. These are fixture-specific helper timings, not total survival-analysis or startup timings.

Artifacts and baseline: `output/survival-risk-mask-20260913/`. No displayed/report content changed. No installer was rebuilt.
