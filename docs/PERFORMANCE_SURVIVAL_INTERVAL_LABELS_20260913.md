# Reuse life-table interval labels — 2026-09-13

All groups in a life table use the same interval boundaries. The function now lazily formats each interval label on its first use and reuses it for subsequent groups within that call. Numeric calculations and row order remain unchanged. A formatting warning or message prevents caching of that label, retaining repeated diagnostic behavior. No label state persists across calls or preference changes.

## Validation

- `scripts/validate_survival_interval_labels.R`: ten precision/diagnostic/RNG scenarios passed against the actual baseline and an uncached reference. Covers multiple output precisions, grouped/ungrouped data and warning/message-emitting formatters.
- `scripts/validate_survival_life_table_build.R` against the actual baseline: 30 exact construction/condition/RNG comparisons passed.
- All benchmark life tables matched exactly.
- Full grouped life-table result/RNG and screen/HTML/Excel comparisons passed; reproduced by `output/survival-interval-labels-20260913/verify-output.R`.
- `git diff --check` passed.

## Timing

Bundled R 4.5.3 with application modules/preferences loaded; 100,000 rows and 20 groups. Five alternating before/after runs, median complete life-table helper seconds:

| Intervals | Before | After |
| ---: | ---: | ---: |
| 10 | 0.05 | 0.03 |
| 50 | 0.11 | 0.09 |
| 100 | 0.20 | 0.16 |

The 100-interval fixture improved about 20%. These are helper timings, not whole survival analysis or startup measurements. The benefit comes from repeated labels across groups; ungrouped tables cannot reuse labels this way.

Artifacts and baseline: `output/survival-interval-labels-20260913/`. No displayed/report content changed. No installer was rebuilt.
