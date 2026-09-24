# Build the survival posthoc table once — 2026-09-13

Measurement correction: the original timings below used different closure environments, allowing nested helper lookup costs to affect the comparison. With both compared functions bound to the application global environment, a fresh five-round batched measurement was **0.4567 to 0.3700 seconds (about 19%)**, not the previously reported 39%. Full result equality passed again. See `output/survival-posthoc-index-review-20260913/benchmark-prior-posthoc-corrected.R` and its CSV. The original table is retained as historical evidence and is superseded by this correction.

`survival_km_posthoc_table()` now collects successful pairwise test results as lists and constructs the table once. Previously every successful pair built a one-row data frame, then all rows were bound together. Pair selection, factor preparation, weighted rank tests, omission of failed/undefined comparisons, row order, column types and Holm adjustment remain unchanged.

## Validation

- `scripts/validate_survival_posthoc_table.R`: all 144 exact table/condition/RNG comparisons passed against both the immediate baseline and a restored row-wise reference. Cases cover 1/2/3/10 groups, all three test methods, delayed entry, tied times, all/no events and missing event values.
- The full-analysis benchmark compares the entire prepared result exactly before timing.
- Complete result/RNG, screen markup, HTML bytes and Excel sheet names/cells matched exactly: `output/survival-posthoc-table-20260913/verify-output.R`, using four groups.

## Measurement scope

Bundled R 4.5.3 with application preferences loaded, 2,000 rows and twenty groups. Five alternating before/after rounds each batch three complete analysis calls. Report the median elapsed time per call. Full analysis includes preflight, fitting, crossing diagnostics, life table and pairwise tests; UI rendering/export are excluded.

| Run | Before seconds | After seconds |
| --- | ---: | ---: |
| Initial | 0.6033 | 0.3767 |
| Confirmation | 0.6100 | 0.3700 |

The unexpectedly large initial improvement was checked in a second independent benchmark process after export verification finished. The confirmation measured about 39% shorter complete-analysis time. Both runs are retained as `benchmark-full-first.csv` and `benchmark-full.csv`. These measurements apply to this twenty-group fixture, not universally to every survival analysis.

Baseline and reproduction scripts: `output/survival-posthoc-table-20260913/`. No displayed/report content changed. No installer was rebuilt.
