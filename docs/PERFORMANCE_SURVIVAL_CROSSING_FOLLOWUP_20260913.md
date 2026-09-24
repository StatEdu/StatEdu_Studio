# Reuse group follow-up ends in crossing diagnostics — 2026-09-13

Full-analysis timing audit: `PERFORMANCE_SURVIVAL_TIMING_AUDIT_20260913.md` uses aligned helper environments and the actual before/after snapshots of this change. The twenty-group full-analysis result was 0.3933 to 0.3967 seconds: no measurable benefit, consistent with the limited full-analysis conclusion below. Helper timings are distinct from total analysis timing.

`survival_km_crossing_diagnostics()` previously searched all summary rows twice for every group pair to find each group's last observation time. It now caches that maximum on first use within the current function call. The pair order, step interpolation, shared grid, subtraction, tolerance and crossing detection are unchanged. A warning or message prevents caching that value, preserving repeated diagnostics.

## Validation

- `scripts/validate_survival_crossing_followup.R`: 96 exact table/condition/RNG comparisons passed against both the immediate baseline and an uncached reference. Covers 1/2/3/10/20 groups, ties, all/no events, three tolerance levels, malformed/empty summaries, and warning/message-emitting maximum calculations.
- The full-analysis benchmark asserts exact equality of the entire prepared result before timing.
- Complete result/RNG, screen markup, HTML bytes and Excel sheet names/cells matched exactly for four groups; reproduced by `output/survival-crossing-followup-20260913/verify-output.R`.

## Measurements

Bundled R 4.5.3, application preferences loaded. Five alternating before/after rounds, each batching three calls and dividing elapsed time by three before taking the median. Crossing helper uses a fitted model from 20,000 rows.

| Groups | Before seconds | After seconds |
| ---: | ---: | ---: |
| 2 | 0.0067 | 0.0100 |
| 10 | 0.0400 | 0.0367 |
| 20 | 0.1433 | 0.1233 |

The twenty-group helper fixture improved about 14%. The two-group case has no reusable maximum and showed no benefit; short measurements are sensitive to timer granularity. These helper timings do not imply the same percentage change in total analysis time.

A separate 2,000-row, twenty-group complete-analysis benchmark measured 0.43 seconds before and after. This fixture shows no measurable overall acceleration. Full timing includes preflight, fitting, diagnostics, life table and pairwise tests, excluding UI rendering/export.

Baseline and reproduction scripts: `output/survival-crossing-followup-20260913/`. No displayed/report content changed. No installer was rebuilt.
