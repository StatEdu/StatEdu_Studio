# Survival benchmark environment audit — 2026-09-13

No application code changed. This pass remeasures three recent improvements after the separately sourced baseline environment was found to confound isolated-helper timing comparisons.

## Controls

- Both compared helper closures are rebound to `.GlobalEnv` and installed in the same global helper slot.
- All other current application helpers remain identical during each comparison.
- The follow-up cache comparison uses its actual before and after snapshots, excluding the later crossing-table construction change.
- Inputs, RNG seed and loaded application preferences are identical. Each pair is checked for exact complete-result, diagnostic and RNG equality before timing.
- Bundled R 4.5.3; 2,000 rows; two or twenty groups; five alternating rounds of three full analysis calls. Timing runs execute without concurrent validation workloads.

Full analysis includes preflight, fitting, crossing diagnostics, life table and pairwise tests. Startup, UI rendering and export are excluded. These are isolated changes in the current application context, not a reconstruction of all historical application versions. Do not add their percentages or compare absolute elapsed times between sessions.

Artifacts: `output/survival-timing-audit-20260913/audit.R` and `audit.csv`. Original historical measurements remain in their artifact directories for provenance; the audited full-analysis values supersede the corresponding performance claims for the current context. Previous numerical and export equivalence evidence is unaffected by this timing-method correction.

No installer was rebuilt.

## Results

All six complete-result/diagnostic/RNG comparisons passed exactly. Median elapsed seconds:

| Change | Groups | Before | After |
| --- | ---: | ---: | ---: |
| Rank covariance | 2 | 0.0400 | 0.0433 |
| Rank covariance | 20 | 0.3967 | 0.3667 |
| Follow-up maximum reuse | 2 | 0.0400 | 0.0400 |
| Follow-up maximum reuse | 20 | 0.3933 | 0.3967 |
| Crossing table construction | 2 | 0.0433 | 0.0400 |
| Crossing table construction | 20 | 0.4400 | 0.3767 |

For twenty groups, covariance vectorization reduced full-analysis time by about 8%, and building the crossing table once by about 14%. Follow-up maximum reuse had no measurable full-analysis benefit here. Differences around 0.003 seconds, including all two-group changes, are too small for a meaningful speedup claim with this timing method.
