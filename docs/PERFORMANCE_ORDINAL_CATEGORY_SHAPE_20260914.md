# Ordinal cache: more categories and unequal frequencies — 2026-09-14

This follow-up changes no production code. It compares the integrated guarded matrix-check cache with the actual pre-integration correlation source on seven- and ten-category data, and on associated seven-category data with unequal frequencies and missing values.

## Fixture and timing method

Bundled Windows R 4.5.3; three fresh processes per version, run sequentially with version order reversed in set two. Each fixture has 10,000 rows, eight ordinal variables and 28 pairs. Latent correlations and normality output are enabled. Seed 33 creates the data; every analysis starts with seed 99. Each condition has a first and a subsequent timed full-analysis call, and their returned values, conditions, stdout and RNG state must match exactly.

The seven- and ten-category fixtures discretize independent normal variables at equal-probability cut points. The unequal-frequency fixture uses a shared normal component with loading sqrt(0.75), independent components with loading sqrt(0.25), and cumulative cut probabilities 0.01, 0.03, 0.08, 0.20, 0.50, 0.85. Each variable then has an independently selected 10% of rows set to missing. This yields population category probabilities 1%, 2%, 5%, 12%, 30%, 35%, 15% before missingness.

Condition order is seven categories, ten categories, then unequal frequencies with missing values. Only the first seven-category call includes first-ever lazy engine setup. Bootstrap, fixture construction, rendering and export are excluded. These timings do not measure Electron loading or UI latency, and the eight-variable fixture cannot be directly compared with prior ten- or twenty-variable timings.

## Median elapsed seconds

| Condition | Baseline subsequent | Current subsequent | Reduction | Baseline first in condition | Current first in condition |
| --- | ---: | ---: | ---: | ---: | ---: |
| Seven categories | 1.81 | 1.48 | 18.2% | 2.00 | 1.73 |
| Ten categories | 3.70 | 3.00 | 18.9% | 3.78 | 3.09 |
| Seven categories, unequal frequencies, associated, missing | 3.25 | 2.74 | 15.7% | 3.33 | 2.64 |

Every paired first and subsequent measurement was faster on the current source. These are three-repetition local measurements rather than confidence bounds or universal speed guarantees. The earlier binary fixture's first-call slowdown remains a documented limitation; this follow-up does not remove that initialization cost.

## Result preservation

All nine complete baseline/current comparisons passed `identical(..., num.eq = FALSE)`, including every returned value, captured warning/message, stdout and RNG state. All 18 within-process first/subsequent comparisons also passed. All results contained 28 pairwise rows and eight-by-eight correlation matrices; all 28 off-diagonal estimates were finite and within [-1, 1]. No warnings or messages were captured in these fixtures.

Estimated correlation ranges were -0.021372 to 0.02465537 for seven categories, -0.01829258 to 0.02322284 for ten categories, and 0.7381033 to 0.7537232 for associated unequal-frequency data. Thus the timing comparison did not merely measure failed estimates. The current engine was confirmed initially uninitialized, then active with cache hits in every process.

Artifacts are in `output/ordinal-category-shape-20260914/`: source snapshots, `measure.R`, `compare.R`, six timing CSVs and 18 result RDS files. Current snapshot and production source both have SHA-256 `C4E57EC744E133186842007DD732EEBF561AC4B127A22167024E3E3D99D91CE4`.

No new memory, rendering or export tests were run, and no installer was built. Prior integrated export and memory evidence is recorded separately in `PERFORMANCE_ORDINAL_CHECK_CACHE_INTEGRATION_20260914.md`.
