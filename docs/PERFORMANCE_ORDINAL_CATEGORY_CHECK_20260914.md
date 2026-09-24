# Ordinal cache category-count follow-up — 2026-09-14

No production code changes in this follow-up. The integrated guarded matrix-check cache was compared with its actual pre-integration source for two- and five-category ordinal data.

## Method

Bundled Windows R 4.5.3; three fresh processes per version, sequential, reversing version order in set two. Each process analyzes 10,000 rows and 10 ordinal variables (45 pairs), with latent correlations and normality output enabled. Seed 33 generates independent normal variables converted using equal-probability category cuts. Missing-data fixtures remove independently selected 10% of each variable. Every condition has a first call and a subsequent timed call; both reset RNG to seed 99.

Condition order is binary, binary with missing values, five categories, five categories with missing values. Only the first binary call includes first-ever lazy engine setup. Bootstrap, input generation, UI rendering and exports are outside timing. These are analysis timings, not Electron loading measurements. The 10-variable timings should not be compared directly with the earlier 20-variable integration benchmark.

## Median elapsed times

| Condition | Baseline subsequent | Current subsequent | Reduction | Baseline first in condition | Current first in condition |
| --- | ---: | ---: | ---: | ---: | ---: |
| Two categories | 0.37 s | 0.34 s | 8.1% | 0.57 s | 0.61 s |
| Two categories, missing | 0.37 s | 0.35 s | 5.4% | 0.38 s | 0.36 s |
| Five categories | 1.61 s | 1.33 s | 17.4% | 1.61 s | 1.33 s |
| Five categories, missing | 1.60 s | 1.42 s | 11.3% | 1.69 s | 1.33 s |

Five-category subsequent runs improved in all three paired comparisons. Binary gains were small (one missing-data pair tied at 0.36 s). The first binary call was slower in every current run: 0.61 s versus baseline 0.56–0.58 s, a median increase of 0.04 s. Lazy initialization can outweigh savings in a smaller workload; this check does not support a universal speedup claim. Three repetitions describe this local fixture, not statistical confidence bounds.

## Result preservation and evidence

All 12 baseline/current comparisons passed `identical(..., num.eq = FALSE)` for complete returned results, warnings/messages, stdout and RNG state. Every first/subsequent comparison also matched within each process and condition. All returned pairwise tables contained 45 rows and correlation matrices contained 10 rows. Current engines were initially uninitialized, then confirmed active with cache hits.

Artifacts: `output/ordinal-category-check-20260914/` contains exact baseline/current source snapshots, `measure.R`, `compare.R`, six timing CSVs and 24 result RDS files. The current source SHA-256 is `C4E57EC744E133186842007DD732EEBF561AC4B127A22167024E3E3D99D91CE4`.

No new memory or export measurements were made, and no installer was built. Prior integrated memory and export evidence remains documented in `PERFORMANCE_ORDINAL_CHECK_CACHE_INTEGRATION_20260914.md`; it is not new evidence for these category fixtures.
