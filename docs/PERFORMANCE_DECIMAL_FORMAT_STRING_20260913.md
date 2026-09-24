# Pass decimal precision directly to sprintf — 2026-09-13

`format_decimal3()` now uses `sprintf("%.*f", digits, x)` instead of allocating a new format string with `paste0()` on every call. It still reads and normalizes the current precision option on every call; no setting is cached. The leading-zero handling and rounding behavior remain unchanged.

## Validation

`scripts/validate_decimal_format_string.R` passed all 3,108 exact decimal-output/condition/RNG comparisons against both the immediate baseline and a reference restoring dynamic string construction. It includes changing precision requests 0–6, signed/extreme values, invalid inputs and custom scalar/vector/empty behavior. The utility normalizes requested precision to 0–5, so a request of 6 tests the upper-bound clamp, not six displayed decimal places.

The existing 72 table comparisons and 44 full correlation result/condition/RNG comparisons also passed. General and latent correlation screen markup, saved HTML bytes, and all Excel sheet names/cells matched exactly.

## Benchmark controls

Bundled R 4.5.3, application preferences loaded. Both compared formatter closures use `.GlobalEnv`. Five alternating timing rounds; medians of elapsed seconds. Formatter batches use 10,000 signed values. Full analysis uses 300 rows, 100 continuous variables and Pearson, without normality testing. Startup, UI rendering and export are excluded. Validation/export workloads follow the timing run.

Artifacts, immediate baseline and reproduction scripts: `output/decimal-format-string-20260913/`. No displayed/report content changed. No installer was rebuilt.

## Results

| Actual decimal places | Workload | Before | After |
| ---: | --- | ---: | ---: |
| 2 | Format 10,000 signed values | 0.07 | 0.05 |
| 2 | Full Pearson analysis | 0.65 | 0.63 |
| 5 (requested 6) | Format 10,000 signed values | 0.08 | 0.06 |
| 5 (requested 6) | Full Pearson analysis | 0.67 | 0.65 |

Full analysis improved about 3% in these fixtures. The formatter savings are larger in relative terms but represent only part of total analysis time; short helper timings are sensitive to timer resolution.
