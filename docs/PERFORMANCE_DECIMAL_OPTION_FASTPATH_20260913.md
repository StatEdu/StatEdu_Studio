# Skip redundant normalization of valid integer precision — 2026-09-13

`normalize_output_decimal_digits()` now immediately returns the scalar value of an unclassed integer in the supported 0–5 range. Such values are already normalized by application preferences. Other inputs continue through the original conversion, rounding, range limiting and default logic. Named integers return an unnamed scalar as before. No preference is cached, so changes still take effect on the next call.

## Validation

`scripts/validate_decimal_option_fastpath.R` passed 51 exact normalization/type/condition/RNG comparisons against both the immediate source baseline and a reference without the early return. Includes boundaries, fractional numbers, strings, missing/default values, lists, named values, matrices, factors and custom classes.

The 3,108 decimal-output comparisons, 72 correlation-table comparisons and 44 full-result/condition/RNG comparisons also passed against the preserved baseline where supported. General and latent correlation screen markup, HTML bytes and Excel sheet names/cells matched exactly.

## Measurements

Bundled R 4.5.3, loaded preferences, same global environment for both normalizer closures. Five alternating rounds, median elapsed seconds. Formatter workloads use 10,000 signed values; whole analysis uses Pearson with 300 rows and 100 variables, excluding startup/rendering/export. Validation and exports run after timing.

| Raw precision option | Workload | Before | After |
| ---: | --- | ---: | ---: |
| 2L | Format 10,000 values | 0.06 | 0.04 |
| 2L | Full Pearson | 0.62 | 0.61 |
| 6L | Format 10,000 values | 0.06 | 0.06 |
| 6L | Full Pearson | 0.64 | 0.65 |
| 5L (separate run) | Format 10,000 values | 0.07 | 0.03 |
| 5L (separate run) | Full Pearson | 0.64 | 0.63 |

The raw 6L option exercises the original fallback and is normalized to five decimal places. It is not the normal persisted representation of an already normalized setting. The smaller full-analysis differences here are near timing resolution; no broad acceleration is inferred from them.

The separate 5L run confirms a formatter benefit for a normal persisted setting (0.07 to 0.03 seconds per 10,000 values), but only a small whole-analysis difference (0.64 to 0.63 seconds). Formatter gains should not be presented as whole-analysis gains.

Artifacts, baseline and reproduction scripts: `output/decimal-option-fastpath-20260913/`. No displayed/report content changed. No installer was rebuilt.
