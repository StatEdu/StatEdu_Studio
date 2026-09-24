# Simplify decimal leading-zero removal — 2026-09-13

`format_decimal3()` and `format_decimal2()` now share `statedu_strip_decimal_zero()`. For a single nonmissing formatted string it removes `0.` or `-0.` prefixes directly; other strings are returned unchanged. Vector/empty/missing string results retain the original two regular-expression substitutions. `sprintf()`, precision selection and input handling remain unchanged, including negative rounded zero.

## Validation

- `scripts/validate_decimal_prefix.R`: 3,108 exact output/condition/RNG comparisons passed against both the preserved source and an independent regex reference. Covers both formatters, seven precision requests, random signed values, rounding boundaries, missing/infinite/extreme values, invalid inputs, and custom classes producing vector/empty/NA formatting results.
- Existing correlation table validator: 72 exact comparisons passed.
- Existing full correlation validator: 44 result/condition/RNG comparisons passed against the preserved utilities baseline.
- General and latent correlation screen markup, HTML bytes and Excel sheet names/cells matched exactly.

## Timing

Bundled R 4.5.3, preferences loaded, five alternating rounds, median elapsed seconds. The compared `format_decimal3()` closures use `.GlobalEnv`; the same application helper slot is switched between calls. These measurements isolate `format_decimal3()`; the parallel change to `format_decimal2()` is validated but not separately benchmarked.

| Decimal places | Workload | Before | After |
| ---: | --- | ---: | ---: |
| 2 | Format 10,000 signed values | 0.10 | 0.08 |
| 2 | Pearson, 300 rows / 100 variables | 0.70 | 0.65 |
| 5 | Format 10,000 signed values | 0.11 | 0.06 |
| 5 | Pearson, 300 rows / 100 variables | 0.71 | 0.65 |

Precision-label correction: the benchmark requested setting 6, which `normalize_output_decimal_digits()` clamps to 5. The table shows actual displayed precision; the recorded elapsed times and output-equivalence checks are unchanged.

The full-analysis fixtures improved approximately 7–8%. They include analysis preparation and table/matrix assembly, excluding startup, UI rendering and export. Short helper timings are sensitive to timer resolution. Validation/export workloads ran after timing completed.

Artifacts and reproduction: `output/decimal-prefix-20260913/`. Existing presentation content is unchanged. No installer was rebuilt.
