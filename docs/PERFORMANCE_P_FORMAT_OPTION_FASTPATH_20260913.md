# Skip redundant canonical p-format normalization — 2026-09-13

`normalize_p_value_format()` now immediately returns values exactly identical to `"apa"` or `"leading_zero"`. All other inputs retain the existing case conversion, alias recognition and fallback behavior. Strict identity prevents named or classed values from bypassing normalization. Settings are still read on each call; nothing is cached across preference changes.

## Validation

- `scripts/validate_p_format_option_fastpath.R`: 24 exact option/type/condition/RNG comparisons passed against both the preserved source and a reference without the early return. Includes aliases, case variants, empty/missing values, lists, named values, factors and a diagnostic-emitting custom class.
- Existing p-string validator: all 1,068 comparisons passed against the preserved utilities baseline.
- All 44 full-result/condition/RNG comparisons passed. General and latent correlation screen markup, HTML bytes and Excel sheet names/cells matched exactly; reproduced by `output/p-format-option-fastpath-20260913/verify-display.R` with the baseline as its argument.

## Measurements

Bundled R 4.5.3, preferences loaded; both compared normalizer closures use the application global environment. Five alternating rounds, median elapsed seconds. Timing completes before validation/export workloads. Whole-analysis fixture: Pearson, 300 rows and 100 variables, excluding startup, rendering and export.

| Style | Workload | Before | After |
| --- | --- | ---: | ---: |
| APA | Format 10,000 p values | 0.07 | 0.06 |
| APA | Full Pearson | 0.60 | 0.59 |
| Leading zero | Format 10,000 p values | 0.06 | 0.04 |
| Leading zero | Full Pearson | 0.60 | 0.59 |

The full-analysis difference is small, approximately 0.01 seconds. The formatter-only benefit should not be presented as equivalent whole-analysis acceleration, and short timings are sensitive to resolution.

Artifacts and reproduction: `output/p-format-option-fastpath-20260913/`. No displayed/report content changed. No installer was rebuilt.
