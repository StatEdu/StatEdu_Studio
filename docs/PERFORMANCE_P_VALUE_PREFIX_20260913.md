# Simplify p-value leading-zero removal — 2026-09-13

The shared `format_p()` helper now checks whether its `sprintf()` result starts with `0.` and removes the first character directly for APA formatting. Previously it invoked a regular-expression substitution for this fixed prefix. Parsing, the `<.001` threshold, rounding and the leading-zero option are unchanged. This does not change numerical analysis.

## Validation

- `scripts/validate_p_value_prefix.R`: all 1,068 exact string/type/condition/RNG comparisons passed against both the immediate baseline and a restored regex reference. Includes both display styles, randomized values, threshold/rounding boundaries, missing/infinite values, strings, factors, named values, lists and invalid inputs.
- Existing correlation table validator: 72 comparisons passed.
- Both p styles produced identical complete correlation results/RNG, screen markup, HTML bytes and Excel sheet names/cells. Before and after output generation used their respective formatter implementations.

## Measurements

Bundled R 4.5.3 with preferences loaded; five alternating rounds, median elapsed seconds. Both formatter closures use the same global application environment. Export validation followed timing rather than running concurrently.

| Style | Workload | Before | After |
| --- | --- | ---: | ---: |
| APA | Format 10,000 p values | 0.08 | 0.06 |
| APA | Pearson, 300 rows / 100 variables | 0.70 | 0.71 |
| Leading zero | Format 10,000 p values | 0.05 | 0.06 |
| Leading zero | Pearson, 300 rows / 100 variables | 0.70 | 0.70 |

The APA-formatting fixture improved about 25%; whole-analysis timing showed no meaningful benefit. Short formatter measurements are sensitive to timer resolution, and no leading-zero speedup is claimed. Full timings exclude startup, UI rendering and export.

Artifacts, baseline and reproduction scripts: `output/p-value-prefix-20260913/`. Only the shared p-string prefix operation changed. No displayed/report content changed. No installer was rebuilt.
