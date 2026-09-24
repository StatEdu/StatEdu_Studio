# Gwet row-access preparation — 2026-09-13

`interrater_gwet_ac()` now converts homogeneous plain data-frame columns to a matrix once before its row loop. This avoids repeated data-frame row extraction. The fast path requires the exact `data.frame` class, identical column storage types (character/integer/double/logical), and no classed or dimensional columns. Mixed types, factors, dates, custom classes and existing matrices retain their original access path.

Per-row conversion, missing-value filtering, category matching, pair order, weights and accumulation order are unchanged. This introduces a temporary matrix proportional to the input size; peak memory was not measured.

## Validation

- `scripts/validate_gwet_pair_reuse.R` against the actual pre-change source: 120 exact coefficient/condition/RNG comparisons passed.
- `scripts/validate_gwet_row_access.R`: 48 exact comparisons against both the actual baseline and original extraction path. Covers integer/double/character/logical columns, factors, mixed types, dates, matrices, empty/single-row inputs, extreme numeric scales, AC1/AC2 and linear/quadratic weights.
- Full nominal/ordinal result objects and RNG matched. Screen HTML, saved HTML bytes and Excel sheet names/cell values matched.
- `scripts/validate_interrater.R`: existing numerical reference checks passed.
- `git diff --check` passed.

## Timing

Bundled R 4.5.3; 10,000 rows, ten raters, five categories plus missing ratings. Five alternating before/after runs, median complete Gwet-helper seconds:

| Coefficient | Before | After |
| --- | ---: | ---: |
| AC1 | 0.49 | 0.18 |
| AC2, quadratic | 0.50 | 0.17 |

These fixture-specific reductions are about 63% and 66%. They are helper timings, not total inter-rater analysis or startup timings. Inputs outside the guarded fast path are not expected to gain from this change.

Artifacts and baseline: `output/gwet-row-access-20260913/`. No displayed/report content changed. No installer was rebuilt.
