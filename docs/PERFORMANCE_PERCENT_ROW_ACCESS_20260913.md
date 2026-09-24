# Percent agreement row access — 2026-09-13

Percent agreement now uses the same guarded matrix preparation as Gwet AC1/AC2. The type checks were extracted into `interrater_row_frame()` so they are shared rather than duplicated. Only exact data frames with homogeneous, unclassed, nondimensional character/integer/double/logical columns are converted. Mixed types, factors, custom classes and existing matrices retain their original path.

Valid-value filtering, comparison pairs, pair counts and accumulation order are unchanged. Matrix preparation uses additional temporary memory proportional to the frame size; peak memory was not measured.

## Validation

- `scripts/validate_percent_agreement_pairs.R` against the actual baseline: 35 exact agreement/pair-count/condition/RNG comparisons passed.
- `scripts/validate_gwet_row_access.R` now covers both consumers: 60 exact comparisons passed against the actual baseline and original extraction paths. Covers homogeneous/mixed types, factors, dates, matrices, empty/single-row input and extreme numeric scales.
- Full nominal/ordinal prepared results and RNG matched. Screen HTML, saved HTML bytes and Excel sheet names/cell data matched.
- `scripts/validate_interrater.R`: existing numerical reference checks passed.
- `git diff --check` passed.

## Timing

Bundled R 4.5.3; 10,000 rows and ten raters with five categories plus missing ratings. Five alternating before/after runs yielded median complete percent-agreement helper times of **0.48 seconds before and 0.14 seconds after**, about 71% lower. These are helper timings, not whole-analysis or startup timings. The Gwet change in this pass only extracts its existing preparation into the shared helper.

Artifacts and baseline: `output/percent-row-access-20260913/`. No displayed/report content changed. No installer was rebuilt.
