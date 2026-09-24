# Reuse correlation confidence-interval display strings — 2026-09-13

The pairwise table and CI matrix previously formatted the same two confidence limits separately. `correlation_pair_rows_and_matrices()` now passes quietly formatted, unclassed CI strings to `correlation_ci_matrix_from_pairs()` for reuse. The matrix helper's new argument is optional; direct callers retain its original formatting path. Strings are cached only within the current assembly call. Classed CI values and formatting that emits warnings/messages retain repeated formatting.

Numerical tests, confidence-limit calculations, output precision, table/matrix order and content remain unchanged.

## Validation

- `scripts/validate_correlation_ci_labels.R`: 72 exact output/diagnostic/RNG comparisons passed against both the actual baseline and a reference disabling CI reuse. Covers seven precision options, two p-value formats, empty/missing/infinite intervals, duplicate pairs and warning/message-emitting formatters.
- Existing table-construction validator: 72 exact comparisons passed. Its reference construction now traverses the R expression tree rather than depending on hard-coded statement positions.
- Entire Pearson result objects matched exactly before each benchmark.
- All 44 existing full-result/condition/RNG comparisons passed against the actual baseline. General and latent correlation screen markup, saved HTML bytes and all Excel sheet names/cells matched exactly; reproduced by `output/correlation-ci-labels-20260913/verify-display.R` with the preserved baseline passed as its argument.

## Timing

Bundled R 4.5.3 with application preferences loaded, 300 rows, continuous normal inputs, explicit Pearson, no normality test. Both compared assembly helper closures use `.GlobalEnv` and the same current nested helpers. Five alternating rounds each batch three complete analyses; median elapsed seconds per analysis. Timing was completed before the export-validation workload started.

| Variables | Before | After |
| ---: | ---: | ---: |
| 10 | 0.0033 | 0.0100 |
| 50 | 0.2033 | 0.1733 |
| 100 | 0.8433 | 0.7467 |

The hundred-variable fixture improved about 11%; fifty variables improved about 15%. The ten-variable case is short and showed no benefit; no universal acceleration is claimed. These times include analysis preparation and table/matrix assembly, excluding startup, screen rendering and export.

Baseline and reproduction scripts: `output/correlation-ci-labels-20260913/`. No displayed/report content changed. No installer was rebuilt.
