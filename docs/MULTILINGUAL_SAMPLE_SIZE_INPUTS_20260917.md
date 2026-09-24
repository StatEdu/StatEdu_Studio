# Sample-size input retention — 2026-09-17

Language changes redraw input panels. Common controls previously supplied fixed defaults, and the sample-size snapshot retained only design selectors. T-test effect-size controls also supplied fixed defaults.

## Changes

- Common sample-size controls preserve alpha, power, sample size, allocation ratio, alternative and dropout values. Defaults apply only when a value is absent, not when the user clears a text field.
- Sample-size snapshots retain isolated values belonging to the current method while preserving reactive design dependencies. Typing numeric values does not add panel-redraw dependencies.
- Sample-size t-test effect input and all t-test effect-size text inputs preserve user edits on redraw.
- Calculations and report/export content are unchanged. No dictionary changes in this batch.

## Verification

`scripts/validate_sample_size_input_retention.R` checks common controls for every registered method, both sample-size and power targets, eight languages plus Korean return. Actual Shiny reactive rendering verifies custom paired t-test values (effect 0.72, alpha 0.013, power 0.87, t 4.25, n 83) and intentionally blank fields.

The same command runs existing `validate_sample_size.R` numeric/reference and all-menu representative calculation tests. Scoped diff whitespace check passes.

This is a bounded fix: method-specific fields outside t tests and effect-size calculators outside t tests still need retention audits. Remaining explanatory translations are also pending. No fresh browser visual verification or production restart was performed.
