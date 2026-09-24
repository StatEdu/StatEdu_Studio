# Detailed calculator input retention — 2026-09-17

## Change

All effect-size input builders (including GEE SD inputs) and the sample-size detail input builder now reuse a scoped text-input factory. Existing values are read in `isolate`; missing values receive the original defaults. Numeric text, comma-separated vectors, intentionally blank strings and special characters survive a language-triggered redraw. The helper does not parse, round or validate user text and does not change calculation functions. It preserves text on other redraws as well.

The earlier common-control and sample-size snapshot fixes remain in place. This batch adds no translations or report/export changes.

## Verification

- `scripts/validate_sample_size_detail_retention.R`: 213 rendered configurations, 1,095 text-field occurrences across eight languages passed. Cases cover registered effect-size methods and exposed design choices, detail selectors and both sample-size/power targets. This is not an exhaustive Cartesian product of all nested choices.
- The same script runs the existing sample-size numerical reference, effect-size wrapper, representative all-menu and achieved-power tests.
- Earlier actual Shiny reactive input-retention tests are rerun to cover language changes without resetting custom or blank t-test values.
- Scoped `git diff --check` passed.

No browser visual inspection or production restart in this batch. Top-level lazy-panel design selectors and remaining explanatory text still require separate review; this document claims detailed text-field retention, not complete UI-state or translation coverage.
