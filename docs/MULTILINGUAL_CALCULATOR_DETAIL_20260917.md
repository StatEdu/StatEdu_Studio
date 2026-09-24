# Remaining calculator setup captions — 2026-09-17

## Changes

- HINT-8 uses existing localized profile, constant and score captions. Coefficient values and output variable `hint8_score` remain unchanged.
- ASCVD reference coding, exclusion-table headers, medical-history caption and missing-output messages now use the selected language. Race codes 1/2/3, sex codes 1/2, exclusion values 1 and >= 190 are unchanged. Custom output names are formatted once and escaped as text by htmltools.
- Metabolic severity displays the age range as 20–59 and localizes the TG transformation caption. The formula `ln(TG)` and calculation remain unchanged.
- Five new dictionary keys across eight languages are provided by `scripts/fill_calculator_detail_i18n.py`, merged by the shared dictionary task.

## Verification

- `scripts/validate_calculator_detail_i18n.R`: PASS in ko/en/ja/zh/es/fr/de/vi. Checks dictionary coverage, exact HINT-8 coefficient strings, ASCVD coding/rules, output-name fallback and HTML escaping for custom names containing markup and percent placeholders.
- Six calculator numeric fixtures: PASS.
- `scripts/validate_calculator_status_i18n.R`: six actual server paths across eight languages and Korean return; selected inputs, custom output names and calculation values retained.
- Scoped `git diff --check`: PASS.

These are calculator setup captions, not changed analysis report snapshots or export content. No production restart or installer rebuild was performed. This scope was verified through rendered HTML and server tests; fresh browser visual inspection was not performed for this batch. Remaining application translation and common DataTables accessibility text are separate work.
