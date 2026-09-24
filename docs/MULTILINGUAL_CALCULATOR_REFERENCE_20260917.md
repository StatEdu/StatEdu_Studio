# Calculator reference labels: 2026-09-17

## Implemented

- EQ-5D: localized 26 country names, reference-table captions and profile checkbox label in ko/en/ja/zh/es/fr/de/vi. Original country codes, value-set provenance, coefficients and scoring functions are retained.
- EQ-5D: preserve the active options tab when language or settings redraw the setup panel.
- Metabolic syndrome: localized preset names, cutoff descriptions, diagnosis rules, coding/output captions and custom numeric-input labels. Existing thresholds, units and calculation rules are retained.
- Dictionary additions are supplied by `scripts/fill_calculator_reference_i18n.py` and integrated by the parallel translation task.

## Validation

- `scripts/validate_calculator_reference_i18n.R`: PASS. 29 EQ-5D type/country combinations × 8 languages; original codes, selected tab, unchecked profile, numeric reference cells and scores preserved. Five metabolic presets × 8 languages; diagnostic text and cutoff values checked, including custom values 91.25 and 81.5.
- `scripts/validate_calculator_status_i18n.R`: PASS after these changes; six calculator server paths, language changes and numeric fixtures.
- `scripts/validate_calculator_errors_i18n.R`: PASS; 11 actual error paths, six notification handlers, eight languages plus Korean return.
- Isolated synthetic-data browser on port 7903: Canada selected, profile checkbox unchecked and value-table tab open; switching English to Japanese preserved all three and translated slope/intercept captions. Metabolic custom waist cutoffs 91.25/81.5 survived Japanese-to-Korean switching, with translated labels and coding/output captions.
- Scoped `git diff --check`: PASS.

## Limits

This completes the reference-label scope, not all application translation. Browser checks are representative; the full country/preset matrix was checked by R tests. This changes calculator setup/status UI, not analysis report snapshots or file exports. Production port 7894 was not restarted. HINT-8 profile wording, remaining calculator descriptions and common DataTables sorting accessibility text remain follow-up candidates.
