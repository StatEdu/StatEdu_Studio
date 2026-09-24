# Selected-path error fallback localization

Added localized guidance for 13 exact engine error messages concerning unresolved selected paths, missing registries, inconsistent inputs and group contrasts. Three complete templates preserve dynamic column/path names for ambiguous input columns, non-estimable SEM paths and missing per-group free coefficients.

Only recognized engine messages are handled by the new branch. Unrecognized errors are preserved in the additional six languages. The pre-existing broad Korean selected-path fallback remains unchanged. English engine messages remain unchanged. Known Korean static messages keep the existing general guidance, while the three dynamic forms now retain names and provide more specific Korean guidance.

Validation:
- `scripts/validate_selected_path_fallback_i18n.R` passed all eight languages: 13 source-verified static forms, three dynamic forms containing Korean, punctuation, quotes and literal `%s`, plus five actual engine guard failures.
- `scripts/validate_selected_path_errors_i18n.R` passed the existing 12 recognized forms in eight languages.
- `scripts/validate_multilingual_coverage.R` passed six additional languages.

Logs: `tmp/selected-path-fallback-validation.log`, `tmp/selected-path-fallback-regression.log`, `tmp/selected-path-fallback-coverage.log`.

These are error-message and engine-guard tests; no full multi-group fit or browser interaction was performed. Only transient error guidance changed. Result tables and export snapshots are unchanged, so five-format export tests were not repeated. No installer was built.

The previously recorded selected-path fallback gap is covered for these verified engine messages. This does not establish complete coverage of every screen or unrecognized third-party error.
