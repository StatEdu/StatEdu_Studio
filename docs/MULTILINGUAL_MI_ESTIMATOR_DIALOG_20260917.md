# MI documentation and estimator recommendation dialogs

Localized MI modification dialog labels, explanation, placeholder, action buttons and applied/already-applied notifications. Localized the estimator recommendation modal, Mardia explanation, numeric summary, optional Bollen–Stine note and ML/MLR action labels. Uses the existing dictionary lookup, without translating submitted justification or path names.

Validation: `scripts/validate_mi_estimator_dialog_i18n.R` passed in all eight languages using actual UI expressions/functions. It covers MI skipped-detail presence/absence, paths containing Korean and special characters, warning/success states, estimator modal with/without the Bollen note, four diagnostic values and stable input IDs. Common multilingual coverage and the previous Heywood-dialog regression test passed.

Logs: `tmp/mi-estimator-dialog-validation.log`, `tmp/mi-estimator-dialog-coverage.log`, `tmp/mi-estimator-heywood-regression.log`.

This validates generated markup and messages, not interactive browser actions or analysis refits. Analysis settings, submitted justification, calculations and result snapshots are unchanged. Only interaction UI changed, so five-format export checks were not repeated. No installer was built.

Remaining: the separate inline method-guidance panel (candidate reasons and limitations), and MI skipped-candidate reason strings supplied by the engine. This change preserves the latter verbatim; it does not establish complete localization of those reasons or the entire application.
