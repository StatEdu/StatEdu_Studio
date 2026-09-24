# Heywood dialog and completion notifications

Localized the Heywood-constrained reanalysis dialog, explanatory text, percentage label, run button and completion notification. Also localized the shared structural-analysis completion message and required-package notification. The translation closure resolves the current UI language at invocation time.

`scripts/validate_heywood_dialog_i18n.R` passed for all eight languages using actual modal/message expressions from the event registration function. It checks generated HTML, input default 0.1, bounds 0.01–5, step 0.01, confirm input ID, package name, literal variable names, percent formatting, warning severity and duration. This is markup/expression validation, not an interactive browser or constrained-refit test. `scripts/validate_multilingual_coverage.R` passed for the six additional languages.

Logs: `tmp/heywood-dialog-validation.log`, `tmp/heywood-dialog-coverage.log`.

Only interaction UI and transient messages changed. Analysis settings, computations and captured output snapshots are unchanged; five-format output export tests were not repeated. No installer was built.

Remaining identified work in these event modules: the MI modification documentation dialog and related messages; estimator guidance and recommendation dialogs. Overall multilingual coverage is not yet complete.
