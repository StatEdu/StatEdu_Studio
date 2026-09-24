# Solution diagnostic notifications

Localized 12 inadmissibility details, three condition-number details and two warning wrappers into Japanese, Chinese, Spanish, French, German and Vietnamese. The notification function now uses explicit English/Korean translation templates and inserts already-formatted numbers and user variable names afterward.

Validation:
- `scripts/validate_solution_notifications_i18n.R`: all eight languages passed all 15 individual conditions, combined warnings and healthy-result silence. Tests preserve user names containing commas, Korean, `<&>` and `%s`, decimal/scientific formatting, severity and duration.
- Before/after comparison against the saved original function passed exact English/Korean equality for 17 scenarios per language.
- `scripts/validate_multilingual_coverage.R`: six additional languages passed.

Logs: `tmp/solution-notifications-validation.log`, `tmp/solution-notifications-compatibility.log`, `tmp/solution-notifications-coverage.log`.

Tests exercise notification functions with supplied diagnostic results. They do not fit models or verify browser rendering. Only transient notifications changed; result tables, captured snapshots and export writers are unchanged, so five-format export checks were not repeated. No installer was built.

The solution-diagnostic items in the covariance notification inventory are now covered. Generic selected-path mismatch fallback and other unreviewed runtime branches remain; this is not a claim of overall multilingual completion.
