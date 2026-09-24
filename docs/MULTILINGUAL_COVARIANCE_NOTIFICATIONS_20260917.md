# Covariance notification localization

Localized missing exogenous latent covariance warnings (CFA/SEM) and ignored covariance warnings (PLS-SEM) into Japanese, Chinese, Spanish, French, German and Vietnamese. Existing English and Korean wording remains available. Path names are inserted as literal arguments, including commas, percent signs and user labels.

Validation: `scripts/validate_covariance_notifications_i18n.R` passed for all eight languages, covering three eligible analysis types, PLS warnings, empty lists, ineligible types, warning severity and duration. These are notification-function tests with supplied path names, not model-estimation or browser tests. `scripts/validate_multilingual_coverage.R` also passed for all six additional languages.

Logs: `tmp/covariance-notifications-validation.log` and `tmp/covariance-notifications-coverage.log`.

Only transient notifications changed. Result tables, captured snapshots and export writers are unchanged; five-format export tests were not repeated. No installer was built.

## Remaining inventory in this notification module

- Solution diagnostics: 12 inadmissibility details, three condition-number details and two surrounding warning messages still have Korean/English branches.
- Generic selected-path mismatch fallback requires review beyond the already localized specific errors.
- Unknown upstream engine errors intentionally retain their original text; they must not be translated as user labels or guessed from partial matches.

This does not establish completeness across all analysis screens or runtime branches.
