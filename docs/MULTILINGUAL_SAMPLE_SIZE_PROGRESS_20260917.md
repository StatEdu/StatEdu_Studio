# Sample-size progress localization — 2026-09-17

## Scope

Localized startup, calculating and stopped messages plus mediation Monte Carlo, mediation bootstrap, LMM, GLIMMPSE-style, stepped-wedge and approximate SEM parameter-power progress. The presentation helper is used by both rendered progress UI and asynchronous progress messages. Raw worker messages and calculation payloads remain unchanged. Unknown messages and external errors are preserved verbatim; recognized messages preserve counters, percentage values and appended sample/cluster counts.

Dictionary owner integrated `scripts/fill_sample_size_progress_i18n.py`: 10 keys in ko/en/ja/zh/es/fr/de/vi. Changes are confined to status UI and do not alter analysis result snapshots, exports or computational functions.

## Validation

`scripts/validate_sample_size_progress_i18n.R` passed:

- 10 representative progress messages × 8 languages, including numeric invariance, rendered progress text, stop button and external-error preservation.
- Existing `validate_sample_size.R` suite: exact t-test agreement with `stats::power.t.test`, closed-form references, effect-size calculations and UI wrappers, every sample-size menu with representative inputs, and achieved-power modes.
- Scoped `git diff --check` passed.

The standalone legacy validation command initially failed under the host's unsupported startup locale. Running it through the new UTF-8 locale/bootstrap wrapper passed. No production restart or browser visual QA was performed in this batch. Input-state browser checks, remaining setup descriptions and result-note localization remain follow-up work; this is not full sample-size/effect-size translation completion.
