# Longitudinal and structural supplementary localization — 2026-09-16

Source-only update; no installer was built or installed.

## Changes

- Added 56 catalog phrases in all eight supported languages: longitudinal supplementary headings, model/data/missingness/weight labels, and structural multi-group supplementary headings and resampling headers.
- Routed structural invariance supplementary section/table headings through the locale catalog. Equality-constraint score-test headings translate the template before inserting the original stage name.
- Protected Group, Group 1, Group 2, paths and other user identifiers before the invariance formatter translates diagnostic statuses. Explicit column protection survives subsequent shared table localization.
- Added Group 1 / Group 2 to the shared identity-column protection.
- Fixed a discovered Korean longitudinal rendering failure: an NA supplementary cell reached `startsWith()` inside an `if` condition. Missing text now returns unchanged.
- Main publication table generation remains English. User names and labels remain unchanged.

## Verification

- `validate_longitudinal_structural_i18n.R` fits a Gaussian GEE and compares main-table cells, headings and notes across all eight UI languages. It also renders seven PLS-MICOM/MGA supplementary sections with deliberately conflicting group labels (`Passed`, `Adequate`, `Failed`, Korean text), checks their preservation, checks CFA stage-heading interpolation and missing supplementary text.
- `validate_multilingual_table_roles.R` checks the existing eight representative analyses and 16 appendix localizers in all eight languages.
- `validate_i18n_contract.R` passes with UTF-8 locale enabled.
- Changed supplementary fixture exported in current and accumulated modes to HTML, PDF, Word, HWPX and Excel. Content checks and actual PDF text checks pass.
- Targeted `git diff --check` passes.

## Limits

This is not a claim of complete multilingual coverage. Longitudinal diagnostic prose, automatically generated manuscript suggestions, missing-data strategy details, and some structural invariance explanations still need review. PLS supplementary rendering was tested with representative result-shaped fixtures, not a new full PLS resampling fit. Interactive menus and all model/error branches were not exhaustively exercised in this change.
