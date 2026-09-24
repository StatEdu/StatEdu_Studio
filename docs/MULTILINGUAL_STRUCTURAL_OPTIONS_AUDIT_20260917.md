# Structural analysis option audit

Audited `structural_analysis_options_panel` for CFA, CB-SEM, SEM and PLS-SEM in all eight supported languages. No additional translation/runtime changes were needed in this scope.

`scripts/validate_structural_options_languages.R` passes 32 generated-UI combinations. It captures 146 translation sources at runtime, including parameterized helper labels missed by the initial 141-literal source scan, and verifies a nonempty catalog entry for every source in all six additional languages.

It compares input IDs, types, values, defaults, bounds, steps and select values against English. Restoration checks preserve the selected grouping variable and its literal user label; supported common-method controls retain checkbox state, free-text rationale and marker names. PLS correctly omits the common-method controls.

Identical English spellings are confirmed dictionary entries: Bootstrap (es/fr/de/vi), Estimation, Percentile, Diagnostics and Strict (.85) (fr). They are not fallback omissions.

Evidence: `tmp/structural-options-languages.log`. This is generated-markup and settings-restoration validation, not live browser navigation or a reproduction of the historical language-switch/case-selection bug. No analysis output changed, so exports were not rerun. No installer was built. Findings apply only to this option panel; other result modules still need audit.
