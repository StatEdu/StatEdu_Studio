# Cluster, precision, reliability and SEM formula descriptions

Date: 2026-09-18

Thirteen exact-source effect-size descriptions are localized in eight languages: three cluster designs, three precision parameters, four reliability descriptions and three SEM descriptions. Mathematical expressions (including English variable names) remain unchanged. This does not complete translation of method notes or sample-size planning descriptions.

Implementation: `scripts/fill_design_formula_i18n.py`, integrated by the shared dictionary owner; rendering lookup in `R/sample_size_ui.R`.

Validation scope:

- `validate_sample_size_result_i18n.R scripts/fixtures_design_formula_i18n.R`: thirteen formula branches, eight-language token and rendered-text checks, unchanged serialized results.
- Ten branches use UI calculator wrappers. Alpha, ICC and Bland–Altman are core-only reliability fixtures; the current reliability menu exposes kappa only. No menu options were added, and these core tests do not claim UI support for the hidden variants.
- Numeric references cover cluster design adjustment, precision half-width, alpha difference, kappa agreement, Bland–Altman width, RMSEA noncentrality and SEM Fisher transformation.
- Korean/Japanese current and accumulated snapshots passed all five shared export content checks (HTML, PDF, Word, native HWPX, Excel). Separate PDF extracted-text checks passed: 14 pages current and 27 accumulated in each language.
- Existing numerical sample-size/effect-size regression tests and three actual errors across eight languages passed. Scoped whitespace check passed.

Artifacts: `tmp/design-formula-i18n`. Automated content/structure verification only; no new browser, Word or Hancom visual review. Production sessions were not restarted. Further planning formulas and method-note translations remain.
