# Proportion, chi-square and correlation method notes

Date: 2026-09-18

Nine exact-source method descriptions localized in eight languages: proportion arcsine transformation, category-based w, phi, chi-square-based w, point-biserial conversion, Pearson r, Cohen q, r from R-squared and r from F. Formulas remain unchanged. The R-squared sign-identification limitation and one-degree-of-freedom qualification are retained. Formula-only notes are outside this prose batch.

Implementation: `scripts/fill_association_method_notes_i18n.py`, merged by the shared dictionary owner; exact-source rendering lookup in `R/sample_size_ui.R`. No changes to calculations or stored results.

Verification:

- `fixtures_association_method_notes_i18n.R` runs nine actual calculator wrappers and checks exact-source method notes and eight-language formula tokens.
- Numeric references cover negative h/r/d/q, positive r reconstructed from R-squared, F-to-r conversion, phi/w and normalization of non-unit-sum category inputs.
- Shared export fixtures use method-note keys as expected displayed text. Current and accumulated Korean/Japanese content checks passed all nine results in five formats with unchanged serialized source objects. Separate PDF extracted-text checks passed: 10 current / 19 accumulated pages in each language.
- Existing numerical regression suite and three actual error translations across eight languages passed. Scoped whitespace checks passed.

Artifacts: `tmp/association-method-notes-i18n`. Automated content/structure checks, not a new browser or Word/Hancom visual review. Production sessions were not restarted. Additional ANOVA/ANCOVA, regression and other method notes remain.
