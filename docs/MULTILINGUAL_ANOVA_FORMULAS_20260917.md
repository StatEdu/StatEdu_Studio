# ANOVA/ANCOVA/MANOVA formula explanations — 2026-09-17

Seven result descriptions are localized: ANOVA partial eta squared, omega squared and f conversion; ANCOVA adjusted f and partial eta squared; MANOVA Pillai and Wilks conversions. Formula variable names, operators, degrees-of-freedom definitions and reference publications retain their source content. Localization applies at rendering through the existing exact-match helper; calculation objects and numerical functions are unchanged.

Dictionary merge script: `scripts/fill_anova_formula_i18n.py` (seven keys × eight languages).

Validation commands:

- `validate_sample_size_result_i18n.R scripts/fixtures_anova_formula_i18n.R`: seven actual calculation branches, exact source-formula matching, math-token retention in eight languages, rendered prose and immutable serialized results. Korean/Japanese current and accumulated captures use the five common export writers, with content comparison for HTML/DOCX/HWPX/XLSX.
- `validate_sample_size_result_pdf.py tmp/anova-formula-i18n`: normalized extracted PDF text compared with captured expected content.
- `validate_sample_size_result_errors.R`: previous real error-message tests plus existing numeric sample-size/effect-size regression suite.
- Scoped diff whitespace check.

Artifacts: `tmp/anova-formula-i18n`. This scope covers these seven formula descriptions, not every ANOVA/ANCOVA method note or all application translation. No production restart or fresh desktop-editor visual review.
