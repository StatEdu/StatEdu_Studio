# Research versus exploratory model comparison — 2026-09-17

Updated `structural_canvas_fit_difference_result_ui` and added an exact-match display translator in `R/setup_custom_model_canvas_structural_render_fit_core.R`.

- Localized heading, same-sample exploratory warning, suppression template, known comparison-ineligibility reasons and ordinary/observed robust test-method descriptions.
- The robust translation covers the actual lavaan Satorra–Bentler 2001 method heading and its caution about standard versus robust statistics; the method identifier remains unchanged.
- English method text remains untouched. Unrecognized method text, error messages and model-specific inadmissibility details remain literal. The general inadmissibility prefix is translated without interpreting or replacing raw details.
- Numerical comparison, eligibility and source report generation are unchanged.

Validation: `scripts/validate_fit_difference_i18n.R` fits actual nested ML and MLR CFA models with lavaan and checks headings, both notes, unchanged Δχ²/Δdf/p display and source reports across eight languages. Additional fixtures exercise 13 suppression reasons, including dynamic/raw text with Korean, `<&>` and `%s`. The unchanged producer still determines eligibility. This does not claim coverage of every lavaan estimator-specific heading or interactive desktop behavior.

Japanese current/accumulated snapshots in `tmp/fit-difference-i18n` passed HTML/PDF/Word/HWPX/Excel content checks, including PDF text and localized cover. Table order and Excel sheet counts passed (15 current, 16 accumulated). Suppression-only notes are paired with a representative main table; successful comparisons use their actual numeric tables.

No installer was built and no installed application was changed. The overall multilingual audit continues.
