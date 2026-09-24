# PLSpredict supplementary display — 2026-09-17

Updated `structural_canvas_pls_predict_result_ui` in `R/setup_custom_model_canvas_structural_render_fit.R`.

- Localized the panel title, indicator/construct section headings, descriptive column labels, four comparison assessments and full cross-validation explanation.
- The explanation retains fold count, repetition count and seed through a complete translated format string. It defines mean difference, split variability and the proportion of repetitions favoring PLS, and retains the single-repetition limitation.
- Indicator and error-metric columns have distinct labels. Standard metric names and statistical symbols are preserved.
- Only known program assessment cells are translated. User indicator/construct names and formatted values are protected from generic cell translation. Main analysis tables and estimation code are unchanged.

Validation: `scripts/validate_pls_predict_i18n.R` covers repeated/single repetitions, item-only/construct-only results, empty tables and missing predictions across English, Korean, Japanese, Chinese, Spanish, French, German and Vietnamese. It exercises the actual table producer and renderer using prediction fixtures, including lower/higher/tied/unavailable errors and user names containing Korean, `<&>`, `%s` and translation vocabulary. It verifies three-decimal display values and unchanged source tables. This is not a fresh PLS estimation or interactive desktop walkthrough.

Japanese fixtures in `tmp/pls-predict-i18n` passed current/accumulated HTML, PDF, Word, HWPX and Excel content validation. PDF text and localized cover passed, as did HTML/Word table order and Excel sheet counts (6 current, 8 accumulated). Final validation was rerun after the Assessment and Error metric header correction.

No installer was built and no installed application was modified. Completion is limited to this panel; the wider multilingual audit continues.
