# 1.3.1-dev score editor localization (2026-09-20)

The original-item, reliability and parcel workflow now uses the selected UI language: Korean, English, Japanese, Chinese, Spanish, French, German and Vietnamese. Each catalog contains 95 score-specific keys.

## Coverage

- Canvas and sidebar shortcuts, panel controls, mean/sum and alpha/omega choices, missing-response rules, preview/apply actions.
- Loading-balanced allocation, item/model diagnostics, validation errors and changed-model messages. Invalid-loading messages preserve item names and numeric loadings.
- Score-definition and parcel-allocation result tables, explanations and numeric-workbook metadata.
- Model-picture titles for covariate inclusion/exclusion.

User variable names, labels and allocation rationales are preserved. Numeric calculations, saved model precision, mean/omega defaults and the 80% response policy are unchanged. Score-definition tables explicitly carry their translated language metadata; the shared English journal-table policy is unchanged. Long sidebar shortcut labels wrap without moving subsequent fields when the shortcut is absent.

## Validation

- `scripts/validate_canvas_scores_i18n.R --exports`: catalog and placeholder parity, translated errors/diagnostics, preserved user content; current and accumulated HTML, PDF, DOCX, HWPX and XLSX for all eight languages.
- `scripts/validate_canvas_scores_i18n_browser.cjs`: live Shiny panels and calculation previews in all eight languages; mean/omega defaults and original-item selection.
- `scripts/validate_canvas_scores_browser.cjs`: existing selection, preview, apply, parcel replacement, roundtrip and undo/redo checks.
- `scripts/validate_i18n_contract.R`: shared catalog contract.
- Core score tests include alpha/omega, missingness, variance constraints, loading-balanced allocation and model fitting. JavaScript syntax and whitespace checks pass.
- Japanese and German PDF table pages and the German panel were visually inspected. The German allocation heading was shortened to avoid clipping.

Artifacts are under ignored `tmp/canvas-score-i18n`. These checks do not constitute native-speaker editorial review. Existing unrelated application translations are outside this change. No installer was built.
