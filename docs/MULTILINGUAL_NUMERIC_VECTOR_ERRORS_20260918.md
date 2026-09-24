# Numeric-list validation messages

Date: 2026-09-18

Four existing numeric-vector error messages now display in eight languages, explicitly identifying Group 1 means, Group 2 means, unstructured correlations, or unstructured working correlations. The example `0, 0.2, 0.5` is preserved. Exact lookup in `R/sample_size_ui.R` leaves unknown field names untouched; parser rules and raw error objects are unchanged. Shared dictionary owner applied `scripts/fill_numeric_vector_errors_i18n.py`.

`scripts/fixtures_numeric_vector_errors_i18n.R` checks 20 failures across eight languages: empty input, text, Inf and NaN for four subjects, plus four real LMM/GEE calculation paths. Five valid input forms (comma, semicolon, space, tab/newline and numeric array) pass. Existing seven matrix-error cases, matrix references, three real LMM outputs and RNG-state preservation also pass.

Korean/Japanese current and accumulated HTML, PDF, DOCX, native HWPX and XLSX valid-result content checks passed (PDF 4/7 pages). Existing numerical regression/validation tests and scoped whitespace checks passed. Errors remain transient UI warnings. Artifacts: `tmp/numeric-vector-errors-i18n`.

Automated content/structure verification only; no fresh browser/editor visual inspection, production restart or installer rebuild. Additional validation messages remain untranslated.
