# Proportion/correlation formula prose — 2026-09-17

Six exact-source result descriptions now translate risk difference, risk ratio, odds ratio with zero-cell correction, point-biserial correlation, r from F and r from R-squared. Mathematical expressions and referenced publications are retained. Four chi-square effect-size formulas remain unchanged. This does not translate every method note or result-table label.

Dictionary additions: `scripts/fill_effect_size_formula_i18n.py`, merged by the shared dictionary owner. Calculation and export code is unchanged; localization occurs through `sample_size_result_text` at rendering.

Validation uses `validate_sample_size_result_i18n.R scripts/fixtures_effect_size_formula_i18n.R` and `validate_sample_size_result_pdf.py tmp/effect-size-formula-i18n`:

- Actual calculator wrappers produce six result fixtures, including a zero cell in the odds-ratio table. Its corrected odds ratio matches `(0.5/50.5)/(10.5/40.5)`.
- Eight-language rendered formulas retain mathematical expressions and serialized result objects. Four chi-square formulas are unchanged in all languages.
- Shared note formatting may reorder definition sentences; checks verify every formatted fragment rather than requiring the original prose order.
- Korean/Japanese current and accumulated captures are checked through common HTML/DOCX/HWPX/XLSX/PDF writers. Expected table cells, formulas and references are compared, with Unicode-normalized PDF extraction.
- Existing numerical sample-size/effect-size regression suite and previous real validation-error tests passed. Scoped diff whitespace check passed.

Artifacts: `tmp/effect-size-formula-i18n`. No production restart or fresh Word/Hancom visual review. Broader formula/method-note coverage remains follow-up work.
