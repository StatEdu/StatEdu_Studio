# Rank, McNemar and regression formula explanations — 2026-09-18

Ten exact-source descriptions now use eight-language dictionary entries: rank-biserial and paired rank-biserial correlation, epsilon squared, Kendall W, three matched-pair/McNemar conversions, incremental f2, logistic OR-to-d and moderation f2. Existing mathematical tokens and numeric calculation objects remain unchanged. Some method notes and mathematical variable names remain English; this is not full result-text translation completion.

Dictionary script: `scripts/fill_rank_regression_formula_i18n.py`, integrated by the shared dictionary owner. Earlier interrupted work was resumed; no production session was restarted.

Verification:

- `validate_sample_size_result_i18n.R scripts/fixtures_rank_regression_formula_i18n.R`: ten actual calculator branches, formula source matching, token preservation in eight languages, exact 0.5/10.5 zero-cell odds-ratio correction and incremental f2 check, immutable serialized source results.
- Korean/Japanese current and accumulated captures written through all five shared export writers, with HTML/DOCX/HWPX/XLSX content comparison.
- `validate_sample_size_result_pdf.py tmp/rank-regression-formula-i18n`: normalized PDF extraction compared with captured expected cells, descriptions and references.
- `validate_sample_size_result_errors.R`: prior actual error cases and full existing numerical sample-size/effect-size regression suite passed.
- Scoped diff whitespace check passed.

Artifacts: `tmp/rank-regression-formula-i18n`. These are content/structure checks, not a fresh Word/Hancom visual review. Remaining longitudinal-model and other formula/method descriptions require further work.
