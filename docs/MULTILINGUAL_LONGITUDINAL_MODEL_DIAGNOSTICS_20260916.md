# Longitudinal model diagnostics — 2026-09-16

Source/catalog update only; installer not built or installed.

Added 20 Japanese, Chinese, Spanish, French, German and Vietnamese translations for existing longitudinal supplementary messages. Topics include convergence and singular fits, random-effect normality, fixed/random-effects assumptions, Pearson dispersion, residual heteroskedasticity, panel serial correlation, lag-1 correlation and GEE correlation-structure comparisons. The existing localization path consumes these catalog entries; analysis calculations are unchanged.

Expanded `validate_longitudinal_structural_i18n.R` to fit Gaussian GEE, LMM and panel fixed-effects models and compare publication-table content across all eight languages. All 20 exact diagnostic messages are exercised in supplementary tables while identical user-variable labels remain untouched. The test fixture is stored in `scripts/fixtures/longitudinal_i18n_model_diagnostics.json`.

The simulated LMM emitted a singular-fit diagnostic; it still returned a model and the rendered output is included in the language comparison. This check verifies rendering/language invariants, not statistical suitability of the simulated model.

Current and accumulated HTML/PDF/Word/HWPX/Excel exports include the new diagnostic fixture; content preservation, actual PDF text, catalog contract and targeted whitespace checks pass.

Remaining work: other model-specific recommendations and unavailable-test messages, combined manuscript paragraphs, weighted summaries and other analysis families. No whole-application multilingual completion claim.
