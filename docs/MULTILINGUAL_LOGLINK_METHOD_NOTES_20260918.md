# Log-link rate and mean effect descriptions

Date: 2026-09-18

Added two exact method descriptions in eight languages: incidence rate ratio for Poisson/negative-binomial regression and mean ratio for Gamma regression. The exp(beta) and log-ratio identities and the log-link model qualification are preserved. Numerical calculations and journal-facing table cells are unchanged.

Implementation: `R/sample_size_ui.R` exact-source lookup; `scripts/fill_loglink_method_notes_i18n.py` merged through the shared dictionary owner.

Validation:

- `scripts/fixtures_loglink_method_notes_i18n.R`: twelve actual UI-wrapper calculations crossing three model types, ratio/log-ratio input, and ratios .5/2.
- Checked primary and model-specific ratios and log coefficients against independent log/exp references. Both negative and positive coefficients, existing six neutral-effect input rejections, English identity, unknown-text preservation, and unchanged result serialization passed.
- Both descriptions rendered correctly in all eight languages.
- Korean/Japanese current and accumulated HTML, DOCX, native HWPX, and XLSX content verification passed. PDF extracted text passed: 13 current and 25 accumulated pages for each language.
- Existing sample-size numerical regression checks, three actual validation errors across eight languages, and scoped whitespace checks passed.

Artifacts: `tmp/loglink-method-notes-i18n`. Automated content/structure verification only; no fresh browser or Word/Hancom visual inspection. No user server restart or installer rebuild. Whole-application translation completeness has not been established.
