# PLS validity guide localization — 2026-09-17

Updated the PLS discriminant-validity supplementary panel in `R/setup_custom_model_canvas_structural_render_validity.R`.

- Localized the heading, explanatory note, metadata headers, construct types, measurement modes, four evidence-role explanations and Fornell–Larcker decisions in Korean and six additional UI languages.
- Localized metadata before compacting identical columns into the shared-specification note. The shared note uses a complete translated template.
- Translate only known program metadata, preserving construct names, numeric precision, technical symbols and unknown values. Protect rendered cells from subsequent generic translation.
- Main HTMT matrix remains English. No statistical calculation, installer or installed application was changed.

Validation:

- `scripts/validate_pls_validity_guide_i18n.R`: English, Korean, Japanese, Chinese, Spanish, French, German and Vietnamese; mixed metadata, identical metadata, empty optional columns and empty table. Tests all four evidence roles, status values, literal user names (`Review`, `Normality`, `Primary`, Korean plus `<&> %s`), numeric strings and language-invariant main output.
- Tests use table fixtures passed through the actual guide-table selector and render expression; they do not refit a PLS model or constitute an interactive application walkthrough.
- Japanese snapshot fixtures in `tmp/pls-validity-guide-i18n`: current and accumulated HTML, PDF, Word, HWPX and Excel passed content checks. PDF text and localized cover verified. HTML/Word table order and Excel sheet counts passed (current 6, accumulated 8).
- Prior PLS bootstrap validity-note localization test passed again.

This completes this panel only; it is not a claim that every analysis and multilingual branch has been verified.
