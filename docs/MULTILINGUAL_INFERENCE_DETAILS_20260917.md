# Inference and bootstrap details — 2026-09-17

Updated the `_result_effect_inference_details` supplementary output in `R/setup_custom_model_canvas_structural_render.R`.

- Uses three complete localized headings for structural paths, specific indirect effects and combined direct/indirect/total effects.
- Explicitly passes the UI language to the final table renderer and protects already-localized cells from repeated translation.
- Added translations for B/beta CI-source headers and five known BH hypothesis-family labels. Unknown family names are preserved instead of generic dictionary translation.
- User paths, predictors, outcomes, counts and original reporting tables remain unchanged. Tables retain landscape orientation and existing conditional visibility; main renderers and estimators were not changed.

`scripts/validate_inference_details_i18n.R` passes all eight languages for all three sections, partial sections, nonapplicable PLS branch, translated headers/families, dynamic source descriptions and literal user strings including Korean, `<&>` and `%s`. The test uses the actual render expression with reporting fixtures. SEM structural reporting and dynamic inference regressions also pass.

Japanese current/accumulated snapshots in `tmp/inference-details-i18n` passed HTML/PDF/Word/HWPX/Excel content checks, PDF text and localized cover checks, and HTML/Word ordering plus Excel counts (3 current, 6 accumulated). This is renderer/export validation, not an interactive desktop walkthrough.

No installer was built or installed application modified. The wider audit continues.
