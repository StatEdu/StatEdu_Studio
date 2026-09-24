# PLS effect bootstrap supplement — 2026-09-17

Updated the PLS `_result_fit_bootstrap` renderer in `R/setup_custom_model_canvas_structural_render_fit.R`.

- Section headings for specific indirect, total indirect and total effects now follow the UI language.
- Translated pending, failed, canceled, insufficient and fallback inference notes, bootstrap-not-requested explanation, status/count summary, retained non-positive-definite PLSc count and inference-method explanation.
- Dynamic explanations use complete format strings, preserving all counts, ratios and user-provided failure details. Known statuses are translated; unknown status strings remain literal.
- Supplementary tables receive an explicit UI language and protected data cells. The BH-adjusted p header is translated. Standard statistical symbols remain intact. Main-result rendering and numerical estimation were not changed.
- The retained-PLSc explanation is a separate note paragraph, following the count summary, with its existing condition preserved.

Validation:

- `scripts/validate_pls_effect_bootstrap_i18n.R`: eight states × eight languages; actual render expression with fixture result tables and bootstrap metadata. Checked all three effect sections, notes, warning classes, no-inference column omission, empty/no-applicable-effect output, numeric precision, Korean/special-character path names and raw failure messages containing translation vocabulary and `%s`.
- This validation does not rerun PLS estimation or provide interactive desktop coverage.
- Japanese snapshots in `tmp/pls-effect-bootstrap-i18n` passed current and accumulated HTML/PDF/Word/HWPX/Excel content verification. PDF text and localized cover passed. Table order and Excel sheet counts passed: 24 current, 27 accumulated.
- Final export checks were rerun after adding the BH-adjusted p translation.

No installer was built and the installed application was not modified. PLSpredict and other unreviewed panels remain in the wider multilingual audit.
