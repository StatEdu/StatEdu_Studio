# PLS measurement bootstrap supplement — 2026-09-17

Updated the PLS branch of `_result_measurement_ci` in `R/setup_custom_model_canvas_structural_render.R`.

- Localized numbered title, inference-unavailable explanation, successful-inference note, construct-type/mode cells and loading/weight header prefixes. BH-adjusted p headers also follow the UI language. Statistical abbreviations and CI grouping remain handled by the shared renderer.
- Known bootstrap states are translated; unknown states and raw failure details remain literal. User construct/indicator names and numeric strings are protected from generic cell translation.
- The unavailable explanation now reports status, valid/requested count and reporting threshold separately. It no longer asserts that every unavailable state (including failed/canceled) necessarily represents a below-threshold count.
- No changes to main measurement tables, estimation, installer or installed application.

`scripts/validate_pls_measurement_bootstrap_i18n.R` exercises the actual render expression and measurement-bootstrap table selector with fixtures across eight languages. Cases cover adequate, insufficient, pending, failed, canceled, unrecorded, external state, no request and empty table. Assertions cover titles, counts, threshold, warning class, optional table display, translated metadata/header prefixes, special-character names, literal failure details, decimal precision and the em dash.

Japanese export snapshots are in `tmp/pls-measurement-bootstrap-i18n`. For unavailable states with no supplementary table, fixtures pair the actual note with a representative English main table so all five export formats can validate the note. Tests do not rerun a PLS fit or constitute an interactive installed-app walkthrough.

Current and accumulated HTML/PDF/Word/HWPX/Excel content checks passed, including PDF text and localized cover. HTML/Word table order and Excel sheet counts passed (7 current, 8 accumulated).

The wider multilingual audit is still in progress.
