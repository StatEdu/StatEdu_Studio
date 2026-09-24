# Identification and power preflight panel — 2026-09-17

Updated `structural_canvas_identification_result_ui` in `R/setup_custom_model_canvas_structural_render_fit_core.R`.

- Localized heading, inventory labels, scaling description, unavailable/unrecorded values and three explanatory notes.
- Power-basis codes now use the existing option labels; missing-detail text is translated. User-written details are appended without translation, including text that matches UI vocabulary.
- The result issue table now calls the existing code-aware identification-message translator, including cross-loading messages, and translates severity and headers. Element names, diagnostic codes and unknown messages remain literal; protected cell attributes prevent a second generic translation.
- Counts, fitted model df/free parameters, diagnostic generation and main analysis results are unchanged.

`scripts/validate_identification_panel_i18n.R` passed 112 panel cases (seven power-basis cases, details absent/present, eight languages), alongside the existing 13-code diagnostic test. It checks a fitted lavaan model’s df/free parameters, structural-path counting excluding covariance/higher-order edges, all inventory labels/notes, authored text, all static issue messages and cross-loading factor-name preservation. The panel uses fixture snapshots and diagnostic rows; this is not an installed-app walkthrough.

Japanese snapshots in `tmp/identification-panel-i18n` passed current/accumulated HTML/PDF/Word/HWPX/Excel content checks, PDF text and localized cover checks, and HTML/Word table order plus Excel sheet counts (9 current, 10 accumulated). Final exports were regenerated after adding the Element header translation.

No installer was built or installed application modified. The broader multilingual audit continues.
