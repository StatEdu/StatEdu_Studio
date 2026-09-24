# PLS structural-effect guide — 2026-09-17

The supplementary structural-effect guide now passes the UI language explicitly to its table renderer and uses translations for its heading, Outcome/Predictor headers and f²/Inner VIF explanation. Rendering is isolated in `structural_canvas_pls_fit_guide_ui` and called from the existing Shiny output.

All cells are protected from generic translation, preserving user variable names and displayed numbers. The statistical symbols f² and Inner VIF remain unchanged. Main analysis tables and computations were not modified. No installer was built or installed application changed.

Validation:

- `scripts/validate_pls_fit_guide_i18n.R` passed for English, Korean, Japanese, Chinese, Spanish, French, German and Vietnamese.
- Table fixtures exercise the actual guide selector and renderer, including direct-effect filtering, empty-row removal, VIF-only rows and empty results. Exact displayed cell values, special characters, Korean names and names matching translation vocabulary are preserved. Spanish `Predictor` is intentionally the same spelling as English.
- Japanese snapshots in `tmp/pls-fit-guide-i18n` passed current and accumulated HTML/PDF/Word/HWPX/Excel content checks. PDF text and cover passed; HTML/Word ordering and Excel sheet counts passed (2 current, 3 accumulated).
- These are renderer and export checks, not a fresh PLS estimation or interactive desktop walkthrough.

Other PLS panels, including the structural bootstrap supplementary panel and PLSpredict explanations, still require continued inspection. This note does not certify completion of the entire multilingual audit.
