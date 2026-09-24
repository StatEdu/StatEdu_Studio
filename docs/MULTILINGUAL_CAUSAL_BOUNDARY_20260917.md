# Causal-interpretation boundary — 2026-09-17

The structural-model causal interpretation output now uses `structural_canvas_causal_boundary_ui`. Its title, explanatory paragraph, column headers and exact known assumption/record/consequence strings follow the UI language, including Korean. Previously most table cells remained English.

The indirect-chain present/absent messages are both translated. The causal interpretation producer, path detection and numerical analysis are unchanged. Unknown text is preserved, and localized data cells are protected from subsequent generic translation.

`scripts/validate_causal_boundary_i18n.R` passed 48 cases (CB-SEM/SEM/PLS × indirect chain present/absent × eight languages). It checks every table cell, header, heading and explanatory note, source-result preservation, nonapplicable CFA and unknown text containing translation vocabulary, Korean, `<&>` and `%s`. Tests use the actual interpretation producer and renderer with graph fixtures, not an interactive installed-app walkthrough.

Japanese direct/indirect snapshots in `tmp/causal-boundary-i18n` passed current and accumulated HTML/PDF/Word/HWPX/Excel content verification. PDF text and localized cover passed, as did HTML/Word table order and Excel sheet counts (2 current, 3 accumulated).

No installer was built and the installed application was not modified. The broader multilingual audit remains in progress.
