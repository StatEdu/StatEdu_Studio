# Word and native HWPX table formatting

The user-edited `tmp/user-history-hwpx-benchmark/all.hwpx` is the visual reference. The subsequent screenshot explicitly adds a thin solid line between a spanning 95% CI header and its LLCI/ULCI subheaders.

- Table text: Arial, 9 pt, regular weight throughout; superscript markers retained.
- All four cell margins: 0.2 mm (56 HWP units; nearest Word value 11 twips).
- Top/bottom table rules: 0.5 mm dark gray; header/group and F-summary separators: 0.1 mm black. Word rounds to its supported border units.
- No routine body-row or vertical rules. CI separators cover only the associated spanned columns.
- External titles, notes, snapshot content, merged cells, orientation, and post-table gaps retain their existing behavior.

The shared rules live in `result_document_model.R` and are consumed by the Word table builder and native HWPX writer. Word margins are serialized explicitly because flextable otherwise truncates this sub-point padding to zero.

Validation: `validate_native_hwpx.R` passes current/accumulated content and merged-grid checks, margins, regular font, and the CI separator in a three-level header. `validate_analysis_note_exports.R` passes current and accumulated HTML/PDF/Word/HWPX/Excel preservation checks (28 analysis fixtures and 38 accumulated notes).

Visual Word rendering was attempted with the documents skill renderer but is unavailable because LibreOffice is not installed. No installer was rebuilt.
