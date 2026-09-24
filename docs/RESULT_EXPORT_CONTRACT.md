# Result snapshot contract

Every output change or addition must update and validate HTML, PDF, Word, HWPX,
and Excel in the same change. The current result and accumulated results are
equally authoritative.

- Keep every displayed heading (including h6), introduction, table cell, merged
  cell, symbol, note, explanation, image, order, and paper direction.
- Never rerun statistical estimation or substitute a different report on save.
  Conversion and pagination are permitted; dropped prose is not.
- By explicit request on 2026-09-14, PDF restores the existing localized cover.
  Word/HWPX insert one empty paragraph after each table including its notes and
  explanations, before the next heading, table or figure. These presentation
  additions must not omit or reorder captured result content.
- Capture computed display styles and column/row geometry along with the HTML.
- Word/HWPX share the captured document content. PDF/HTML reuse its HTML nodes.
- Excel keeps each displayed table with its headings and explanations on its own
  sheet. Do not move table notes to a separate notes sheet. Printing settings
  must not determine which content is exported.
- Entry delete/up/down operations act on stable IDs and persist the new list.
  An undo operation must not discard results appended since the edited state.
- Strip model editing hit targets from exported figures and hide coefficient-label
  rectangle strokes/borders. Preserve actual path lines and significance styles.

## HWPX conversion

Only the Korean accumulated Results toolbar exposes HWPX alongside Word.
Individual analysis screens do not expose HWPX buttons. The Windows
converter uses a separate hidden Hancom instance to open the same generated DOCX
and save HWPX. It stages files in a unique Windows temporary directory and uses
Hancom's normal file-access policy. It does not change registry values, install
DLLs, dismiss security dialogs, or attach to the user's open documents.
`STATEDU_HWP_SECURITY_MODULE` optionally selects an already configured module.
Hancom must be installed and permit access to the staged files. A bounded worker
reports failure if conversion stalls; cleanup identifies only its own Hancom
process by window handle and creation time. Only a structurally valid HWPX is
copied to the selected destination.

Official API and deployment conditions:
[Hancom automation guide](https://developer.hancom.com/hwpautomation).
Hancom states that commercial use of its automation requires separate approval/licensing.
Do not redistribute a third-party automation DLL without resolving its terms.

## Validation

### Local HWPX integration status (2026-09-13)

**Actual current and accumulated HWPX export passed on this host with Hancom 2024.**
The former unconditional module-registration prerequisite was incorrect here:
normal conversion of staged temporary files succeeds without a custom module.
Tests verified every title/note/explanation and table cell in order, Korean text,
an embedded PNG, and portrait/landscape/portrait sections. Hancom reopened the
HWPX and saved it back to DOCX; complete text, table values/order, image presence,
and section dimensions were checked. A three-page PDF rendered by Hancom was
visually reviewed. This does not assert pixel-identical typesetting across all
Word/Hancom versions or fonts.

`scripts/diagnose_hwpx_module.ps1` is a manual diagnostic, not an application
startup step. It must not be run automatically because it temporarily changes an
existing Hancom registry setting. Previous diagnostic settings were restored and
verified; the application converter no longer performs any registry mutation.

- `scripts/validate_result_collection_management.R`: persistence, undo, duplicate
  IDs, and Korean-only HWPX control.
- `scripts/validate_result_export_fidelity.R`: all visible text and table cells in
  order, including h6 and associated Excel notes, plus mixed paper directions.
- `scripts/validate_canvas_export_ui.cjs`: actual coefficient labels have no
  exported border/hit rectangle at 300/600 DPI in all four model editors.
- `scripts/validate_structural_screen_export_contract.R`: matrix orientation and
  additional-fit-family orientation across output formats.
- `scripts/validate_hwpx_export.R`: actual current/accumulated conversion, Korean
  text, notes, tables, inline image without a special CSS class, and orientations.
- `scripts/validate_hwpx_roundtrip.ps1`: reopen HWPX in Hancom and produce DOCX/PDF
  for comparison and visual inspection.
- `scripts/validate_hwpx_roundtrip.py`: compare complete text, table values/order,
  exact page dimensions, and unchanged image bytes with the captured DOCX.
- `scripts/validate_hwpx_controls.R`: no individual-analysis HWPX controls and
  unchanged snapshot payload passed to the writer.
- HWPX integration requires a real conversion, HWPX ZIP/XML inspection, and a
  Hancom round-trip check. Do not report it as validated when Hancom is unavailable.

### Regression publication update (2026-09-13, after installer build)

- Standard hierarchical regression now shares the mediation/moderation model-table
  renderer. Regression-family cells share publication styling. Conditional effects
  wrap Path/Moderator with explicit column proportions; HWPX controls appear only
  in Korean accumulated Results.
- `validate_regression_publication_style.R` and `.cjs` passed: actual three-model
  hierarchy, computed cell-style equality, real conditional-table preprocessing,
  long-label bounds, and Korean/English Results controls.
- `validate_regression_publication_exports.R` and `.py` passed for current and
  accumulated HTML/PDF/Word/HWPX/Excel: headings, subtitles, table cells, notes,
  conditional column proportions and landscape sections. Hancom reopened the
  accumulated HWPX; complete DOCX text/table order and page dimensions matched.
  The rendered conditional-effects page was visually inspected. The first
  accumulated Hancom conversion timed out; the complete retry passed.
- Regression notes, optional-note visibility, mediation notes, generalized
  regression, HWPX controls and the common result-table contract passed.
- These changes are newer than the installer recorded in
  `docs/BUILD_1_3_0_20260913.md`; that installer has not been rebuilt for this update.
