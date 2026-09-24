# Generated manuscript paragraph localization — 2026-09-16

Source-only update; no installer built or installed.

`longitudinal_manuscript_text()` now records the original component messages alongside the unchanged English paragraph text. `longitudinal_appendix_table()` localizes these components before joining them, instead of attempting to identify sentence boundaries from punctuation in the combined text. The metadata is used only if the current paragraph text still matches its recorded source, preventing stale metadata from overwriting an edited paragraph. No model is refitted for export.

Added 10 catalog sources with six foreign-language translations for missing-data methods, LMM and panel model rationale, MAR exclusions, and unavailable assumption/sensitivity analysis messages. Existing Korean handling remains in place.

Validation:

- Actual GEE, LMM and panel-FE results in all eight languages retain English main-table cells, titles and notes.
- Generated manuscript paragraphs equal the ordered translations of their component messages. The three tested Methods paragraphs no longer retain the tested English rationale/sample/exclusion/weight/MI sentences.
- A modified paragraph containing a dotted version string remains unchanged despite stale component metadata.
- Existing original-label, diagnostic, numeric, MI suffix and structural appendix checks pass.
- Current/accumulated five-format export fixture includes the actual generated manuscript sections as well as diagnostic probes. Catalog and targeted whitespace checks pass.

Limitations: earlier saved results without the new component metadata use the existing text fallback; they are not silently regenerated. Some individual Sensitivity and Assumptions messages and other model branches still lack translations, even though paragraph boundaries are now retained. This is not a claim of full multilingual completion.
