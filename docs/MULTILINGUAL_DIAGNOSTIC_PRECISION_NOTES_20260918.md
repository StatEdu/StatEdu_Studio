# Buderer diagnostic precision descriptions

Date: 2026-09-18

Four dictionary entries in eight languages: sensitivity/specificity sample-size descriptions and their achieved-half-width templates. The numeric token is captured as a string by anchored, design-specific patterns requiring three decimal places. Translation never recomputes or reformats the value.

Implementation: `scripts/fill_diagnostic_precision_notes_i18n.py`, merged by the shared dictionary owner; exact-source and dynamic-note lookup in `R/sample_size_ui.R`.

Verification:

- `scripts/fixtures_diagnostic_precision_notes_i18n.R` reuses the established clinical fixture and selects four real sensitivity/specificity sample-size/precision results.
- Achieved half-widths agree with normal-precision reference expressions. Their power remains NA, rather than being replaced by precision or a fabricated power estimate.
- Eight-language checks cover rendered dynamic notes, 0.000/0.001/12.345/000.120 token preservation, and unchanged unknown text with a suffix, custom design or unsupported decimal precision.
- Serialized source results remain unchanged. Korean/Japanese current and accumulated HTML, DOCX, native HWPX and XLSX content checks passed, including captured dynamic notes. PDF extracted text passed: 4 current and 7 accumulated pages per language.
- Existing numerical regression suite, three actual validation errors across eight languages and scoped whitespace checks passed.

Artifacts: `tmp/diagnostic-precision-notes-i18n`. Automated content/structure checks only; no fresh browser or Word/Hancom visual inspection. Production sessions were not restarted. Other sample-size method descriptions remain untranslated.
