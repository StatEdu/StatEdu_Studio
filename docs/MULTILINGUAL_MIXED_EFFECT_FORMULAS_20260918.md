# GEE, GLMM and LMM formula explanations — 2026-09-18

Eleven exact-source formula descriptions now use eight-language dictionary entries: five GEE descriptions, three GLMM descriptions and three LMM descriptions. Mathematical variable names and expressions remain identical to the source. Existing numerical calculations and serialized result objects are unchanged. This does not complete translation of all method notes or result text.

Dictionary script: `scripts/fill_mixed_effect_formula_i18n.py`, integrated by the shared dictionary owner. Rendering uses `sample_size_result_text` in `R/sample_size_ui.R`.

Verification:

- `validate_sample_size_result_i18n.R scripts/fixtures_mixed_effect_formula_i18n.R`: eleven actual calculator branches using real menu identifiers, exact formula source matching, mathematical token preservation and rendered descriptions in eight languages; immutable serialized source results.
- Korean/Japanese current and accumulated snapshots written through HTML, PDF, DOCX, native HWPX and XLSX shared writers. HTML/DOCX/HWPX/XLSX content comparisons passed.
- `validate_sample_size_result_pdf.py tmp/mixed-effect-formula-i18n`: PDF extracted content passed, 12 pages current and 23 accumulated in each language. Japanese middle dot U+30FB was extracted as U+2027; the comparison normalizes this specific glyph mapping in addition to Unicode compatibility and whitespace. Export content was not changed to satisfy the check.
- `validate_sample_size_result_errors.R`: three actual validation errors across eight languages and the existing numerical sample-size/effect-size regression suite passed.
- Scoped whitespace checks passed.

Artifacts: `tmp/mixed-effect-formula-i18n`. These are content and structure checks, not a fresh browser, Word or Hancom visual review. Production sessions were not restarted. Survival, equivalence, diagnostic, rates and other formula/method descriptions remain for subsequent work.
