# Fritz–MacKinnon empirical mediation method description

Date: 2026-09-18

Added six keys in eight languages: one dynamic empirical Table 3 description, four effect-size categories, and the Baron–Kenny causal-steps label with its original direct-effect value. The other five test names reuse existing localized labels. The fixed .80 power, citation, table number, and path order are preserved. Statistical calculations and journal-facing result cells are unchanged.

Implementation: `R/sample_size_ui.R` matches the exact known sentence, effect categories, and nine supported tests. Unknown categories/tests, changed power, and appended content pass through untouched. `scripts/fill_fritz_method_note_i18n.py` was merged through the shared dictionary owner.

Validation:

- `scripts/fixtures_fritz_method_note_i18n.R` exercises 144 actual calculations (4 a-path categories × 4 b-path categories × 9 tests), each translated into eight languages.
- Nine small/small sample-size reference values, numeric path effects, indirect effects, 10% dropout adjustment, English identity, strict unknown-text matching, and unchanged source serialization passed.
- Nine representative outputs cover all tests and all four categories in both path slots. Their descriptions render in all eight languages.
- Korean/Japanese current and accumulated HTML, PDF, DOCX, native HWPX, and XLSX content verification passed. PDF extracted-text checks: 10 current / 19 accumulated pages per language.
- Existing sample-size numerical regression checks, three actual validation errors across eight languages, and scoped whitespace checks passed.

Artifacts: `tmp/fritz-method-note-i18n`. Automated content and structure checks only, without fresh browser or Word/Hancom visual inspection. No user server restart or installer rebuild. Remaining dynamic descriptions include log-link rate effect notes; this batch does not establish full application translation completion.
