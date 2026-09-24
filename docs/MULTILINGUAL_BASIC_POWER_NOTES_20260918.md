# Basic planning and achieved-power method notes

Date: 2026-09-18

Ten method descriptions added in eight languages: exact t power for three designs, rank power with ARE=.955, one/two-proportion sample size and power, Fisher-z correlation planning and noncentral chi-square planning. Exact versus approximate wording and null p=.50 are retained. Numerical calculations are unchanged.

Implementation: `scripts/fill_basic_power_notes_i18n.py`, merged by the shared dictionary owner, and exact-source lookup in `R/sample_size_ui.R`.

The export fixture exposed a genuine single-row Power-table failure in editable export: the table parser treated the row-label th as a column-header row, leaving no body and causing out-of-bounds indexing in `result_document_table`. `R/result_saved_ui.R` now excludes `sample-size-result-table` from the inferred-header fallback; explicit thead and other tables retain their existing behavior. This preserves key/value rows, including Design rows in longer tables.

Verification:

- Fourteen real UI calculations in `scripts/fixtures_basic_power_notes_i18n.R`. Independent references use power.t.test, ARE-adjusted n, normal proportion expressions, negative-r Fisher-z calculations and the noncentral chi-square distribution.
- `scripts/validate_sample_size_row_headers.R` verifies single/multiple sample-size body rows, ordinary column-header inference and explicit thead preservation.
- Eight-language notes and source serialization preservation passed.
- Korean/Japanese current and accumulated HTML, DOCX, native HWPX and XLSX content passed, including single-row Power and multirow sample-size tables. PDF extracted text passed: Korean 7/12 pages and Japanese 7/13 pages for current/accumulated output.
- Existing numerical regression suite, three actual validation errors across eight languages, and scoped whitespace checks passed.

Artifacts: `tmp/basic-power-notes-i18n`. Automated content/structure checks only, without new browser or Word/Hancom visual inspection. Production sessions were not restarted. Other planning descriptions remain untranslated.
