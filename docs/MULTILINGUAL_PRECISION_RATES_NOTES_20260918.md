# Precision, McNemar and rate planning method descriptions

Date: 2026-09-18

Seven descriptions translated into eight languages: mean/proportion/correlation precision, McNemar, single Poisson rate precision, negative-binomial rate comparison and two-Poisson-rate comparison. Approximation qualifications, confidence-interval half-width, paired discordance, overdispersion and person-time allocation are preserved. No numerical calculation changes.

Implementation: `scripts/fill_precision_rates_notes_i18n.py`, merged by the shared dictionary owner; exact-source lookup in `R/sample_size_ui.R`.

Verification:

- `scripts/fixtures_precision_rates_notes_i18n.R` reuses fourteen real UI calculations from the established precision/rates fixture and verifies their method notes.
- Existing reference checks cover normal mean/proportion sample sizes and achieved half-widths, single-rate person-time/precision, overdispersion sample-size ordering and valid achieved powers.
- Eight-language notes, paired-probability tokens and unchanged source serialization passed.
- Korean/Japanese current and accumulated HTML, DOCX, native HWPX and XLSX content checks passed. PDF extracted text passed: Korean 8/15 and Japanese 9/17 current/accumulated pages.
- Existing numerical regression suite, three actual validation errors across eight languages and scoped whitespace checks passed.

Artifacts: `tmp/precision-rates-notes-i18n`. Automated content/structure checks only, without fresh browser or Word/Hancom visual inspection. Production sessions were not restarted. Other planning descriptions remain untranslated.
