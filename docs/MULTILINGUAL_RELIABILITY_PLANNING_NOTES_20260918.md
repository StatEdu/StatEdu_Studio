# Reliability sample-size method descriptions

Date: 2026-09-18

Four descriptions translated into eight languages: Bland–Altman precision, Bonett-style alpha precision, Fisher-z ICC precision and large-sample kappa precision. Alpha/ICC item or rater counts use strict anchored dynamic matching, preserving the input count string. Alpha minimum n=items+1 and equal category prevalence assumptions are retained.

Implementation: `scripts/fill_reliability_planning_notes_i18n.py`, merged by the shared dictionary owner; exact-source/dynamic lookup in `R/sample_size_ui.R`. No numerical changes.

Verification:

- `scripts/fixtures_reliability_planning_notes_i18n.R` runs five actual planning calculations, including an alpha case where the minimum-subject rule is active.
- Independent expressions check all sample sizes and 10% dropout rounding; alpha with 20 items and a broad target half-width returns minimum n=21.
- Eight-language dynamic note rendering, 0005 count preservation, appended unknown text identity and source serialization preservation passed.
- Korean/Japanese current and accumulated HTML, DOCX, native HWPX and XLSX content checks passed. PDF extracted text passed: 6 current and 11 accumulated pages per language.
- Existing numerical regression suite, three actual validation errors across eight languages and scoped whitespace checks passed.

Artifacts: `tmp/reliability-planning-notes-i18n`. Automated content/structure checks only; no fresh browser or Word/Hancom visual inspection. Production sessions were not restarted. Other method descriptions remain untranslated.
