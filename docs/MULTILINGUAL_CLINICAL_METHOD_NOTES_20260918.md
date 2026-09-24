# Survival, equivalence and AUC planning method notes

Date: 2026-09-18

Five descriptions translated into eight languages: Schoenfeld event-based survival approximation, exact t-based TOSTER equivalence, normal-approximation TOST, one-sided non-inferiority and Hanley–McNeil ROC AUC. Engine names, approximation qualifications and the exact TOST raw mean-difference input scale are retained. No numerical calculation changes.

Implementation: `scripts/fill_clinical_method_notes_i18n.py`, merged by the shared dictionary owner; exact-source lookup in `R/sample_size_ui.R`.

Verification:

- `scripts/fixtures_clinical_method_notes_i18n.R` reuses the established 16-case clinical fixture and selects twelve sample-size/power results for the five new descriptions.
- Existing checks cover the exact TOSTER engine, unequal survival allocation, required events, valid achieved powers and diagnostic precision behavior. Sensitivity/specificity Buderer method descriptions remain outside this translation batch.
- Eight-language method rendering/engine-token checks and unchanged source serialization passed.
- Korean/Japanese current and accumulated HTML, DOCX, native HWPX and XLSX content checks passed. PDF extracted text passed: 11 current and 21 accumulated pages per language.
- Existing numerical regression suite, three actual validation errors across eight languages and scoped whitespace checks passed.

Artifacts: `tmp/clinical-method-notes-i18n`. Automated content/structure checks only; no new browser or Word/Hancom visual review. Production sessions were not restarted. Other planning method descriptions remain untranslated.
