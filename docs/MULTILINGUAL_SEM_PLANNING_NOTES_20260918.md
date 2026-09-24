# SEM complexity and RMSEA method descriptions

Date: 2026-09-18

Three descriptions added in eight languages: complexity-based heuristic with cases-per-free-parameter count, RMSEA with model-structure-estimated degrees of freedom and RMSEA with directly supplied degrees of freedom. Approximate detectability and estimated-df qualifications remain explicit. No numerical changes.

Implementation: `scripts/fill_sem_planning_notes_i18n.py`, merged by the shared dictionary owner; exact-source/dynamic lookup in `R/sample_size_ui.R`.

Verification:

- `scripts/fixtures_sem_planning_notes_i18n.R` runs fourteen real calculations: three complexity levels in sample-size/power modes, and manual/structure df crossed with close-fit/not-close-fit and both modes.
- Checks verify the 10/15/20 cases-per-parameter rules and independent noncentral chi-square upper/lower-tail power at N=120. Manual df reference uses the input 20 because this existing result branch does not retain df as an output field.
- Eight-language method rendering and unchanged result serialization passed.
- Korean/Japanese current and accumulated HTML, DOCX, native HWPX and XLSX content checks passed. PDF extracted text passed: 15 current and 29 accumulated pages per language.
- Existing numerical regression suite, three actual validation errors across eight languages and scoped whitespace checks passed.

Artifacts: `tmp/sem-planning-notes-i18n`. Automated content/structure checks only, without fresh browser or Word/Hancom visual inspection. Production sessions were not restarted. Other method descriptions remain untranslated.
