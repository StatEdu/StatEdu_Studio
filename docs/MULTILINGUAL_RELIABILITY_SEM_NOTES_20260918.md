# Precision, reliability and SEM method notes

Date: 2026-09-18

Three new exact-source descriptions in eight languages: approximate Bland–Altman agreement limits, standardized SEM parameter effect, and RMSEA effect. Precision mean/proportion/correlation, alpha, ICC, kappa and SEM complexity notes already match existing translated sources and are reused. Approximation wording and mathematical expressions are preserved.

Implementation: `scripts/fill_reliability_sem_notes_i18n.py`, merged by the shared dictionary owner; exact-source rendering lookup in `R/sample_size_ui.R`. No numerical calculation changes or new menus.

Verification:

- `scripts/fixtures_reliability_sem_notes_i18n.R` reuses the established design fixture's calculations and independent numerical references, then verifies method-note sources for ten precision/reliability/SEM results.
- Seven results use actual UI wrappers. Alpha, ICC and Bland–Altman use existing internal reliability functions with their method details; these three are not claimed as currently exposed effect-size menus.
- Reference checks include agreement width 7.84, reliability difference .2, kappa observed agreement .9, standardized half-widths .05/.2, RMSEA noncentrality difference and Fisher z.
- Eight-language method rendering/expression checks and unchanged source serialization passed.
- Korean/Japanese current and accumulated HTML, DOCX, native HWPX and XLSX content checks passed. PDF extracted text passed: 11 current and 21 accumulated pages per language.
- Existing numerical regression suite, three actual validation errors across eight languages and scoped whitespace checks passed.

Artifacts: `tmp/reliability-sem-notes-i18n`. Automated content/structure checks only; no fresh browser or Word/Hancom visual inspection. Production sessions were not restarted. Other method descriptions, particularly sample-size planning notes, remain outside this batch.
