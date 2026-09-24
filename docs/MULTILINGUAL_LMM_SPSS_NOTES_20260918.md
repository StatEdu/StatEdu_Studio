# LMM SPSS-output effect-size descriptions

Date: 2026-09-18

Two method descriptions translated into eight languages: partial eta squared from the omnibus F test and pairwise Cohen's dz from the model covariance estimates. Three exact-source dictionary keys cover each separate note and the combined sentence saved when both sets of inputs are supplied. Approximation language and mathematical expressions are preserved. No calculation or stored-result changes.

Implementation: `scripts/fill_lmm_spss_notes_i18n.py`, merged by the shared dictionary owner; exact-source lookup in `R/sample_size_ui.R`.

Verification:

- `scripts/fixtures_lmm_spss_notes_i18n.R` uses five real calculator cases: F/df only, pairwise inputs only, both sets, zero mean difference and negative covariance.
- Independent numerical references verify partial eta squared, Cohen f, signed dz and paired-difference SD. Unrequested effects remain absent and the combined output retains both effect sizes.
- Eight-language checks verify both expressions, exact combined-note composition, rendered notes and unchanged source serialization.
- Korean/Japanese current and accumulated HTML, DOCX, native HWPX and XLSX content checks passed. PDF extracted text passed: 7 current and 13 accumulated pages for each language.
- Existing numerical regression suite, three actual validation errors across eight languages, and scoped whitespace checks passed.

Artifacts: `tmp/lmm-spss-notes-i18n`. Automated content/structure checks only, without fresh browser or Word/Hancom visual inspection. Production sessions were not restarted. Other effect-size and sample-size planning descriptions remain outside this batch.
