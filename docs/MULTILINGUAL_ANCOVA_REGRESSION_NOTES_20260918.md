# ANCOVA and regression planning method descriptions

Date: 2026-09-18

Seven descriptions added in eight languages: ANCOVA, rank-transformed ANCOVA, MANOVA, logistic regression, overall multiple regression, hierarchical increments and interaction increments. Covariate residual-variance adjustment, rank efficiency, Pillai transformation and Hsieh-style approximation qualifications are preserved. No numerical calculation changes.

Implementation: `scripts/fill_ancova_regression_notes_i18n.py`, merged by the shared dictionary owner, and exact-source lookup in `R/sample_size_ui.R`.

Verification:

- `scripts/fixtures_ancova_regression_notes_i18n.R` reuses the existing 13-design model-planning validation and selects fourteen snapshots: seven designs in sample-size and achieved-power modes.
- Independent noncentral-F checks verify ANCOVA and rank ANCOVA at N=150 with covariate R-squared=.2 and the .955 rank-efficiency adjustment.
- Eight-language method rendering and source serialization preservation passed.
- Korean/Japanese current and accumulated HTML, DOCX, native HWPX and XLSX content checks passed. PDF extracted text passed: 9 current and 17 accumulated pages per language.
- Existing numerical regression suite, three actual validation errors across eight languages, and scoped whitespace checks passed.

Artifacts: `tmp/ancova-regression-notes-i18n`. Automated content/structure checks only, without fresh browser or Word/Hancom visual inspection. Production sessions were not restarted. Other planning descriptions remain outside this batch.
