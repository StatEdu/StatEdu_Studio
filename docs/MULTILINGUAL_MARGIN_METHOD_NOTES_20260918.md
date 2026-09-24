# Equivalence and non-inferiority margin interpretation

Date: 2026-09-18

Two new method descriptions translated into eight languages: positive distance means inside the equivalence margin, or above the negative non-inferiority boundary. The diagnostic AUC method sentence already matches `diagnostic_auc` and reuses that translation. The survival log-HR method note is formula-only and unchanged.

Implementation: `scripts/fill_margin_method_notes_i18n.py`, merged by the shared dictionary owner, and exact-source lookup in `R/sample_size_ui.R`. No numerical calculation or stored-result changes.

Verification:

- `scripts/fixtures_margin_method_notes_i18n.R` exercises nine actual calls: mean/proportion outcomes for both objectives, exact mean boundaries and outside boundaries for both objectives, and AUC.
- Binary-exact margin .25 and effect -.25 test distance zero without decimal rounding ambiguity. Strictly positive distances alone report inside_margin=Yes; zero and negative distances report No. Mean SD and pooled Bernoulli SD standardization are checked independently.
- AUC difference and approximate d match independent references. Eight-language rendered notes and expressions pass, and source serialization is unchanged.
- Korean/Japanese current and accumulated HTML, DOCX, native HWPX and XLSX content checks passed. PDF extracted text passed: 10 current and 19 accumulated pages in each language.
- Existing numerical regression suite and three actual validation errors across eight languages passed. Scoped whitespace checks passed.

Artifacts: `tmp/margin-method-notes-i18n`. Automated content/structure checks, not fresh browser or Word/Hancom visual inspection. Production sessions were not restarted. Remaining effect-size and planning method descriptions are outside this batch.
