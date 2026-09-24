# Cluster effect-size method descriptions

Date: 2026-09-18

Two method descriptions added in eight languages: binary cluster effects using Cohen h and the simple stepped-wedge period adjustment. Continuous cluster effects already use the exact existing `cluster_continuous` translation. Formula expressions and the simple-approximation qualification are retained.

Implementation: `scripts/fill_cluster_method_notes_i18n.py`, merged by the shared dictionary owner; exact-source lookup in `R/sample_size_ui.R`. No calculation or stored-result changes.

Verification:

- `scripts/fixtures_cluster_method_notes_i18n.R` runs three designs at ICC .01 and .05, checking negative primary effects, design effects and adjusted planning effects against independent expressions.
- The initial ICC=0 test established that the existing validator requires 0<ICC<1. Three additional rejection assertions preserve that behavior. ICC=0 support was not added as part of translation work.
- Eight-language expression/rendering checks and source serialization preservation passed.
- Korean/Japanese current and accumulated HTML, DOCX, native HWPX and XLSX content checks passed. PDF extracted text passed: 7 current and 13 accumulated pages per language.
- Existing numerical regression suite, three actual validation errors across eight languages, and scoped whitespace checks passed.

Artifacts: `tmp/cluster-method-notes-i18n`. Automated content/structure checks only; no fresh browser or Word/Hancom visual inspection. Production sessions were not restarted. Other effect-size and sample-size planning descriptions remain.
