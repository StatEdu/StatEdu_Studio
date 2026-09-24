# Closed-form longitudinal LMM method note

Date: 2026-09-18

The longpower LMM longitudinal slope/change method description is translated into eight languages. It retains the exchangeable random-intercept correlation assumption and the interpretation of the standardized fixed effect as group-by-time slope/change difference per residual SD. Engine identifier `longpower::diggle.linear.power` is preserved.

Implementation: `scripts/fill_longpower_note_i18n.py`, merged by the shared dictionary owner; exact-source lookup in `R/sample_size_ui.R`. No numerical changes.

Verification:

- `scripts/fixtures_longpower_note_i18n.R` runs four actual sample-size/power calculations at effect sizes +.5 and -.5. Engine identity, finite power and sign symmetry passed.
- Eight-language method rendering/engine-token checks and unchanged source serialization passed.
- Korean/Japanese current and accumulated HTML, DOCX, native HWPX and XLSX content checks passed. PDF extracted text passed: 5 current and 9 accumulated pages per language.
- Existing numerical regression suite, three actual validation errors across eight languages and scoped whitespace checks passed.

Artifacts: `tmp/longpower-note-i18n`. Automated content/structure checks only, without fresh browser or Word/Hancom visual inspection. Production sessions were not restarted. Other planning descriptions remain untranslated.
