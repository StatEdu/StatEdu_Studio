# ANOVA, ANCOVA and regression effect-size method notes

Date: 2026-09-18

Seven method descriptions localized in eight languages: partial omega squared from F, ANOVA partial eta squared, Pillai trace, Wilks lambda, ANCOVA partial eta squared, logistic OR-to-d conversion and moderation incremental f-squared. Mathematical expressions and approximation qualifications are preserved.

Implementation: `scripts/fill_model_effect_notes_i18n.py`, merged by the shared dictionary owner; exact-source lookup in `R/sample_size_ui.R`. Calculations and stored result objects are unchanged.

Verification:

- `scripts/fixtures_model_effect_notes_i18n.R` runs seven actual calculator branches and checks exact English source notes and mathematical tokens in eight languages.
- Independent numerical expectations: omega squared 7/97, partial eta squared 9/96, Pillai f-squared .1/.9, Wilks f-squared .9^(-.5)-1, negative logistic d at OR=.5, and moderation f-squared .05/.95.
- Eight-language rendering and serialized source preservation passed. Korean/Japanese current and accumulated HTML, DOCX, native HWPX and XLSX content checks passed; PDFs were generated and extracted text verified. Each language produced 8 current and 15 accumulated PDF pages.
- Existing sample-size numerical regression suite and three actual validation errors across eight languages passed. Scoped whitespace checks passed.

Artifacts: `tmp/model-effect-notes-i18n`. Verification covers automated content and structure, not fresh browser or Word/Hancom visual review. Production sessions were not restarted. Other effect-size and planning method notes remain; this is not a claim of complete application translation.
