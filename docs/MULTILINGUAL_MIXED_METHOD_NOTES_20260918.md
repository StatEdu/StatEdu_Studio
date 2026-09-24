# GEE, GLMM and LMM effect-size method descriptions

Date: 2026-09-18

Seven new exact-source notes in eight languages: GEE supplied standardized difference, GLMM binary logit, binary probabilities, count log link, count rates, Gaussian identity link, and LMM GLIMMPSE-style vectors. The distinction between model and latent scales, approximate d, marginal contrasts and consistent offset/exposure handling is retained. Existing formula-only notes are unchanged.

Implementation: `scripts/fill_mixed_method_notes_i18n.py`, merged by the shared dictionary owner; exact-source lookup in `R/sample_size_ui.R`. No numerical calculation changes.

Verification:

- `scripts/fixtures_mixed_method_notes_i18n.R` exercises ten actual calculator calls, including both coefficient and ratio input for logit/log-link models and both one-group and two-group LMM vectors.
- Independent numerical references cover negative supplied d, negative B, OR from probabilities (.375), rate ratio .5, Gaussian d=-.25, log(.5), latent d, Cohen h and repeated-measures planning effects -.4*sqrt(3/2) and .8*sqrt(3/2).
- The initial GEE test omitted the persistent SD input and produced a zero-length metadata value. The fixture now supplies the screen's standard direct-SD input; no production change was made for that incomplete synthetic input.
- All eight languages passed rendering and mathematical-expression checks. Serialized source results remained unchanged.
- Korean/Japanese current and accumulated HTML, DOCX, native HWPX and XLSX content checks passed. PDF extracted text passed: 11 current and 21 accumulated pages in each language.
- Existing sample-size numerical regression suite, three actual errors across eight languages, and scoped whitespace checks passed.

Artifacts: `tmp/mixed-method-notes-i18n`. Automated content/structure checks, not a fresh browser or Word/Hancom visual review. Production sessions were not restarted. Other method notes, including LMM SPSS-output explanations and further planning descriptions, remain outside this batch.
