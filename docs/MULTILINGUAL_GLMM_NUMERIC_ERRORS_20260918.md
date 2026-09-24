# GLMM coefficient input errors

Date: 2026-09-18

Three existing fixed-effect coefficient B errors are localized in eight languages: binary logit, count log link and Gaussian identity link. The dictionary merge script is `scripts/fill_glmm_numeric_errors_i18n.py`. Exact display lookup only; statistical calculations and validation conditions are unchanged.

`scripts/fixtures_glmm_numeric_errors_i18n.R` checks twelve actual wrapper errors (nonnumeric text, NaN and signed infinity for each design) in eight languages. Raw error snapshots and unknown messages remain unchanged. Nine valid negative/zero/positive B cases are checked against exp(B), latent d=B*sqrt(3)/pi and Gaussian d=B/SD references. Two alternative-input controls verify OR=1 and IRR=1 remain valid when an unused coefficient field contains text. General sample-size numerical and validation regression checks pass.

Current and accumulated Korean/Japanese HTML, PDF, DOCX, native HWPX and XLSX content checks pass using valid captured results. PDF text checks pass for 10/19 pages in both languages. Shared dictionary owner merged the translation script. Errors remain transient warnings. Artifacts: `tmp/glmm-numeric-errors-i18n`.

Automated content/structure checks only; no fresh browser/editor visual inspection, production restart or installer rebuild. HWPX UI availability is unchanged. Other translation work remains.
