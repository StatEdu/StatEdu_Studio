# ANCOVA and MANOVA input validation

Date: 2026-09-18

Four existing errors now display in eight languages: covariate R-squared in [0,1), positive dependent-variable count, and Pillai V / Wilks lambda in (0,1). Exact display lookup only; statistical calculations and validation conditions are unchanged. Shared dictionary owner applied `scripts/fill_ancova_validation_i18n.py`.

`scripts/fixtures_ancova_validation_i18n.R` checks 22 actual errors in eight languages: negative/unit/nonfinite covariate R2 in effect and planning wrappers; zero/negative/nonfinite dependent-variable counts; zero/unit/negative/nonfinite Pillai and Wilks values. Unknown messages and raw error snapshots remain unchanged. Six valid outputs are checked against independent adjusted-f, Pillai, Wilks and noncentral-F power references, including R2=0 and one dependent variable.

Korean/Japanese current and accumulated HTML, PDF, DOCX, native HWPX and XLSX content checks pass with the shared result export harness and PDF text validator (PDF 7/13 pages in both languages). Errors remain transient warnings; exported fixtures are valid results. Artifacts: `tmp/ancova-validation-i18n`. General sample-size numerical and validation regression checks pass.

Automated content/structure checks only; no fresh browser/editor visual inspection, production restart or installer rebuild. HWPX UI availability is unchanged. Other translation work remains.
