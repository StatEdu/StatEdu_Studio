# Logistic input validation

Date: 2026-09-18

Two existing errors now display in eight languages: positive nonneutral odds ratio and covariate R-squared in [0,1). Exact display lookup only; original errors and calculations remain unchanged. Shared dictionary owner applied `scripts/fill_logistic_validation_i18n.py`.

`scripts/fixtures_logistic_validation_i18n.R` verifies thirteen actual errors across eight languages: OR 0/-1/1/NaN/Inf in effect and planning wrappers, plus negative/unit/nonfinite covariate R2. Six valid OR .5/2 outputs include covariate R2=0, minimum sample sizes checked against independent power at n and n-1, achieved power references, and signed log-OR-to-d conversion. Raw errors and unknown messages remain unchanged.

Korean/Japanese current and accumulated HTML, PDF, DOCX, native HWPX and XLSX content checks pass (PDF 5/9 pages). Existing general numerical/validation and scoped whitespace checks pass. Errors remain transient warnings. Artifacts: `tmp/logistic-validation-i18n`.

Automated content/structure checks only; no fresh browser/editor visual inspection, production restart or installer rebuild. Other translation work remains.
