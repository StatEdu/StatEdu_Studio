# Survival effect and planning validation

Date: 2026-09-18

Two existing errors now render in eight languages: positive nonneutral hazard ratio and overall event probability strictly between 0 and 1. Exact display lookup only; original validation and computations remain unchanged. Shared dictionary owner applied `scripts/fill_survival_planning_errors_i18n.py`.

`scripts/fixtures_survival_planning_errors_i18n.R` verifies fourteen real errors across eight languages: HR 0/-1/1/NaN/Inf in both effect and planning wrappers, and event probability 0/1/NaN/Inf. Six valid outputs cover HR .5/2 in sample-size, power and effect modes. Independent event-count and allocation rounding references, reciprocal-HR power equality and log-HR values pass. Unknown messages and raw error objects remain unchanged.

Korean/Japanese current and accumulated HTML, PDF, DOCX, native HWPX and XLSX content checks pass (PDF 7/13 pages). Existing numerical regression/validation and scoped whitespace checks pass. Errors remain transient warnings. Artifacts: `tmp/survival-planning-errors-i18n`.

Automated content/structure verification only; no fresh browser/editor visual inspection, production restart or installer rebuild. Further validation-message translation remains.
