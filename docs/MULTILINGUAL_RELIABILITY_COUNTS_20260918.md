# Reliability input-count validation

Date: 2026-09-18

Three existing messages now render in eight languages: minimum item count for alpha, minimum raters/measurements for ICC, and minimum categories for kappa. Only exact display lookup changes; original errors, restrictions and calculations remain unchanged. Shared dictionary owner applied `scripts/fill_reliability_counts_i18n.py`.

`scripts/fixtures_reliability_counts_i18n.R` checks twelve actual failures across eight languages (1/0/NaN/Inf for each count). Three valid count=2 controls match independent sample-size and dropout-adjustment references. Five existing reliability cases retain their numerical and minimum-n checks. Unknown messages and original error objects are preserved.

Eight valid outputs pass Korean/Japanese current and accumulated HTML, PDF, DOCX, native HWPX and XLSX content verification (PDF 9/17 pages). Existing general numerical/validation and scoped whitespace checks pass. Errors remain transient warnings. Artifacts: `tmp/reliability-counts-i18n`.

Automated content/structure checks only; no fresh browser/editor visual inspection, production restart or installer rebuild. Other validation translations remain unfinished.
