# Regression R-squared input validation

Date: 2026-09-18

Four existing messages now render in eight languages: R-squared, full-model R-squared, interaction delta R-squared bounds, and reduced/full model ordering. Exact display lookup only; original errors and calculations remain unchanged. Shared dictionary owner applied `scripts/fill_regression_r2_errors_i18n.py`.

`scripts/fixtures_regression_r2_errors_i18n.R` checks thirteen actual failures across eight languages, including zero/unit/nonfinite bounds and negative/equal/greater reduced-model values. Four valid f-squared calculations match independent references, including reduced R2=0. Pure-formula overall f2 rendering remains unchanged in eight languages; three representative localized results are selected for export. Raw errors and unknown messages remain unchanged.

Korean/Japanese current and accumulated HTML, PDF, DOCX, native HWPX and XLSX content checks pass (PDF 4/7 pages). Existing general numerical/validation and scoped whitespace checks pass. Errors remain transient warnings. Artifacts: `tmp/regression-r2-errors-i18n`.

Automated content/structure checks only; no fresh browser/editor visual inspection, production restart or installer rebuild. Further translation work remains.
