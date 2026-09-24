# Regression predictor count errors

Date: 2026-09-18

Three existing regression planning errors are localized in eight languages: at least one predictor, at least one tested predictor, and total predictors at least tested predictors. Shared dictionary owner merged `scripts/fill_regression_count_errors_i18n.py`. Exact display lookup only; calculations and validation conditions remain unchanged.

`scripts/fixtures_regression_count_errors_i18n.R` checks twenty actual errors across eight languages: zero/negative/nonfinite predictor counts for multiple regression; tested/interaction counts and total counts for hierarchical/moderation regression. Raw errors and unknown messages remain unchanged. Six valid minimum-count cases cover all three designs with one tested/total predictor in achieved-power and required-sample-size modes. Independent noncentral-F references check power and that n meets target while n-1 does not. General sample-size numerical and validation regression checks pass.

Korean/Japanese current and accumulated HTML, PDF, DOCX, native HWPX and XLSX content checks pass with valid captured results. PDF text checks pass for 4/7 pages in both languages. Errors remain transient warnings. Artifacts: `tmp/regression-count-errors-i18n`.

Automated content/structure checks only; no fresh browser/editor visual inspection, production restart or installer rebuild. HWPX UI availability is unchanged. Other translation work remains.
