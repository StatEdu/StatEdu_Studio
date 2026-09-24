# GEE working-correlation validation

Date: 2026-09-18

Three messages now display in eight languages: required pairwise-correlation count, unstructured working-correlation bounds, and rho bounds. Exact integer-count matching preserves both numeric slots. The existing GEE approximation accepts correlations in [0,1); this batch preserves that implementation constraint and changes no calculations. Shared dictionary owner applied `scripts/fill_gee_correlation_errors_i18n.py`.

`scripts/fixtures_gee_correlation_errors_i18n.R` checks twelve actual UI-wrapper errors across eight languages: counts for three/four time points, negative/unit unstructured correlations, and negative/unit/nonfinite rho under exchangeable/AR(1). It also verifies leading-zero count strings, unknown-message passthrough, unchanged error objects and six independent design-effect references at zero/.999. Twelve real valid GEE outcomes retain existing numerical checks.

Korean/Japanese current and accumulated HTML, PDF, DOCX, native HWPX and XLSX valid-result content verification passed (PDF 11/21 pages). Existing numerical regression/validation tests and scoped whitespace checks passed. Errors remain transient UI warnings. Artifacts: `tmp/gee-correlation-errors-i18n`.

Automated content/structure checks only; no fresh browser/editor visual inspection, production restart or installer rebuild. Other input-validation translations remain unfinished.
