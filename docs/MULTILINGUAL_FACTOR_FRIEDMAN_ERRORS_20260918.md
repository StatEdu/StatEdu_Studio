# Factorial ANOVA and Friedman input errors

Date: 2026-09-18

Three existing errors now display in eight languages: at least two levels per factor, at least three Friedman measurements, and Kendall W in (0,1]. Shared dictionary owner merged `scripts/fill_factor_friedman_errors_i18n.py`. Exact display lookup only; calculations and validation order remain unchanged. This covers an existing Friedman calculation branch, without adding menu options.

`scripts/fixtures_factor_friedman_errors_i18n.R` checks fourteen actual errors in eight languages: zero/unit/nonfinite levels for both factors; too few/nonfinite measurements; W above one. Nonpositive/nonfinite effect sizes are intercepted by the existing shared positive validator and are outside this batch. Raw error snapshots and unknown messages remain unchanged. Five valid power outputs use independent noncentral-F references for both main effects and interaction in a 2x3 design, and noncentral chi-square references for Friedman with three measurements and W=.15/1. General numerical and validation regression checks pass.

Korean/Japanese current and accumulated HTML, PDF, DOCX, native HWPX and XLSX content checks pass with valid captured results. PDF text checks pass for 4/6 pages in both languages. Errors remain transient warnings. Artifacts: `tmp/factor-friedman-errors-i18n`.

Automated content/structure checks only; no fresh browser/editor visual inspection, production restart or installer rebuild. HWPX UI availability is unchanged. Other translation work remains.
