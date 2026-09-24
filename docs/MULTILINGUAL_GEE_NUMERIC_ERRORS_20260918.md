# GEE numeric input errors

Date: 2026-09-18

Three existing GEE effect-size errors now display in eight languages: numeric group means, numeric pre/post means, and numeric parameter estimate B. Shared dictionary owner applied `scripts/fill_gee_numeric_errors_i18n.py`. Exact display lookup only; calculations and validation conditions are unchanged.

`scripts/fixtures_gee_numeric_errors_i18n.R` checks 28 actual wrapper errors: nonnumeric text, NaN, positive infinity and negative infinity in each of two group means, four pre/post means and B. All eight languages render the expected messages. Raw error snapshots and unknown messages remain unchanged. Six valid cases check negative and zero differences, standardized effects and B across the three designs. General sample-size numerical/validation regression checks pass.

Current and accumulated Korean/Japanese HTML, PDF, DOCX, native HWPX and XLSX content checks pass using valid captured results. PDF text checks pass for 7/13 pages in both languages. Errors remain transient warnings. Artifacts: `tmp/gee-numeric-errors-i18n`.

Automated content/structure checks only; no fresh browser/editor visual inspection, server restart or installer rebuild. HWPX UI availability is unchanged. Other translation work remains.
