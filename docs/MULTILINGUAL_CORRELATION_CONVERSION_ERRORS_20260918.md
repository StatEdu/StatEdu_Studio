# Correlation conversion input errors

Date: 2026-09-18

Two existing errors are localized in eight languages: finite nonzero t and point-biserial r with absolute value strictly between zero and one. English retains the exact source string for lookup; other translations explicitly describe the absolute-value check already used in calculation. Shared dictionary owner merged `scripts/fill_correlation_conversion_errors_i18n.py`. No numerical or validation conditions change, including the existing rejection of zero.

`scripts/fixtures_correlation_conversion_errors_i18n.R` checks twelve actual wrapper errors in eight languages (zero, text, NaN and signed infinity for t; additionally signed unit boundaries for r). Unknown text and raw error snapshots remain unchanged. Four valid signed t/r cases are checked against independent formulas. Pure t formulas render in eight languages; two point-biserial snapshots provide the current/accumulated export fixtures. General sample-size numerical and validation regression checks pass.

Korean/Japanese current and accumulated HTML, PDF, DOCX, native HWPX and XLSX content checks pass with valid captured results. PDF text checks pass for 3/5 pages in both languages. Errors remain transient warnings. Artifacts: `tmp/correlation-conversion-errors-i18n`.

Automated content/structure checks only; no fresh browser/editor visual inspection, production restart or installer rebuild. HWPX UI availability is unchanged. Other translation work remains.
