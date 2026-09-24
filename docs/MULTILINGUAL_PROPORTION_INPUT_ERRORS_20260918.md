# Proportion input errors

Date: 2026-09-18

Three existing errors are localized in eight languages: nonnegative 2x2 counts and p01/p10 strictly between zero and one. Translation merge script: `scripts/fill_proportion_input_errors_i18n.py`. Exact display lookup only; calculations and validation order remain unchanged. Unreachable later guards for zero discordant probabilities are not added to the translation lookup.

`scripts/fixtures_proportion_input_errors_i18n.R` checks 36 actual wrapper errors across eight languages: negative/text/NaN/infinite values in each table cell, and zero/unit/negative/NaN/infinite p01/p10 for both matched OR and Cohen g. Raw errors and unknown messages remain unchanged. Seven valid results independently verify cross-product OR, the existing all-cell 0.5 correction when each of the four cells is zero, and matched OR/g when p01+p10=1. General numerical and validation regression checks pass.

Korean/Japanese current and accumulated HTML, PDF, DOCX, native HWPX and XLSX content checks pass using valid captured results. PDF text checks pass for 8/15 pages in both languages. Shared dictionary owner merged the translation script. Errors remain transient warnings. Artifacts: `tmp/proportion-input-errors-i18n`.

Automated content/structure checks only; no fresh browser/editor visual inspection, production restart or installer rebuild. HWPX UI availability is unchanged. Other translation work remains.
