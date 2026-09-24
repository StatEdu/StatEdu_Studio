# Matched-pair effect validation

Date: 2026-09-18

Four existing errors now display in eight languages: nonnegative discordant b/c counts, at least one discordant pair, and probability sum at most 1. Exact display lookup only; underlying calculations and restrictions remain unchanged. Shared dictionary owner applied `scripts/fill_mcnemar_effect_errors_i18n.py`.

`scripts/fixtures_mcnemar_effect_errors_i18n.R` verifies nine actual errors across eight languages, including negative/nonfinite b/c, both counts zero and excessive probability sums for matched OR and Cohen g. Valid sum=1 controls yield OR=3 and g=.25. Existing count references cover both one-cell-zero continuity corrections and uncorrected counts. Raw errors and unknown-message passthrough remain unchanged.

Five representative matched-pair outputs pass Korean/Japanese current and accumulated HTML, PDF, DOCX, native HWPX and XLSX content verification (PDF 6/11 pages). Existing numerical regression/validation and scoped whitespace checks pass. Errors remain transient warnings. Artifacts: `tmp/mcnemar-effect-errors-i18n`.

Automated content/structure checks only; no fresh browser/editor visual inspection, production restart or installer rebuild. Other translation work remains.
