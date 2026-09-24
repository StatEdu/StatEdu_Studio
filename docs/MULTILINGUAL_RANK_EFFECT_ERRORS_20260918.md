# Rank effect-size validation

Date: 2026-09-18

Two existing errors now render in eight languages: Mann–Whitney U above n1*n2, and fewer than two measurements. Exact display lookup only; calculations and input restrictions remain unchanged. Shared dictionary owner applied `scripts/fill_rank_effect_errors_i18n.py`.

`scripts/fixtures_rank_effect_errors_i18n.R` checks six real failures across eight languages. Five valid references cover negative/zero/upper-bound rank-biserial correlations, Cliff's delta equality, Friedman with two measurements, and the existing Kendall-W cap at 1. Raw errors and unknown-message passthrough remain unchanged.

Five valid outputs pass Korean/Japanese current and accumulated HTML, PDF, DOCX, native HWPX and XLSX content verification (PDF 6/11 pages). Existing numerical regression/validation and scoped whitespace checks pass. Errors remain transient warnings. Artifacts: `tmp/rank-effect-errors-i18n`.

Automated content/structure checks only; no fresh browser/editor visual inspection, production restart or installer rebuild. Other translation work remains.
