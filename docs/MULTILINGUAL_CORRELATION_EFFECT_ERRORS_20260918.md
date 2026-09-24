# Correlation effect-size validation

Date: 2026-09-18

Two existing errors now render in eight languages: finite single correlation inside (-1,1), and the same requirement for both correlations. Exact display lookup only; original errors and calculations remain unchanged. Shared dictionary owner applied `scripts/fill_correlation_effect_errors_i18n.py`.

`scripts/fixtures_correlation_effect_errors_i18n.R` checks sixteen actual errors across eight languages: -1/1/NaN/Inf in Pearson r and Fisher z, and in each Cohen-q input independently. Eight valid calculations verify negative/zero/positive r, atanh references and q sign reversal. Formula-only Fisher z rendering remains unchanged in all languages. Unknown messages and raw error objects are preserved.

Five representative Pearson/q outputs pass Korean/Japanese current and accumulated HTML, PDF, DOCX, native HWPX and XLSX content verification (PDF 6/11 pages). Existing general numerical/validation and scoped whitespace checks pass. Errors remain transient warnings. Artifacts: `tmp/correlation-effect-errors-i18n`.

Automated content/structure verification only; no fresh browser/editor visual inspection, production restart or installer rebuild. Other translation work remains.
