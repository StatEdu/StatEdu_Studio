# Chi-square effect validation

Date: 2026-09-18

Three existing errors now render in eight languages: matching proportion-list lengths with at least two categories, observed/expected proportion bounds, and minimum two rows/columns. Exact display lookup only; original errors and calculations remain unchanged. Shared dictionary owner applied `scripts/fill_chisquare_effect_errors_i18n.py`.

`scripts/fixtures_chisquare_effect_errors_i18n.R` verifies eleven actual failures across eight languages, covering too few/mismatched categories, negative/nonfinite values, zero expected proportion and invalid dimensions. Valid zero observed proportion yields w=1; a valid 2x2 case agrees with sqrt(.1). Existing association-effect references and raw-error/unknown-text preservation pass.

Four representative valid outputs pass Korean/Japanese current and accumulated HTML, PDF, DOCX, native HWPX and XLSX content checks (PDF 5/9 pages). Existing numerical regression/validation and scoped whitespace checks pass. Errors remain transient warnings. Artifacts: `tmp/chisquare-effect-errors-i18n`.

Automated content/structure checks only; no fresh browser/editor visual inspection, production restart or installer rebuild. Other translation work remains.
