# Rate validation messages

Date: 2026-09-18

Four existing validation messages now render in eight languages: neutral ratio, zero/nonfinite log ratio, equal rates, and negative/nonfinite dispersion. Only the display lookup in `R/sample_size_ui.R` changes; original errors and validation rules remain intact. Dictionary owner applied `scripts/fill_rate_validation_i18n.py`.

`scripts/fixtures_rate_validation_i18n.R` verified 20 actual failures across eight languages, two valid zero-dispersion controls, unchanged source errors and unknown-message passthrough. Twelve valid rate-effect calculations retain numerical references and export regression coverage. Existing general numerical/validation checks passed.

Korean/Japanese current and accumulated HTML, PDF, DOCX, native HWPX and XLSX valid-result content checks passed (PDF 13/25 pages). Errors remain transient UI warnings and were not turned into saved analysis results. Artifacts: `tmp/rate-validation-i18n`.

Automated content/structure checks only; no fresh browser/editor visual inspection, server restart, or installer rebuild. Other modules still contain untranslated validation messages.
