# SEM standardized-parameter validation

Date: 2026-09-18

Two existing validation messages now render in eight languages: numeric standardized parameter and the open (-1,1) interval. Exact display lookup only; calculations, original error objects and input restrictions are unchanged. Shared dictionary owner applied `scripts/fill_sem_parameter_errors_i18n.py`.

`scripts/fixtures_sem_parameter_errors_i18n.R` verifies 18 real errors across eight languages: text/NaN/Inf/-1/1/1.1 for path, loading and latent-correlation parameters. Six valid signed parameters (-.3/.3 across three types) produce finite power, preserve RNG state and original snapshots. Existing zero rejection and unknown-message passthrough are maintained. Zero-rejection message translation is outside this batch.

Korean/Japanese current and accumulated HTML, PDF, DOCX, native HWPX and XLSX valid-result content checks pass (PDF 7/13 pages). Existing numerical regression/validation tests and scoped whitespace checks pass. Errors remain transient UI warnings. Artifacts: `tmp/sem-parameter-errors-i18n`.

Automated content/structure verification only; no fresh browser/editor visual inspection, production restart or installer rebuild. Other validation translations remain unfinished.
