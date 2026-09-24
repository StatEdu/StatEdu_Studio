# SEM model-size validation messages

Date: 2026-09-18

Four existing errors now display in eight languages: latent-variable minimum, measured-variable count relative to latent variables, nonnegative structural-path count, and free-parameter minimum. Exact display lookup only; validation and numerical computation remain unchanged. Shared dictionary owner applied `scripts/fill_sem_structure_counts_i18n.py`.

`scripts/fixtures_sem_structure_counts_i18n.R` verifies fourteen real errors across eight languages in complexity planning and structure-estimated df paths, with invalid boundaries and NaN. A minimum-count complexity model (one latent/one observed/zero paths/one free parameter) remains accepted. Independent df reference with one latent/four observed/zero paths yields 10 observed moments, 8 free parameters and df=2. Error objects and unknown messages remain unchanged.

Fourteen valid SEM complexity/RMSEA results retain existing numerical references and pass Korean/Japanese current and accumulated HTML, PDF, DOCX, native HWPX and XLSX content verification (PDF 15/29 pages). Existing general numerical/validation and scoped whitespace checks pass. Errors remain transient warnings. Artifacts: `tmp/sem-structure-counts-i18n`.

Automated content/structure checks only; no fresh browser/editor visual inspection, production restart or installer rebuild. Other untranslated validation messages remain.
