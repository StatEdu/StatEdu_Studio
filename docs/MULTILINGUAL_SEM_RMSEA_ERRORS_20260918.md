# SEM RMSEA and degrees-of-freedom validation

Date: 2026-09-18

Four existing messages now display in eight languages: manual and estimated model degrees of freedom, close-fit RMSEA ordering, and not-close-fit RMSEA ordering. Exact display lookup only; calculations, validation rules and raw errors remain unchanged. Shared dictionary owner applied `scripts/fill_sem_rmsea_errors_i18n.py`.

`scripts/fixtures_sem_rmsea_errors_i18n.R` checks eight actual failures across eight languages: manual df 0/-1/NaN, nonpositive structure-estimated df, and equal/reversed RMSEA values in both tests. Two valid df=1 controls agree with independent upper/lower noncentral chi-square tail calculations. Existing fourteen SEM complexity/RMSEA cases retain reference checks; eight valid RMSEA outputs are selected for export.

Korean/Japanese current and accumulated HTML, PDF, DOCX, native HWPX and XLSX content checks passed (PDF 9/17 pages). Existing numerical regression/validation and scoped whitespace checks passed. Unknown messages and original error objects remain unchanged; errors remain transient warnings. Artifacts: `tmp/sem-rmsea-errors-i18n`.

Automated content/structure verification only; no fresh browser/editor visual inspection, production restart or installer rebuild. Further input-validation translation work remains.
