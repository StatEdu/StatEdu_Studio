# Repeated-measures correlation validation messages

Date: 2026-09-18

Five existing error messages now display in eight languages: minimum time points, unstructured coefficient bounds, positive definiteness for unstructured/exchangeable matrices, and required pairwise-correlation count. The count template strictly matches integer strings and retains both count and time-point values. Core matrix validation and calculations are unchanged. Shared dictionary owner applied `scripts/fill_lmm_correlation_errors_i18n.py`.

`scripts/fixtures_lmm_correlation_errors_i18n.R` checks seven real matrix errors across eight languages, including counts for three/four time points, both coefficient boundaries, and invalid matrices. Checks include exact leading-zero count preservation, unknown-message passthrough, unchanged error objects, three independent valid matrix references, three real LMM/GLS power outputs, and RNG-state preservation.

Korean/Japanese current and accumulated HTML, PDF, DOCX, native HWPX and XLSX valid-result content verification passed (PDF 4/7 pages). Existing numerical regression tests, prior error tests and scoped whitespace checks passed. Errors remain transient warnings. Artifacts: `tmp/lmm-correlation-errors-i18n`.

Automated content/structure verification only; no fresh browser/editor visual inspection or production restart. This batch does not cover the AR(1) numerical-failure guard or numeric-vector parsing messages; other validation translations remain.
