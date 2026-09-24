# Diagnostic detail localization — 2026-09-16

Source changes only; no installer was built or installed.

## Scope

- Added 38 diagnostic phrases/templates to the eight language catalogs, including Japanese, Chinese, Spanish, French, German and Vietnamese.
- Repeated-measures supplementary output: sphericity decisions, unchecked normality, combined Shapiro-Wilk and Gaussian-model explanations.
- ANCOVA supplementary output: slope/linearity decisions, VIF and Cook's D messages, residual-normality explanation, observed-descriptive appendix heading.
- Survival supplementary output: overview/mapping/audit headings, curve-crossing and tail-risk diagnostics, reporting checklist labels and interpretation guidance.
- Connected survival appendix title/note helpers to the shared catalog. Notes without a catalog translation still fall back to English; this is not a claim that every survival note is translated.
- Complete generated diagnostic formats are translated before captured values are inserted. The existing identity-column protection preserves user names and labels. Main tables continue to use English.

## Validation

- `validate_diagnostic_detail_i18n.R`: six foreign languages passed; numbers and user group names within generated messages preserved; variable labels deliberately equal to diagnostic phrases preserved.
- `validate_multilingual_table_roles.R`: eight representative analyses in all eight languages passed main-table title/cell/note equivalence to English; 16 appendix localizers preserve user identifiers and numeric data.
- `validate_i18n_contract.R`: passed with UTF-8 locale set before sourcing. An initial invocation without that locale failed during parsing; rerun with the required locale passed.
- Current and accumulated HTML, PDF, Word, HWPX and Excel exports passed for the changed diagnostic fixture. Actual PDF text was checked with `validate_multilingual_pdf.py`.
- `git diff --check`: passed for affected source/catalog paths.

The Japanese rendered-appendix candidate scan decreased from 41 to 4. The four remaining matches in this fixture are two original user-label strings and two translated sentences containing the Shapiro-Wilk test name. This scan covers table cells and nearby headings in the representative fixtures, not all menus, standalone notes, analysis branches, charts or UI states.

## Remaining audit

Continue the source-wide inventory from `MULTILINGUAL_REAUDIT_20260916.md`, especially longitudinal and structural-model supplementary diagnostics, survival branches and standalone explanatory notes. Whole-application multilingual completion has not been established.
