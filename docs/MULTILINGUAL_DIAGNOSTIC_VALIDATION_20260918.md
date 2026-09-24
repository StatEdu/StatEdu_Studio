# Diagnostic validation messages

Date: 2026-09-18

Three existing validation messages now display in eight languages: effect-size AUC versus null AUC, expected AUC versus null AUC in sample-size planning, and diagnostic precision below 1. Original error strings and numeric validation rules remain unchanged. Exact lookup in `R/sample_size_ui.R`; shared dictionary owner applied `scripts/fill_diagnostic_validation_i18n.py`.

`scripts/fixtures_diagnostic_validation_i18n.R` verified eight actual errors across eight languages, unknown-message passthrough, and unchanged error objects. Seven valid diagnostic snapshots cover sensitivity/specificity sample size and precision, AUC sample size and power, and AUC effect size. Independent sensitivity/specificity sample-size and AUC-effect references and two precision=.999 boundary controls passed.

Korean/Japanese current and accumulated HTML, PDF, DOCX, native HWPX and XLSX valid-result content checks passed (PDF 7/13 pages). Errors remain transient UI warnings rather than saved results. Scoped whitespace checks passed. Artifacts: `tmp/diagnostic-validation-i18n`.

Automated content/structure checks only; no fresh browser/editor visual inspection, server restart, or installer rebuild. Additional untranslated validation messages remain in other modules.
