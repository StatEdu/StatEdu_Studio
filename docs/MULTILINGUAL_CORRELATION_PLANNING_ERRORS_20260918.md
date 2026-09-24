# Correlation planning and precision validation

Date: 2026-09-18

Three existing errors now render in eight languages: finite expected r inside (-1,1), nonzero expected r inside (-1,1) for power planning, and invalid CI half-width for the expected correlation. Only exact display lookup changes. Shared dictionary owner applied `scripts/fill_correlation_planning_errors_i18n.py`.

`scripts/fixtures_correlation_planning_errors_i18n.R` checks ten actual errors across eight languages: -1/1/NaN/Inf in planning and precision, zero in planning, and the precision branch’s existing upper-clamp guard at r=.9999999. Negative valid correlations and precision at r=0 remain accepted. Seven valid outputs match Fisher-z sample-size/precision references and signed-r power equality. Raw errors and unknown-message passthrough are preserved.

Korean/Japanese current and accumulated HTML, PDF, DOCX, native HWPX and XLSX valid-result content checks pass (PDF 4/7 pages). Existing numerical regression/validation and scoped whitespace checks pass. Errors remain transient UI warnings. Artifacts: `tmp/correlation-planning-errors-i18n`.

Automated content/structure verification only; no fresh browser/editor visual inspection, production restart or installer rebuild. Other translation work remains.
