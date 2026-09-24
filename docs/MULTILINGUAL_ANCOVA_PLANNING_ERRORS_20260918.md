# ANCOVA/MANOVA planning input errors

Date: 2026-09-18

Three existing planning errors now display in eight languages: nonnegative covariate count, at least two dependent variables for MANOVA, and Pillai V strictly between zero and one. Shared dictionary owner applied `scripts/fill_ancova_planning_errors_i18n.py`. Exact display lookup preserves statistical calculations and validation order.

`scripts/fixtures_ancova_planning_errors_i18n.R` verifies 15 actual errors in eight languages: negative/nonfinite covariate counts across ANCOVA, ranked ANCOVA and MANOVA; too few/nonfinite MANOVA outcomes; unit/above-unit Pillai V. Zero/negative/nonfinite effects are intercepted by the existing shared positive-effect validator and are outside this batch. Raw errors and unknown messages remain unchanged.

Six valid zero-covariate results cover required sample size and achieved power for all three calculation branches. Independent noncentral-F references check power and required group-rounded sample size, including that one fewer participant per group falls below target power. Ranked ANCOVA is an existing calculation branch, not an added menu option. General numerical and validation regression checks pass.

Current and accumulated Korean/Japanese HTML, PDF, DOCX, native HWPX and XLSX content checks pass with the shared export harness and PDF text validator (PDF 5/9 pages in both languages). Artifacts: `tmp/ancova-planning-errors-i18n`. Errors remain transient warnings; only valid calculation snapshots are exported. No UI export availability changes, server restart or installer rebuild. Automated content/structure verification only; no fresh browser or document-editor visual inspection. Other translation work remains.
