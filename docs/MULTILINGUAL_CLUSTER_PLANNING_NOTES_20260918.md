# Cluster trial planning method notes

Date: 2026-09-18

Three descriptions translated into eight languages: WebPower parallel two-arm continuous trial, design-effect adjustment and Hussey–Hughes-style stepped-wedge simulation. The latter two use anchored dynamic templates preserving the exact three-decimal design-effect string and integer simulation count. Balanced cluster rounding, fixed period effects and random cluster intercepts remain explicit.

Implementation: `scripts/fill_cluster_planning_notes_i18n.py`, merged by the shared dictionary owner; exact-source/dynamic lookup in `R/sample_size_ui.R`. No numerical changes.

Verification:

- `scripts/fixtures_cluster_planning_notes_i18n.R` runs five real cluster outputs: stepped-wedge power plus sample-size/power modes for WebPower and binary design-effect adjustment; it also reuses the established advanced fixture checks.
- Cluster rounding checks verify ceil(raw clusters/2) per group, group balance, total clusters and participant count. Supplementary numeric assertions were run after adding them without regenerating unchanged export content.
- Eight-language dynamic-note rendering, exact 00020/001.450 token preservation and appended-unknown-text identity passed. Source serialization is unchanged.
- Korean/Japanese current and accumulated HTML, DOCX, native HWPX and XLSX content passed. PDF extracted text passed: 6 current and 11 accumulated pages per language.
- Existing numerical regression suite, three actual validation errors across eight languages and scoped whitespace checks passed.

Artifacts: `tmp/cluster-planning-notes-i18n`. Automated content/structure checks only; no fresh browser or Word/Hancom visual inspection. Production sessions were not restarted. Other method descriptions remain untranslated.
