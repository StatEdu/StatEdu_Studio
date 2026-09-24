# Structural effect supplements — 2026-09-17

Localized direct/indirect/total-effect supplementary section headings and explanations, grouped effect headers, path/source labels and the CI-source note. Explicit language is now passed through the long-format and fallback table renderers. Non-Korean reporting metadata and inference text use dictionary lookup; unmapped text remains as supplied. Main-table renderers were not edited.

`scripts/validate_effect_supplement_i18n.R` passes all eight languages for wide effect/p-value, grouped CI and long-format tables, using actual render expressions and table helpers. Tests preserve user paths, source tables, numeric/interval strings and empty results. The current cases cover model-based inference and insufficient standardized-bootstrap CI; this does not certify every dynamic bootstrap source sentence.

Japanese snapshots in `tmp/effect-supplement-i18n` passed current/accumulated HTML/PDF/Word/HWPX/Excel content checks. PDF text/cover, HTML/Word order and Excel counts passed (3 current, 4 accumulated).

Additional regression limitation: `scripts/validate_sem_structural_reporting_tables.R` stops at line 137, before its effect-summary tests, because it expects the literal header `B 95% CI` in the specific-indirect table. The rendered headers are `B`, `SE`, `95% CI`, `beta`, `z`, `p`, `BH p`, `LLCI`, `ULCI`. That specific-indirect renderer was not edited in this change. The broad script is not reported as passing and its assertion was not weakened. See `tmp/effect-supplement-regression.log`.

Follow-up: the regression discrepancy above was resolved in `MULTILINGUAL_SPECIFIC_INDIRECT_MAIN_20260917.md`. The test now checks the documented grouped CI structure and exact interval values, and the full SEM structural reporting validation passes. That follow-up also fixes unintended translation of the specific-indirect main-table Path header.

No installer was built and the installed application was not modified. The wider multilingual audit remains open.
