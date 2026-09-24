# Structural-effect capability supplement — 2026-09-17

Added `structural_canvas_effect_plan_ui` and routed the existing structural-effect capability output through it. The producer `structural_canvas_structural_effect_plan` is unchanged.

Localized the title, introductory note, column names and known program values: direct/mediation/moderation/moderated-mediation effects, support status, estimation methods and interpretation limitations. Coverage includes CB-SEM product-indicator variants, PLS/PLSc two-stage/product-indicator/orthogonal interactions, selective consistency correction, Johnson–Neyman requirements and PLSpredict restrictions.

The renderer translates exact known values only, then protects all cells from further generic translation. Unknown text (including Korean, translation vocabulary, `<&>` and `%s`) remains unchanged. Main analysis output and statistical calculations are unchanged.

Validation:

- `scripts/validate_effect_plan_i18n.R`: 3 engines × 3 method variants × 2 moderation-request states × 8 languages = 144 render cases. All headings, notes and program cells checked; German Mediation/Moderation intentionally retain the same spelling. Source plans remain identical. Empty and unknown-text cases also checked.
- Tests exercise the real preflight-plan producer and UI helper, not a fitted SEM or interactive desktop walkthrough.
- Japanese snapshots for the nine engine/method combinations in `tmp/effect-plan-i18n` passed current and accumulated HTML/PDF/Word/HWPX/Excel content checks. PDF text and localized cover passed. HTML/Word order and Excel sheet counts passed (9 current, 10 accumulated).

No installer was built and no installed application changed. The overall multilingual audit remains in progress.
