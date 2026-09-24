# Default longitudinal sensitivity recommendations — 2026-09-16

Source/catalog-only continuation; installer not built or installed.

Added 15 catalog sources in all eight languages for the default GEE, LMM, GLMM, panel-FE and panel-RE sensitivity recommendations and the unknown-model fallback. The GEE correlation-comparison sentence now uses a localized template while preserving the original correlation label.

Verification calls `longitudinal_sensitivity_recommendations()` for all five models and the fallback. Every returned message is localized for Korean and the six foreign languages, and identical user-variable labels remain unchanged. A correlation label containing Korean text, a period and parentheses is preserved.

Actual GEE/LMM/panel-FE manuscript Sensitivity paragraphs contain none of their original English component messages in non-English UI languages. Main-table content remains English across all eight languages. GLMM and panel-RE recommendation tests exercise the producer directly; they do not constitute new model fits.

Current and accumulated five-format exports include the recommendation lists and generated manuscript sections. Catalog and targeted whitespace checks pass; PDF text is checked separately.

Scope limit: this completes the tested default recommendation producer, not every longitudinal recommendation branch. REML-specific overrides, Assumptions prose, weighted summaries and other analysis families remain for review. No whole-application multilingual completion claim.
