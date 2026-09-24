# Specific indirect main table and CI contract — 2026-09-17

Resolved the regression discrepancy recorded in `MULTILINGUAL_EFFECT_SUPPLEMENT_20260917.md`.

The shared table contract (`docs/table_output_rules.md`) requires a grouped `95% CI` heading with `LLCI` and `ULCI`. The old integration assertion incorrectly required the contiguous text `B 95% CI`. It now checks the complete header sequence, the two-column CI span and exact lower/upper values, instead of an obsolete HTML substring.

Inspection also found an actual language defect: `structural_canvas_specific_indirect_html_table` translated reporting metadata before rendering its main table. Removed that translation call so the main table remains English under every UI language, with user path text preserved. Supplementary metadata localization remains available elsewhere.

Validation:

- The complete `scripts/validate_sem_structural_reporting_tables.R` now passes.
- `scripts/validate_specific_indirect_main_language.R` reuses its model-based and bootstrap fixtures, checks identical main HTML under all eight UI languages, exact CI cells/grouping and user paths containing Korean, dictionary terms, `<&>` and `%s`.
- Current/accumulated export snapshots in `tmp/specific-indirect-main-language` passed HTML/PDF/Word/HWPX/Excel content checks, PDF text and localized cover checks, and HTML/Word ordering plus Excel sheet counts (2 current, 3 accumulated).

No estimator behavior, installer or installed application was changed. The broader multilingual audit remains ongoing.
