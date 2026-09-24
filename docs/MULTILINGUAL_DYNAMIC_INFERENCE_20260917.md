# Dynamic bootstrap inference descriptions — 2026-09-17

Extended non-English/non-Korean inference localization beyond whole-string dictionary lookup. Recognized program syntax now supports BCa, BC, percentile and the BCa alias, optional R quantile type, valid/valid-standardized counts and percentages, and Adequate/Caution/Unreliable status suffixes.

Only the known interval syntax and numeric/status suffix are parsed. Unknown text, malformed/custom metadata and user paths remain literal. Exact status/suppression descriptions have translations for requested, pending, canceled, failed, blocked, unavailable and fixed/no-test cases. Reporting metadata now routes inference/source/status columns through this helper. English and the existing Korean branches are preserved.

Validation:

- `scripts/validate_dynamic_inference_i18n.R`: four interval spellings × three quantile variants × three statuses × two count variants × eight languages. Count/percentage and quantile values remain intact. Additional suppression, fixed-effect, insufficient-standardized-replicate and unknown-text cases pass.
- SEM structural reporting integration and effect-supplement language tests pass again.
- Fixtures test string rendering and metadata tables; they do not rerun bootstrap estimation.
- Japanese current/accumulated snapshots in `tmp/dynamic-inference-i18n` passed HTML/PDF/Word/HWPX/Excel content verification, PDF text and localized cover checks, and HTML/Word table order plus Excel counts (5 current, 6 accumulated).

No installer or installed application changed. Broader multilingual verification remains in progress.
