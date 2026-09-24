# MI skipped-candidate diagnostics

New sequential MI results retain separate path, admissibility-reason and raw engine-error fields in `skipped_records`, alongside the unchanged English `skipped_details` string. The MI documentation dialog and skipped-candidate supplementary table use the same renderer. Ten recognized diagnostic reasons and the fit-error prefix follow the UI language. Variable/group/path names and raw third-party error details remain unchanged.

Old results without structured records retain their original detail text rather than attempting ambiguous parsing of names containing brackets or separators. Main MI values, candidate selection and refit logic are unchanged. The skipped-detail heading is localized; other MI result headings/notes still require a separate audit.

Validation:
- `validate_mi_skipped_i18n.R`: eight languages, ten reasons, literal names containing brackets/separators/percent signs, raw errors, legacy results and actual supplementary-table UI.
- `validate_cfa_mi_holdout.R`: existing estimation/refit and holdout checks, including structured-record presence and unchanged English details for generated sequential results.
- Common multilingual coverage passed.
- Current and accumulated Japanese snapshots passed HTML, PDF, Word, HWPX and Excel content checks. PDF extracted text and cover passed; table order/count checks passed (two current tables, four accumulated tables).

Export artifacts: `tmp/mi-skipped-i18n/ja-current.*` and `ja-accumulated.*`. Validation logs: `tmp/mi-skipped-{validation,engine,coverage,exports}.log`. No installer was built.

These checks establish the changed detail path, not complete application-wide localization or interactive browser coverage. Raw external engine error text is intentionally retained for diagnosis.
