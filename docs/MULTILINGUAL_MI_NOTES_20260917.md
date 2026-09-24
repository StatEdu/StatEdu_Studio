# MI candidate table explanations

Localized the selection header/button and six explanatory paragraphs for MI significance, BH correction, robust-estimator limitations, EPC, sequential refits, excluded candidates and substantive justification. English/Korean wording and statistical calculations remain unchanged. Protected all candidate cell values from dictionary substitution while retaining localized supplementary-table headers.

`validate_mi_notes_i18n.R` passed all eight languages in theory and free modes, including six/four conditional notes, stable selection IDs, and a cell deliberately equal to the dictionary term `Review`. It also reruns the skipped-candidate tests. Common multilingual coverage passed.

Current and accumulated Japanese snapshots passed HTML, PDF, Word, HWPX and Excel content preservation; actual PDF text/cover and table order/count checks passed. Artifacts: `tmp/mi-notes-i18n/ja-{current,accumulated}.*`. Logs: `tmp/mi-notes-{validation,coverage,exports}.log`.

No installer was built. This pass covers the candidate panel only. MI modification history and holdout-validation panels below it still contain Korean/English branches and remain to be localized; user-written justification fields were not changed.
