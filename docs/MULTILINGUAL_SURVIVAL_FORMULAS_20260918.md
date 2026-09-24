# Survival, equivalence, diagnostic and rate effect-size descriptions

Date: 2026-09-18

Six exact-source formula descriptions use eight-language dictionary entries: survival log hazard ratio, equivalence distance, non-inferiority distance, ROC AUC conversion, gamma mean ratio and Poisson/negative-binomial incidence rate ratio. Mathematical expressions remain unchanged. Translation applies at rendering; stored numerical result objects and user data remain unchanged. This is not completion of all method-note translations.

Implementation: `scripts/fill_survival_formula_i18n.py` (merged by the shared dictionary owner), with exact-source lookup in `R/sample_size_ui.R`.

Validation:

- `validate_sample_size_result_i18n.R scripts/fixtures_survival_formula_i18n.R` exercises twelve actual input branches: survival, four mean/proportion by equivalence/non-inferiority combinations, AUC, and three rate designs by ratio/log-ratio input. Six formula descriptions and mathematical tokens are checked in all eight languages.
- Reference numerical checks cover log hazard ratio, standardized margin distance, AUC-to-d conversion and ratio/log-ratio equivalence. Serialized calculation results must remain unchanged during rendering and export.
- Korean/Japanese current and accumulated snapshots passed HTML, PDF, Word, native HWPX and Excel content checks. PDF extraction passed separately using `validate_sample_size_result_pdf.py tmp/survival-formula-i18n`: 13 pages current and 25 accumulated in each language.
- Existing numerical sample-size/effect-size regression suite and three actual validation errors across eight languages passed. Scoped whitespace check passed.

Artifacts: `tmp/survival-formula-i18n`. These are automated content/structure checks, not a new browser or Word/Hancom visual inspection. Production sessions were not restarted. Cluster, precision, reliability, SEM and further planning/method descriptions remain.
