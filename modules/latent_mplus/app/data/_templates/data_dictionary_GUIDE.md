# Data Dictionary Writing Guide

## Purpose

`data_dictionary.csv` defines variable roles, labels, ordering, references, and categorical value labels used throughout the pipeline.

Because CSV does not support comments well, this document is the shared source of documentation for every dataset dictionary.

Reference files:

- [C:\StatEdu\Latent\data\03_KSWL\config\data_dictionary.csv](C:/StatEdu/Latent/data/03_KSWL/config/data_dictionary.csv)
- [C:\StatEdu\Latent\data\_templates\data_dictionary_HEADER.csv](C:/StatEdu/Latent/data/_templates/data_dictionary_HEADER.csv)

## Required Columns

- `var_name`: raw variable name in the source data
- `var_label`: display label used when a single common label is enough
- `label_ko`: Korean label
- `label_en`: English label
- `role`: pipeline role
- `type`: variable type
- `use`: whether the variable is used in the current analysis
- `display_order`: output order in tables and figures
- `display_group`: broad grouping for display logic
- `reference`: reference value for categorical variables
- `reference_label`: display label for the reference category
- `description`: short functional description
- `value_1` to `value_6`, `label_1` to `label_6`: coded value labels for categorical variables

## Role Rules

- `id`: case or person identifier
- `indicator`: latent profile / class indicator
- `covariate`: predictor or adjustment variable
- `outcome`: distal outcome
- `weight`: survey weight
- `strata`: survey strata
- `cluster`: survey cluster / PSU
- `time`: time or wave variable when needed

Use one clear role per variable. Do not create dataset-specific custom roles unless the pipeline is extended to support them.

## Type Rules

- `continuous`: numeric continuous variable
- `binary`: two-level categorical variable
- `categorical`: unordered multi-category variable
- `ordered`: ordered categorical variable
- `id`: identifier field
- `auto`: reserved only for special future placeholders; avoid for ordinary analysis variables

## Label Rules

- Fill `label_ko` and `label_en` whenever possible.
- Keep `var_label` aligned with the main display label used most often in tables.
- Avoid abbreviations unless they are already standard in the dataset.
- If a label is not final, still use a readable placeholder rather than leaving a broken string.

## Reference Rules

- Use `reference` only for categorical, binary, or ordered variables.
- Set `reference_label` to the exact display label that matches the coded reference value.
- Leave both fields blank for continuous variables.
- If CFG overrides a reference, the CFG value wins, but the dictionary should still reflect the intended default.

## Display Rules

- `display_order` should be globally consistent within a dataset.
- Indicators usually come first, then covariates, then outcomes, then reserved/future variables.
- Keep related variables adjacent.
- `display_group` should use simple stable groups such as `indicator`, `covariate`, `outcome`, `design`, `state`, or `future`.

## Description Rules

Use short functional descriptions, for example:

- `latent profile indicator`
- `covariate`
- `outcome variable`
- `time-invariant covariate`
- `baseline state`

Descriptions should explain the role of the variable in the analysis, not restate the label word-for-word.

## Categorical Value Label Rules

- Use `value_n` / `label_n` pairs only as far as needed.
- Keep the coded order aligned with the real source data.
- Do not skip codes inside the documented sequence unless the raw data itself skips them.
- If a variable has more than 6 categories, extend the schema only after confirming the pipeline also supports the added columns.

## Recommended Workflow

1. Start from the shared header template.
2. Add all variables that matter for the dataset.
3. Assign `role`, `type`, `use`, and `display_order`.
4. Fill labels and references.
5. Add value-label pairs for categorical variables.
6. Verify the file still matches the shared header exactly.

## Important Note

Do not put comments, explanation rows, or blank metadata sections inside `data_dictionary.csv`.
Keep explanation in this guide or in a nearby markdown note, and keep the CSV machine-readable at all times.
