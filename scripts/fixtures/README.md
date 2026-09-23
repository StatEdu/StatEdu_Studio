# Validation fixtures

`crosstab_gamma_reference.R` preserves the pre-optimization row-by-row Gamma
calculation for exact numeric, diagnostic, and RNG comparisons. It is repository
code, not external data. Keep its original arithmetic when changing the optimized
implementation; `scripts/validate_crosstab_gamma.R` exercises its boundary cases.

`crosstab_trend_reference.R` preserves the ordered trend implementation before
removing the repeated data frame. `scripts/validate_crosstab_trend_vectors.R`
compares results and diagnostics, and verifies the same ordered vectors reach cor().

`crosstab_display_reference.R` preserves the scalar percentage formatting path.
`scripts/validate_crosstab_percent_format.R` compares complete display tables,
diagnostics and RNG across output flags, decimal separators and edge values.

`crosstab_row_frame_reference.R` preserves the display path after vectorized
percentage formatting, before using list2DF for completed rows with ordinary
column names. `scripts/validate_crosstab_row_frame.R` also covers unusual labels.

`survival_validation.csv` is a deterministic synthetic fixture for the Kaplan-Meier and Cox regression validation scripts. It contains no patient or external-package data.

The fixture deliberately includes two grouping levels, three performance-status levels, continuous ages, censoring, events, tied analysis horizons, and follow-up beyond 400 time units. Its purpose is stable engine and rendering regression testing across the host and bundled R runtimes; it is not an example clinical data set.

## Ohio longitudinal reference data

`longitudinal_ohio.rds` preserves the existing longitudinal validation input from
`geepack::ohio`, geepack 1.3.13 (GPL >= 3). Package authors are Søren Højsgaard,
Ulrich Halekoh, and Jun Yan; contributor Claus Thorn Ekstrøm. A copy of GPL v3
is included in `GPL-3-geepack.txt`. This is external reference data, not synthetic
data generated for this repository. The fixture is used by validation only.

The object contains 2,148 rows and the four integer columns `resp`, `id`, `age`,
`smoke` in their original row order, with original data-frame attributes.
The bundled runtime may omit package example data; using this snapshot keeps
the same input without loading packages from a developer's personal library.

Reproduce with a full installation of geepack 1.3.13 and R 4.5.3:

```r
env <- new.env(parent = emptyenv())
utils::data(list = "ohio", package = "geepack", envir = env)
original <- as.data.frame(env$ohio)[, c("resp", "id", "age", "smoke"), drop = FALSE]
saveRDS(original, "scripts/fixtures/longitudinal_ohio.rds", version = 2)
stopifnot(identical(original, readRDS("scripts/fixtures/longitudinal_ohio.rds"), num.eq = FALSE))
```

SHA256: `3da7f5bea90f54e48ab35a30b71f1decfd0ded73a3d4e38776dec4119270e30f`.
The validation checks this hash, dimensions, and column types before fitting.
