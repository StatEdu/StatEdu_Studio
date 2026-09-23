# PLS fit formula validation against cSEM

## Validation record

- Date: 2026-08-19
- StatEdu source version: 1.2.3-dev
- R: 4.5.3
- Independent package: cSEM 0.6.1
- Installation scope: isolated temporary R library; not added to the bundled StatEdu runtime
- Validator: `scripts/validate_pls_fit_csem.R`
- Numeric tolerance: 1e-12

The validator passed by calling the non-exported cSEM matrix functions `calculateSRMR()`, `calculateDG()`, and `calculateDL()` on the same fixed observed and implied correlation matrices used by StatEdu.

| Metric | StatEdu value | cSEM comparison |
|---|---:|---|
| SRMR | 0.027748873851023207 | Equal within 1e-12 |
| d_G | 0.0012678708941371466 | Equal within 1e-12 |
| d_ULS | 0.007699999999999995 | Equal within 1e-12 |

Command used:

```powershell
$env:R_LIBS_USER = "$env:TEMP\statedu-csem-validation-lib"
& "C:\StatEdu\Studio\packaging\electron\runtime\R-4.5.3\bin\x64\Rscript.exe" `
  scripts\validate_pls_fit_csem.R
```

## Interpretation boundary

This test establishes package-level equality of the three matrix discrepancy formulas for a fixed matrix pair. It does not establish equality of PLS/PLSc scores, loadings, consistency corrections, model-implied correlation reconstruction, saturated-versus-estimated model handling, or bootstrap exact-fit inference across StatEdu, SmartPLS, and ADANCO. Those claims require the fixed external benchmark and actual version-recorded output from the proprietary program.

## 2026-08-21 follow-up reproduction

- GitHub base: `2e69ac8`, with the repository-fixture reproducibility correction under review
- R: 4.5.3
- cSEM: CRAN 0.6.1
- Installation: isolated temporary library; system and bundled runtime libraries were not modified
- Result: SRMR, d_G, and d_ULS matched the cSEM matrix functions within `1e-12`

The first isolated run intentionally exposed only the temporary library and therefore could not load the
existing Shiny dependency. The successful run supplied both the isolated cSEM library and the existing
read-only user library through `R_LIBS_USER`; cSEM and its added dependencies remained confined to the
temporary directory.
