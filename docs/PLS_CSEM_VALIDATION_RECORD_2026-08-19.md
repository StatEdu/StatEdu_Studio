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
