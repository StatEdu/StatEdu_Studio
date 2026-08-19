# StatEdu Studio 1.2.3 Final Packaged Manual QA Record

This record is for the final non-development `1.2.3` Windows package only. Development-build automation and the historical 1.2.0 record may be cited as corroborating evidence, but they do not change a row in this file from `Pending` to `Pass`.

Overall status: blocked / pending final public package

## Candidate identity

```text
Release candidate: 1.2.3
QA date:
Tester:
Git commit:
Installer: dist/electron/StatEdu_Studio_Setup_1.2.3.exe
Installer SHA-256:
Blockmap: dist/electron/StatEdu_Studio_Setup_1.2.3.exe.blockmap
Blockmap SHA-256:
R runtime: R-4.5.3
Windows version:
Display scale:
SmartPLS/ADANCO evidence record:
```

## Automated prerequisites

Every row must pass against the same commit used to build the installer.

| Check | Status | Evidence |
|---|---|---|
| `scripts/validate_stabilization.ps1 -Full` | Pending | |
| `scripts/release_preflight.ps1` | Pending | |
| `scripts/release_preflight.ps1 -FullElectronSmoke` | Pending | |
| `scripts/smoke_electron_app_lifecycle.ps1` | Pending | |
| `scripts/validate_sem_release_promotion.R` | Pending | |
| Git worktree and release evidence are clean/complete | Pending | |

## Package identity, startup, and shutdown

| Manual check | Expected result | Status | Evidence / notes |
|---|---|---|---|
| Launch installed `StatEdu Studio.exe` | App opens without a console window or startup error | Pending | |
| Navbar, About, and version history | All display public `v1.2.3`; no `-dev` label | Pending | |
| About > Open Source Licenses | Bundled notices open and are readable | Pending | |
| Local connection | App uses the authenticated `127.0.0.1` Shiny session | Pending | |
| Close the Electron window | Electron and bundled R/Shiny processes terminate | Pending | |

## Language and navigation

| Manual check | Expected result | Status | Evidence / notes |
|---|---|---|---|
| Switch Korean to English and restart | Selection persists; header, Result, Latent, and every Analysis submenu are English | Pending | |
| Switch English to Korean and restart | Selection persists; the same surfaces are Korean | Pending | |
| Open every first-level Analysis group in both languages | No blank page, missing submenu, mixed language, or stale selection | Pending | |
| CFA and SEM running-state UI | Status/progress placement and cancellation behavior are consistent between CFA and SEM | Pending | |

## Data and file workflow

| Manual check | Expected result | Status | Evidence / notes |
|---|---|---|---|
| Import CSV and Excel sample files | Data and variable metadata render correctly | Pending | |
| Import from a path containing spaces | Import and subsequent analysis succeed | Pending | |
| Import from a path containing Korean characters | Import and subsequent analysis succeed | Pending | |
| Save and load settings | Only `.studio` is offered and the model/data reconnect message is accurate | Pending | |
| Native `.stmodel` open dialog | CFA, SEM, and PLS models restore nodes, paths, construct types, and analysis settings | Pending | |

## CFA workflow

| Manual check | Expected result | Status | Evidence / notes |
|---|---|---|---|
| Higher-order-factor toolbar control | The control visibly identifies `2nd` / higher-order factor rather than relying on an ambiguous icon | Pending | |
| Higher-order CFA execution | General-factor loadings, lower-order R2/residual variance, and conditional omega-h render with limitations | Pending | |
| Higher-order CFA Excel export | Dedicated higher-order sheets match displayed values and open without repair | Pending | |
| Covariate-adjusted CFA | Covariate coefficients render for every declared target | Pending | |
| CFA model comparison | Research, covariate-adjusted, and Delta rows use estimator-matched statistics | Pending | |
| Multigroup CFA | Configural/metric/scalar/strict results and group-specific reliability, AVE, HTMT, and residual diagnostics render | Pending | |
| Invariance gate | Structural comparisons remain blocked until the required measurement-invariance gate passes | Pending | |
| Ordinal CFA | WLSMV/DWLS with theta parameterization is labeled consistently | Pending | |
| CFA bootstrap method | Default bias-corrected method and selectable percentile/BCa method are labeled in results | Pending | |
| CFA bootstrap progress and Stop | Progress updates, cancellation is independent, and base results remain visible | Pending | |

## SEM workflow

| Manual check | Expected result | Status | Evidence / notes |
|---|---|---|---|
| Continuous/categorical covariates | Each declared covariate effect reports B, SE, p, standardized effect where defined, and target | Pending | |
| Research versus adjusted fit | Both models and estimator-correct Delta statistics render; robust/scaled chi-squares are not subtracted directly | Pending | |
| Indirect effects | Table title and body are English, Path has sufficient width, and headers are centered | Pending | |
| Bootstrap inference | Chosen method, requested/valid replicates, valid ratio, CI limits, progress, and cancellation are reported | Pending | |
| Moderated mediation | Conditional indirect effects and index labels match the drawn paths | Pending | |
| Causal-language boundary | UI/export describes theory-directed associations and does not claim causal identification from cross-sectional data | Pending | |
| Recommendation wording | Recommended-model assistance is not labeled as fully automated SEM | Pending | |

## PLS-SEM and PLSc workflow

| Manual check | Expected result | Status | Evidence / notes |
|---|---|---|---|
| Reflective PLS model | PLS saturated SRMR, d_G, and d_ULS are numeric and the saturated basis is explicit | Pending | |
| Eligible PLSc model | PLSc runs and reports distinct consistency-corrected saturated diagnostics | Pending | |
| Composite/formative construct with PLSc request | PLSc is rejected with a precise construct-specific explanation; standard PLS remains available | Pending | |
| Mixed construct model | Only the supported reflective measurement subset is used for discrepancy diagnostics and the basis is reported | Pending | |
| External comparison claim | Result/release text says agreement only within recorded external output precision and cites the exact software version | Pending | |

## Export and public-scope workflow

| Manual check | Expected result | Status | Evidence / notes |
|---|---|---|---|
| Save HTML through native dialog | File is nonempty, opens, and matches visible results | Pending | |
| Save PDF through native dialog | File is nonempty, renders, and matches visible results | Pending | |
| Verify native exports | `scripts/verify_manual_export_dialog_outputs.ps1` passes for the saved HTML/PDF | Pending | |
| Excel/Word exposure | Public visibility matches the documented release rule | Pending | |
| Result collection | Saved result reopens with tables, notes, estimator, and version intact | Pending | |
| Public filenames | Exported files use StatEdu Studio naming | Pending | |

## Failure record

Every failed row must be linked here and remain `Fail` until the fix and re-run are recorded.

| QA row | Defect | Fix commit | Re-run command | Re-run result |
|---|---|---|---|---|
| | | | | |

## Final sign-off

```text
Manual QA status: Pending
Open failures:
Tester sign-off:
Sign-off time:
Release approver:
Approval time:
```

This file may be marked complete only when every required row is `Pass` or has a justified `NA`, all failures are closed, the recorded installer/blockmap hashes match the promotion manifest, and the final sign-off is complete.
