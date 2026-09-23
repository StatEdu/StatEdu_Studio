# StatEdu Studio 1.2 Packaged Validation Notes

Use this file to record final validation evidence after building the public
1.2.0 package.

## Package

```text
Product: StatEdu Studio
Version: 1.2.0
Installer: D:\Program\Studio\dist\electron\StatEdu_Studio_Setup_1.2.0.exe
Installer SHA256: AB68645EAB246C6AE88230214F24750462399D523A42A6100C1D30A833CCF0DA
Blockmap SHA256: 9BC490604CF57323264357B516046863C815647D3F2C4F082E5B04095CCDB8E2
R runtime: R-4.5.3
Validation date: 2026-08-08
```

## Required Checks

| Check | Status | Notes |
|---|---|---|
| `scripts\validate_stabilization.ps1` passes | Pass | Core stabilization validation passed before packaging. |
| `scripts\release_preflight.ps1 -FullElectronSmoke` passes | Pass | Full stabilization validation, Shiny startup smoke, and full Electron release smoke passed. |
| `scripts\smoke_electron_release.ps1` passes | Pass | Verified package names, bundled version metadata, runtime pruning, hardware acceleration disabled by default, and bundled runtime resources. |
| Final installer is `StatEdu_Studio_Setup_1.2.0.exe` | Pass | Built at `D:\Program\Studio\dist\electron\StatEdu_Studio_Setup_1.2.0.exe`. |
| Final executable is `StatEdu Studio.exe` | Pass | Verified by Electron release smoke checks. |
| `scripts\smoke_electron_app_lifecycle.ps1` passes | Pass | Packaged app loaded bundled Shiny URL and closing Electron stopped the bundled Shiny process. |
| About > Version History shows `v1.2.0` first | Pass | Packaged changelog starts with `v1.2.0`, followed by `v1.1.3`, with no `-dev` version entries. |
| Mixed Repeated-Measures ANOVA opens and renders a result | Pass | Automated validation covers setup/result rendering; public release smoke passed against the packaged app. |
| Public build includes the custom model canvas | Pass | Packaged Electron launcher now sets `STATEDU_ENABLE_CUSTOM_MODEL_CANVAS=1`; the public menu includes `analysis_custom_model_canvas` in Regression / Models with Korean label `매개·조절 사용자 정의 모델`. |
| Public text contains no deferred feature claims | Pass | Public notes explicitly exclude license activation, edition gates, and in-app updates. |

Packaged validation status: Automated package checks passed for public deployment.

## Development Package Snapshot: 1.2.2-dev

```text
Product: StatEdu Studio Dev
Version: 1.2.2-dev
Installer: D:\Program\Studio\dist\electron\StatEdu_Studio_Dev_Setup_1.2.2-dev.exe
Installer SHA256: 1503955BF64FA6E3D94310C86793ADEE8CF3175260386225BD2F73D320380783
Installer bytes: 321539854
Blockmap SHA256: D72D4B6DF1221F3827D0FA7C67974FAF5486473D49F3227693BAF595F7F77AD3
Blockmap bytes: 322362
R runtime: R-4.5.3
Validation date: 2026-08-12
```

| Check | Status | Notes |
|---|---|---|
| `scripts\validate_cfa_all.R` passes | Pass | CFA canvas, reporting/export, and external reference comparison validations passed. |
| `scripts\validate_stabilization.ps1 -Full` passes | Pass | Full stabilization suite includes CFA aggregate validation and survival validation. |
| `scripts\release_preflight.ps1 -FullElectronSmoke` passes | Pass | Full stabilization validation, Shiny startup smoke, Electron release smoke, bundled app metadata, installer artifact, runtime pruning, and bundled modules passed. |
| Dev installer is `StatEdu_Studio_Dev_Setup_1.2.2-dev.exe` | Pass | Built at `D:\Program\Studio\dist\electron\StatEdu_Studio_Dev_Setup_1.2.2-dev.exe`. |
| Packaged executable metadata is `StatEdu Studio Dev` | Pass | Verified by full Electron smoke checks against `win-unpacked`. |
| Required SEM/CFA package dependencies are bundled | Pass | Local `seminr 2.5.0` dependency was installed before packaging; bundled runtime smoke passed. |
| CFA capture model hook smoke passes | Pass | `scripts\smoke_shiny_app.ps1` passed with `STATEDU_CAPTURE_CFA_MODEL_FILE=D:\Program\Studio\outputs\cfa_qa_two_factor.stmodel` and `STATEDU_CAPTURE_CFA_RUN=yes`; `scripts\validate_cfa_all.R` also covers the hook. |
| CFA theta ordinal bootstrap validation passes | Pass | `scripts\validate_cfa_all.R` covers theta-parameterized WLSMV ordered-indicator AVE/reliability bootstrap. |
| CFA BCa/percentile CI validation passes | Pass | `scripts\validate_cfa_all.R` covers percentile and BCa CI paths for AVE/reliability bootstrap and HTMT bootstrap. |
| CFA bootstrap progress/cancel callback validation passes | Pass | `scripts\validate_cfa_all.R` covers progress callbacks and cooperative cancel paths for AVE/reliability bootstrap and HTMT bootstrap; user-facing packaged UI cancel control remains pending. |

Development package status: Automated package checks passed after the CFA capture-hook rebuild; visual CFA workflow and native picker QA remain manual checks before promoting a public release build.

## Development Package Snapshot: 1.2.3-dev

```text
Product: StatEdu Studio Dev
Version: 1.2.3-dev
Installer: C:\StatEdu\Studio\dist\electron\StatEdu_Studio_Dev_Setup_1.2.3-dev.exe
Installer SHA256: A6258BE4E1557F9AC426C78915144E37F47EAA228444822F26C41697A7EC33E2
Installer bytes: 331304922
Blockmap SHA256: 4A4864B235E5BA9E4112793F8E085884D43AF8CB4572315FB4261E75FAA423CD
Blockmap bytes: 333517
Electron: 43.4.0
electron-builder: 26.15.3
R runtime: R-4.5.3
Validation date: 2026-08-19
```

| Check | Status | Notes |
|---|---|---|
| `scripts\validate_stabilization.ps1 -Full` passes | Pass | Full suite passed before packaging, including all dedicated SEM validations. |
| `scripts\smoke_electron_release.ps1` passes | Pass | Package names, pinned Electron dependencies, bundled version, licenses, pruned R runtime, and bundled module loading passed. |
| `scripts\smoke_electron_app_lifecycle.ps1` passes | Pass | Packaged app reached the bundled Shiny URL; closing Electron stopped the bundled R process. |
| Windows bundled-R UTF-8 startup | Pass | Electron now overrides inherited Linux-style locale values with `English_United States.utf8` for the R probe and Shiny process. |
| Electron ASAR integrity boundary | Pass | Electron main code is packaged in `app.asar`; executable R, Shiny application resources, and package metadata are explicitly placed in `app.asar.unpacked`. Release smoke verifies both paths and lifecycle smoke verifies runtime resolution. |
| Windows executable resource editing | Pass | `signExecutable: false` disables code signing only; electron-builder applies the icon, product metadata, version resources, and ASAR integrity without the former custom `afterPack` rcedit hook. |
| npm/pnpm build-runner fallback | Pass | `build_electron_release.ps1` completed with explicit `NodePath` and `PnpmPath`, using pinned npm 10.9.2 through pnpm; package structure and lifecycle smoke checks passed. |
| Build-time R locale isolation | Pass | Runtime pruning, dependency discovery, and license generation run with a Windows UTF-8 locale scoped to each R subprocess; inherited locale values are restored and the build emits no `C.UTF-8` warnings. |
| npm dependency security audit | Pass | Exact-pin updates to Electron 43.4.0 and electron-builder 26.15.3 reduced findings from 13 to 0. The lockfile was reproduced with `npm ci`; no automatic `npm audit fix --force` was applied. |
| Source-app CFA visual workflow | Pass | In-app browser QA loaded a three-factor model and 180-row data set, confirmed the explicit independent-cross-sectional sampling gate, ran CFA with zero errors or warnings, and rendered model overview, fit, validity, and measurement-model results. |
| Packaged native CFA/SEM model picker | Pass | Windows UI Automation exercised the actual packaged Electron `열기` dialog, not the browser fallback. The dialog exposed the `StatEdu Model Canvas` filter; `sample\cfa.stmodel` and `sample\sem.stmodel` restored the expected three-factor canvases, enabled Run from an initially disabled empty canvas, and continued through Run options to the Mardia/MLR estimator-decision step. Capture-hook and R-level validators separately cover completed fit execution. |
| Packaged ordinal WLSMV/theta workflow | Pass | Windows UI Automation loaded a 360-row, nine-indicator, five-category data set, marked every indicator ordinal, restored `sample\cfa.stmodel`, and executed CFA. The overview rendered `Estimator = DWLS`, `Parameterization = Theta`, `N = 360`, and `Converged = Yes`; zero canvas errors/warnings were present. The DWLS label is lavaan's internal representation of the requested WLSMV workflow. Base results stayed available during and after stopping the background HTMT bootstrap. |
| CFA high-order-factor affordance | Pass | The canvas toolbar visibly identifies the control with `2nd`, `Factor`, and `고차요인`, rather than relying on an unlabeled icon. |
| Packaged higher-order CFA execution and export | Pass | Windows UI Automation loaded `sample\cfa_higher_order.stmodel` with all 301 Holzinger-Swineford cases and ran MLR with zero canvas errors/warnings. The fit converged and was admissible. The UI reported general-factor standardized loadings/R2 of .873/.762 (`visual`), .525/.276 (`textual`), and .539/.290 (`speed`), plus omega-h=.552 for the nine-indicator unit-weighted score. The notes explicitly distinguish higher-order CFA from bifactor modeling and identify omega-h as model- and scoring-conditional. The live workbook contained `Higher_Order_Loadings` and `Higher_Order_Omega`; strict OpenXML import matched the UI values, all 26 sheets rendered to nonempty PNGs, and the workbook contained no spreadsheet error tokens. |
| CFA result downloads | Pass | After analysis, Audit JSON, reproducibility record, and Excel result-table links were visible, enabled, and backed by session download URLs. |
| Packaged multigroup CFA and strict Excel inspection | Pass | The packaged app ran the three-factor, nine-indicator CFA on all 301 Holzinger-Swineford observations grouped by `school` (Pasteur N=156; Grant-White N=145) with MLR. Configural, metric, scalar, and strict fits all converged and were admissible; the metric gate changes were ΔCFI=-.002, ΔRMSEA=-.004, and ΔSRMR=.004. The UI rendered six group-specific reliability/AVE rows, six group-specific HTMT rows, group residual diagnostics, and equality-constraint score tables. The live session exported a 33-sheet workbook that imported without repair in the strict OpenXML-based artifact tool, rendered every sheet, reported MLR consistently in `Overview` and `Report_Summary`, and contained no dangling drawing/VML relationships or spreadsheet error tokens. |
| Packaged SEM covariate effects and robust model comparison | Pass | Packaged Windows UI Automation ran the 301-row three-factor SEM with continuous `ageyr` targeting `zm` and final outcome `zy`. Both covariate paths rendered with estimates, standard errors, p values, confidence limits, and standardized coefficients. The comparison rendered research, covariate-adjusted, and Delta rows; MLR model rows used scaled chi-square/p and robust CFI/TLI/RMSEA, while Delta chi-square/df/p came from lavaan's robust/scaled likelihood-ratio difference test (44.734, 2, p<.001). The table and note explicitly identify these bases, and cleanup left zero packaged processes. |
| PLS/PLSc saturated-fit semantics and external benchmark readiness | Pass / fixed-benchmark external values pending | The result table identifies the displayed fit target as `saturated` and explicitly states that structurally restricted estimated-model values are not provided. Standard PLS is estimated as an uncorrected Mode A score model, while PLSc applies disattenuation only to declared common-factor blocks; the fixed Holzinger-Swineford benchmark consequently separates PLS from PLSc (SRMR .09209 vs .07895; d_G .15609 vs .11983; d_ULS .38159 vs .28050). A separate SmartPLS 4.1.1.8 TAM 100-row check matched all displayed PLS/PLSc fit values and seven path coefficients, including PLSc d_G reported as N/A. The generator emits full-precision StatEdu values, an external CSV template, and a SHA-256/version/settings manifest. Comparator and full SEM regression validation pass; the fixed Holzinger-Swineford SmartPLS/ADANCO run remains external evidence. |
| Independent cSEM matrix-formula comparison | Pass (source validation) | An isolated cSEM 0.6.1 installation independently reproduced StatEdu SRMR, d_G, and d_ULS for the fixed observed/implied matrix pair at tolerance 1e-12. The evidence record explicitly limits this result to formula equality and does not treat it as SmartPLS/ADANCO end-to-end equivalence. |
| CFA bootstrap CI defaults | Pass | HTMT output rendered bias-corrected (BC) intervals with 5,000/5,000 valid replicates. AVE/reliability bootstrap now also defaults to BC; percentile and slower BCa remain selectable sensitivity options. `scripts\validate_cfa_all.R` covers BC, percentile/BCa support, progress callbacks, cancellation, exports, and external-reference comparisons. |
| Packaged nondefault BCa/percentile intervals | Pass | A one-factor continuous CFA selected AVE/reliability BCa with 500 resamples. The packaged progress total was 545 (500 case resamples plus 45 leave-one-out jackknife fits), and AVE, CR, alpha, and omega rows rendered BCa limits with 468/500 valid replicates (93.6%, `Adequate`). A separate 301-row, nine-indicator, three-factor CFA selected HTMT Percentile with 1,000 resamples; all three factor pairs rendered percentile limits, one-sided upper limits, threshold comparisons, and 1,000/1,000 valid replicates (100%, `Adequate`). |
| Packaged CFA bootstrap progress | Pass | The rebuilt packaged Shiny resources displayed the standard progress panel during a 500-resample BC reliability run, including a determinate bar and live `completed/requested` plus valid-replicate counts (observed at 15/500 and 25/500). |
| CFA bootstrap background progress and cancel | Pass | CFA reliability, Bollen-Stine, and HTMT bootstraps run in a supervised background R process. Source and rebuilt packaged-resource browser QA confirmed that base CFA results appear immediately, a determinate combined progress panel exposes the active phase and completed/requested count, the Stop button terminates the worker, and base results remain visible after cancellation. Packaged QA exercised the default 5,000-resample HTMT job and confirmed worker cleanup. |
| Packaged SEM concurrent bootstrap progress and cancel | Pass | Final packaged-app browser QA ran a three-factor, nine-indicator SEM with 180 observations. Base results appeared before the 5,000-resample jobs completed; independent progress panels were visible for HTMT and structural effects, and each live Stop button terminated only its own worker while the model overview, fit, paths, validity, and point estimates remained available. |
| SEM cancellation semantics | Pass | HTMT and structural-effect result sections distinguish user cancellation from estimation failure. The final package displayed explicit user-stopped messages for both jobs and did not retain the former `could not be estimated` or `Canceled by user` failure wording. |
| Packaged lifecycle cleanup robustness | Pass | The lifecycle smoke uses shared-read access for the live startup log, accepts either the explicit Shiny-ready marker or the bundled loopback listening marker before renderer load, aligns its startup timeout with the 180-second product limit, always executes final cleanup, and finished with zero packaged Electron/R processes. |

Development package status: Automated SEM/CFA structure, bootstrap, release, and lifecycle checks pass. Packaged QA additionally confirms the native Windows CFA/SEM model picker, ordinal WLSMV/theta execution, higher-order CFA estimation and strict Excel export, robust SEM covariate effects and nested-model comparison, default BC and nondefault BCa/percentile intervals, concurrent SEM HTMT/structural-effect progress, independent cancellation, accurate cancellation messaging, retained base results, and zero packaged-process residue after cleanup.
