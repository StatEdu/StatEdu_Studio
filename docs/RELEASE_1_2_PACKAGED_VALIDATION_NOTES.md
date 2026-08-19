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
Installer SHA256: A39E435240E8B2F3E841FA610036D86A1BA4084D2596B691872E06D859DD561E
Installer bytes: 331284693
Blockmap SHA256: 23DBC0289B92DCBB3B2D411E147A4F0EA9AD316B2173423B1E1A759DF40DFE9B
Blockmap bytes: 333580
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
| CFA high-order-factor affordance | Pass | The canvas toolbar visibly identifies the control with `2nd`, `Factor`, and `고차요인`, rather than relying on an unlabeled icon. |
| CFA result downloads | Pass | After analysis, Audit JSON, reproducibility record, and Excel result-table links were visible, enabled, and backed by session download URLs. |
| CFA bootstrap CI defaults | Pass | HTMT output rendered bias-corrected (BC) intervals with 5,000/5,000 valid replicates. AVE/reliability bootstrap now also defaults to BC; percentile and slower BCa remain selectable sensitivity options. `scripts\validate_cfa_all.R` covers BC, percentile/BCa support, progress callbacks, cancellation, exports, and external-reference comparisons. |

Development package status: Automated structure and lifecycle checks pass, and the source-app CFA workflow has completed focused browser QA. Packaged-app bootstrap progress/cancel presentation and the remaining SEM-specific visual workflows still require focused QA before public promotion.
