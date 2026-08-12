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
