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
