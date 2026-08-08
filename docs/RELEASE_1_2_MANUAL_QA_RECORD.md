# StatEdu Studio 1.2 Manual QA Record

Use this file to record manual QA for the public 1.2.0 release candidate after
the installer is built. Automated lifecycle smoke has passed; visual and
workflow manual QA remain pending.

## Candidate

```text
Release candidate: 1.2.0
QA date:
Tester:
Installer: D:\Program\Studio\dist\electron\StatEdu_Studio_Setup_1.2.0.exe
R runtime: R-4.5.3
Validation command: scripts\smoke_electron_release.ps1; scripts\smoke_electron_app_lifecycle.ps1
```

## Manual Checks

| Check | Status | Notes |
|---|---|---|
| App launches from packaged `StatEdu Studio.exe` | Pass | `scripts\smoke_electron_app_lifecycle.ps1` loaded the bundled Shiny URL. |
| Navbar/About version displays `v1.2.0` | Pending | |
| About > Version History displays `v1.2.0` first | Pending | |
| Data import works with sample data | Pending | |
| Mixed Repeated-Measures ANOVA setup and result render correctly | Pending | |
| Inter-rater agreement recommended index appears first | Pending | |
| HTML/PDF export works for public release scope | Pending | |
| Excel/Word public exposure matches the documented public rule | Pending | |
| Custom model canvas is visible in Regression / Model | Pending | |
| About > Open Source Licenses opens | Pending | |
| App closes and background Shiny process stops | Pass | `scripts\smoke_electron_app_lifecycle.ps1` confirmed the bundled Shiny process stopped after Electron closed. |

Manual QA status: Automated lifecycle smoke passed; visual and workflow manual QA pending.
