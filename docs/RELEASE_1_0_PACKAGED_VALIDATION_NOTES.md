# StatEdu Studio 1.0 Packaged Validation Notes

Use this file to record the final validation evidence for the public 1.0.0
Windows package. Keep it with the release notes and manual QA record.

## Package

```text
Version: 1.0.0
Build date: 2026-06-27
Git commit: release-candidate commit; verify with `git log -1 --oneline` before publishing
Installer: dist/electron/StatEdu_Studio_Setup_1.0.0.exe
Installer SHA256: 5E9EC88A19ED99D79DF760E62DBE16C064073C642852785F19D6628436A2BDF7
Blockmap SHA256: 3188EFF70C2F8372A186A1C7A3E33CEE0B632C81C9A2FA44597AAE1FF0952C31
Unpacked executable: dist/electron/win-unpacked/StatEdu Studio.exe
Bundled R runtime: R-4.5.3 from D:\Program\R\R-4.5.3
Build machine: StatEdu
```

## Required Commands

| Command | Status | Evidence / Notes |
|---|---|---|
| `scripts\validate_stabilization.ps1 -RscriptPath 'D:\Program\R\R-4.5.3\bin\x64\Rscript.exe'` | Pass | Covered through `scripts\release_preflight.ps1 -FullElectronSmoke` on 2026-06-25. |
| `scripts\release_preflight.ps1` | Pass | Passed for 1.0.0 metadata before packaging on 2026-06-25. |
| Electron package build command | Pass | `scripts\build_electron_release.ps1 -RHome 'D:\Program\R\R-4.5.3'`; final rebuild after DOI/public-citation cleanup succeeded using `-SkipRuntimeCopy -SkipNpmInstall`. |
| `scripts\get_release_checksums.ps1` | Pass | Installer and blockmap SHA256 recorded above. |
| `scripts\smoke_electron_release.ps1` | Pass | Unpacked output, final executable metadata, bundled app version, runtime, and notices passed. |
| `scripts\smoke_electron_app_lifecycle.ps1` | Pass | Packaged Electron app loaded bundled Shiny URL and closing the app stopped bundled Shiny. |
| `scripts\release_preflight.ps1 -FullElectronSmoke` | Pass | Passed on 2026-06-27 against `StatEdu_Studio_Setup_1.0.0.exe` and `win-unpacked/StatEdu Studio.exe`; latest log: `logs\release_preflight_full_20260627_235622.log`. |

## Package Contents

| Check | Status | Evidence / Notes |
|---|---|---|
| Final executable is `StatEdu Studio.exe` for public 1.0 | Pass | Verified by `scripts\smoke_electron_release.ps1`. |
| Installer artifact uses final non-beta name | Pass | `dist/electron/StatEdu_Studio_Setup_1.0.0.exe`. |
| No legacy EasyFlow installer artifacts remain in `dist/electron` | Pass | `dist/electron` contains only `StatEdu_Studio_Setup_1.0.0.exe`, its `.blockmap`, and `win-unpacked`. |
| `win-unpacked` launches successfully | Pass | Verified by `scripts\smoke_electron_app_lifecycle.ps1`. |
| App opens through `127.0.0.1` | Pass | Lifecycle smoke reached the bundled Shiny URL. |
| Closing the Electron window stops bundled R/Shiny | Pass | Verified by lifecycle smoke. |
| Open Source Licenses page displays bundled notices | Pass | Browser QA showed Open Source Licenses heading, third-party notices, and license report table. |

## Runtime And Notices

| Check | Status | Evidence / Notes |
|---|---|---|
| Bundled runtime is `R-4.5.3` | Pass | Verified in `dist/electron/win-unpacked/resources/app/runtime/R-4.5.3`. |
| `THIRD-PARTY-NOTICES.txt` present | Pass | Verified by `scripts\smoke_electron_release.ps1`. |
| `license_report.csv` present | Pass | Verified by `scripts\smoke_electron_release.ps1`. |
| `LICENSES/` present | Pass | Verified by `scripts\smoke_electron_release.ps1`; 101 files. |
| `LICENSE.electron.txt` present in Electron output | Pass | Verified by `scripts\smoke_electron_release.ps1`. |
| `LICENSES.chromium.html` present in Electron output | Pass | Verified by `scripts\smoke_electron_release.ps1`. |
| `.studio` file association metadata is present | Pass | `scripts\smoke_electron_release.ps1` verifies the Electron `fileAssociations` entry for `.studio` and the source icon `packaging\electron\build\studio-file.ico`. |
| Packaged startup timing is acceptable | Pass | Packaged `dist/electron/win-unpacked/StatEdu Studio.exe` reached the Shiny Data tab in 2.67 seconds on 2026-06-27; startup log reported Shiny ready in 1807 ms and BrowserWindow load in 395 ms. |

## Data And Export Smoke

| Check | Status | Evidence / Notes |
|---|---|---|
| Import path with spaces works | Pass / Pending picker QA | Direct data IO smoke read a CSV from a path containing spaces; packaged file-picker confirmation remains manual. |
| Import path with Korean characters works | Pass / Pending picker QA | Direct data IO smoke read a CSV from a path containing Korean characters; packaged file-picker confirmation remains manual. |
| One Data Editor workflow completes | Pending manual QA | Automated Data Editor validations passed; packaged visual workflow remains manual QA. |
| One analysis workflow completes | Pass | Packaged-app browser QA imported `03_KSWL.sav`, completed Data Step 2 and Step 3, and ran t-test / ANOVA (`x1` by `group`) successfully. |
| HTML export works | Pass | Export writer smoke passed and packaged native Windows Save HTML dialog created `sample\StatEdu_Studio_results_manual.html` (925011 bytes) next to the loaded data file. |
| PDF export works | Pass | Export writer smoke passed and packaged native Windows Save PDF dialog created `sample\StatEdu_Studio_results_manual.pdf` (454185 bytes) next to the loaded data file. |
| Public 1.0 hides Excel result export | Pass | Browser QA with `STATEDU_PUBLIC_RELEASE=1` showed Save HTML and Save PDF, with no Save Excel button. |

## Public Gate Evidence

| Check | Status | Evidence / Notes |
|---|---|---|
| `studio.statedu.com` is live | Pass | Initial StatEdu Studio citation/product landing page is live at `https://studio.statedu.com/`. |
| DOI `10.22934/statedu.studio` resolves to the StatEdu Studio citation landing page | Pass | `https://doi.org/10.22934/statedu.studio` resolves to `https://studio.statedu.com/citation/`. |
| Public release notes contain no deferred feature claims | Pass | Draft release notes list deferred surfaces under Not Included and do not claim gated editions, license activation, in-app updates, Longitudinal / Panel Models, Excel result export, or Word result export as public 1.0 features. |
| Manual QA record is complete | Pass | Packaged import, analysis, and native HTML/PDF save-dialog workflow passed; `docs\RELEASE_1_0_MANUAL_QA_RECORD.md` is complete. |

## Final Result

```text
Packaged validation status: Complete
Blocking failures: None currently recorded.
Non-blocking follow-ups:
Approved package: Yes
Approver:
Date: 2026-06-25
```
