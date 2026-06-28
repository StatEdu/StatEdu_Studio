# StatEdu Studio 1.0 Version Bump Checklist

Use this checklist only when changing the stabilized 0.9.x line to the public
1.0.0 release line. The Electron build and smoke scripts switch package names
from beta to final release names when `VERSION` starts with `1.`.

## Preconditions

- Start from a clean git working tree.
- Keep the bundled Windows runtime on `R-4.5.3`.
- Do not add new analysis features during the version bump.
- Confirm `docs/RELEASE_MANUAL_QA.md` is ready to be completed against the
  rebuilt 1.0.0 package.

## Version Metadata

- `VERSION`: change to `1.0.0`.
- `README.md`: update the current version paragraph, citation example, and
  validation summary from `0.9.42` to `1.0.0`.
- `CITATION.cff`: update `version` and `date-released`.
- `packaging/electron/package.json`: confirm `version` is updated to `1.0.0`
  by the Electron build metadata sync before packaging.
- `docs/RELEASE_READINESS_STATUS.md`: change current version and validation
  notes after the 1.0.0 package is rebuilt and tested.
- `CHANGELOG.md`: add the 1.0.0 release section.

## Confirm Final Packaging Names

- Run the Electron build after changing `VERSION` to `1.0.0`.
- Confirm `packaging/electron/package.json` was synchronized to:
  - `name`: `statedu-studio`
  - `appId`: `com.statedu.studio`
  - `productName`: `StatEdu Studio`
  - `artifactName`: `StatEdu_Studio_Setup_${version}.${ext}`
  - `shortcutName`: `StatEdu Studio`
- Confirm the app title, app name, error dialogs, executable resource strings,
  installer artifact, shortcut, and unpacked executable use `StatEdu Studio`
  without `Beta`.
- Confirm `packaging/electron/main.js` uses the final display name when
  `VERSION` starts with `1.`.
- Confirm `packaging/electron/scripts/afterPack.js` writes final executable
  resource strings from the selected Electron `productName`.
- Confirm the final app data folder name is documented and used by the
  lifecycle smoke test.

## Build And Smoke Script Expectations

- Use `scripts/build_electron_release.ps1` for public 1.0 packaging. It
  delegates to the existing compatibility build implementation.
- Confirm the Electron build script selected the final release profile for
  `VERSION=1.0.0`.
- Confirm `scripts/smoke_electron_release.ps1` expects the final setup file
  pattern, executable name, and Windows version strings.
- Confirm `scripts/smoke_electron_app_lifecycle.ps1` expects the final
  executable path and app data log path.
- Confirm `scripts/validate_brand_metadata.R` passes with final non-beta
  package names after packaging metadata is synchronized.
- Update `scripts/validate_version_metadata.R` so validation expects 1.0.0
  metadata and no longer treats beta naming as the current package state.

## Distribution Output

- Remove old beta installers, old blockmaps, old EasyFlow artifacts, debug
  staging directories, and stale locked unpacked folders from `dist/electron`.
- Rebuild the package and confirm `dist/electron` contains only the final
  1.0.0 setup file, its `.blockmap`, and `win-unpacked`.
- Confirm the unpacked app executable is named `StatEdu Studio.exe`.

## Public Claims

- Confirm DOI `10.22934/statedu.studio` resolves before making a public
  citation claim.
- Confirm the DOI landing URL is `https://studio.statedu.com/citation/`.
- Confirm `studio.statedu.com` is live before making a public website claim.
- Do not claim Free/Pro/Latent gates, license activation, or in-app updates for
  1.0.0; those items are deferred in `docs/RELEASE_1_0_DECISION_LOG.md`.
- Confirm the packaged public 1.0 app sets `STATEDU_PUBLIC_RELEASE=1` and hides
  Longitudinal / Panel Models, Excel result export, and Word result export, unless an internal
  validation override explicitly enables them.

## Required Validation Sequence

Run these checks after all version and package-name changes are complete:

```powershell
scripts\validate_stabilization.ps1 -RscriptPath 'D:\Program\R\R-4.5.3\bin\x64\Rscript.exe'
scripts\build_electron_release.ps1 -RHome 'D:\Program\R\R-4.5.3'
scripts\smoke_electron_release.ps1
scripts\smoke_electron_app_lifecycle.ps1
scripts\release_preflight.ps1 -FullElectronSmoke
```
