# StatEdu Studio 1.0 Version Bump Checklist

Use this checklist only when changing the stabilized 0.9.x line to the public
1.0.0 release line. Keep the current 0.9.42 beta packaging names until the
actual 1.0.0 version bump starts.

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
- `packaging/electron/package.json`: update `version` to `1.0.0`.
- `docs/RELEASE_READINESS_STATUS.md`: change current version and validation
  notes after the 1.0.0 package is rebuilt and tested.
- `CHANGELOG.md`: add the 1.0.0 release section.

## Remove Beta Packaging Names

- `packaging/electron/package.json`:
  - change `name` from `statedu-studio-beta` to the final release package name.
  - change `description` so it no longer says beta.
  - change `appId` from `com.statedu.studio.beta` to the final app id.
  - change `productName` from `StatEdu Studio Beta` to `StatEdu Studio`.
  - change installer `artifactName` from `StatEdu_Studio_Beta_Setup_*` to the
    final release artifact name.
  - change `shortcutName` from `StatEdu Studio Beta` to `StatEdu Studio`.
- `packaging/electron/main.js`:
  - remove `Beta` from the window title.
  - change `app.setName(...)` to `StatEdu Studio`.
  - remove `Beta` from error dialog titles.
  - confirm the app data folder name is final and documented.
- `packaging/electron/scripts/afterPack.js`:
  - update the executable path from `StatEdu Studio Beta.exe`.
  - update Windows version strings for `FileDescription`, `ProductName`,
    `InternalName`, and `OriginalFilename`.

## Build And Smoke Script Expectations

- Decide whether `scripts/build_electron_beta.ps1` is renamed to a release
  build script or kept as a compatibility wrapper.
- Update installer cleanup patterns in the Electron build script.
- Update `scripts/smoke_electron_release.ps1` for the final setup file pattern,
  executable name, and Windows version strings.
- Update `scripts/smoke_electron_app_lifecycle.ps1` for the final executable
  path and app data log path.
- Update `scripts/validate_brand_metadata.R` for final non-beta package names.
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
- Confirm the DOI landing URL is `https://studio.statedu.com`.
- Confirm `studio.statedu.com` is live before making a public website claim.
- Do not claim Free/Pro/Latent gates, license activation, or in-app updates for
  1.0.0; those items are deferred in `docs/RELEASE_1_0_DECISION_LOG.md`.

## Required Validation Sequence

Run these checks after all version and package-name changes are complete:

```powershell
scripts\validate_stabilization.ps1 -RscriptPath 'D:\Program\R\R-4.5.3\bin\x64\Rscript.exe'
scripts\build_electron_beta.ps1 -RHome 'D:\Program\R\R-4.5.3'
scripts\smoke_electron_release.ps1
scripts\smoke_electron_app_lifecycle.ps1
scripts\release_preflight.ps1 -FullElectronSmoke
```

If the build script is renamed for 1.0.0, run the renamed release build command
in place of `scripts\build_electron_beta.ps1`.
