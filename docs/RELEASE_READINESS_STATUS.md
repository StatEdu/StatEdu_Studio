# StatEdu Studio Release Readiness Status

Last reviewed: 2026-06-25

Current version: 1.0.0

## Local Validation

The local stabilization checks are passing for the current branch.

- `scripts/validate_stabilization.ps1 -Full`: passed
- `scripts/smoke_shiny_app.ps1`: passed
- `scripts/smoke_electron_release.ps1 -SkipUnpackedChecks`: passed
- `scripts/release_preflight.ps1`: passed
- `scripts/smoke_electron_release.ps1` without `-SkipUnpackedChecks`: passed against the rebuilt 0.9.42 Electron output with bundled `R-4.5.3`.
- `scripts/release_preflight.ps1 -FullElectronSmoke`: passed
- `scripts/smoke_electron_app_lifecycle.ps1`: passed against `dist/electron/win-unpacked/StatEdu Studio Beta.exe`
- `scripts/release_preflight.ps1`: passed on 2026-06-25 before the 1.0.0 metadata bump.
- `scripts/smoke_electron_release.ps1`: passed on 2026-06-25 against the rebuilt 1.0.0 final Electron output.
- `scripts/smoke_electron_app_lifecycle.ps1`: passed on 2026-06-25 against `dist/electron/win-unpacked/StatEdu Studio.exe`.
- `scripts/release_preflight.ps1 -FullElectronSmoke`: passed on 2026-06-25 against the rebuilt 1.0.0 final Electron output.
- Git working tree: pending 1.0.0 release-candidate metadata changes

## Confirmed Local Release Hygiene

- Version metadata is synchronized across `VERSION`, `README.md`, `CITATION.cff`, and Electron package metadata for 1.0.0.
- Tracked generated artifacts, local settings, `.Rhistory`, `.RData`, logs, temporary files, and Electron staging directories are blocked by release hygiene validation.
- Shiny startup, Electron security settings, settings dialogs, UI layout contracts, data IO, data editor workflows, and core analysis outputs are covered by automated validation.
- Full Electron smoke checks that bundled app metadata and installer artifact names match the current `VERSION`.
- Packaged Electron lifecycle smoke confirms bundled Shiny loads and stops when the Electron window closes.
- The previous `dist/electron` output was rebuilt for 0.9.42 with bundled `R-4.5.3`; it is no longer the current public 1.0.0 package after the version bump.
- The current 1.0.0 Electron output was rebuilt with bundled `R-4.5.3`: `StatEdu_Studio_Setup_1.0.0.exe`, its `.blockmap`, and `win-unpacked/StatEdu Studio.exe`.
- Confirm the rebuilt Electron output uses final 1.0 release names across Electron display name, package metadata, installer artifact, shortcut name, executable resource strings, and smoke-test expectations: passed by `scripts/smoke_electron_release.ps1`.
- Installer SHA256: `F05AEB7D597678BF632683836A9B8C93114721588902323379A743A0F1A1A0DA`.
- Blockmap SHA256: `D7DD36A347BC76DEDEF2084A683B28DB9DAFF5F06DDC74CD16C3A42DCC67FD77`.
- `dist/electron` contains only the final 1.0.0 setup file, its `.blockmap`, and `win-unpacked`.
- The public 1.0 Electron build entry point is `scripts/build_electron_release.ps1`,
  which delegates to the compatibility build implementation and selects final
  package names from `VERSION`.
- The 1.0 feature-freeze rule is documented: no new analysis features before 1.0 unless required for correctness, data safety, packaging, or validation coverage.
- The 1.0 version-bump checklist is tracked in `docs/RELEASE_1_0_VERSION_BUMP_CHECKLIST.md` and enforced by `scripts/validate_version_metadata.R`.

## Items Still Required Before Public 1.0

These are not fully resolved by local validation and must be checked before publishing a public 1.0 installer.

- Complete manual packaged-app QA against the rebuilt 1.0.0 release candidate.
- Complete `docs/RELEASE_MANUAL_QA.md` for visual consistency, file dialogs, packaged runtime behavior, and export handoffs.
- Keep the completed manual QA record with the release notes and validation artifacts.
- Launch the generated executable in `dist/electron/win-unpacked` and manually confirm app startup, About > Open Source Licenses, import, analysis, export, and close behavior.
- Build and publish `studio.statedu.com`; the homepage infrastructure is not built yet.
- Register and verify DOI `10.22934/statedu.studio`; DOI infrastructure is not built yet.
- Confirm the DOI landing URL resolves to `https://studio.statedu.com`.
- README, `CITATION.cff`, and About do not publish a DOI citation line until the DOI landing page is live.
- Complete `docs/RELEASE_1_0_VERSION_BUMP_CHECKLIST.md` against the rebuilt 1.0.0 package.
- Free/Pro/Latent gates, license activation, and in-app updates are explicitly deferred for 1.0 in `docs/RELEASE_1_0_DECISION_LOG.md`; do not claim them in public release materials.
- Public 1.0 also hides Longitudinal / Panel Models, Excel result export, and Word result export through `STATEDU_PUBLIC_RELEASE=1`; do not claim those surfaces in public release materials.
- Decide whether remaining installer/download infrastructure items in `docs/RELEASE_1_0_DISTRIBUTION_LICENSE_PLAN_KO.md` are implemented for 1.0 or explicitly deferred.
- Record any additional implementation or deferral decisions in `docs/RELEASE_1_0_DECISION_LOG.md`.
- Prepare final public release notes and packaged validation notes.
- Public release notes draft and packaged validation notes templates are tracked in `docs/RELEASE_1_0_PUBLIC_NOTES_DRAFT.md` and `docs/RELEASE_1_0_PACKAGED_VALIDATION_NOTES.md`; they still require final 1.0 package evidence before publication.

## Repository Check

The configured GitHub repository is reachable by git:

`https://github.com/StatEdu/StatEdu_Studio_dev.git`
