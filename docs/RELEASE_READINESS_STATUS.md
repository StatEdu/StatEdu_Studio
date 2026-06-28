# StatEdu Studio Release Readiness Status

Last reviewed: 2026-06-28

Current version: 1.0.1

## Current 1.0.1 Package Snapshot

- Built package: `dist/electron/StatEdu_Studio_Setup_1.0.1.exe`
- Installer SHA256: `6E408CE402D6DD802B745675485A81C7960E6B8676F44221FDEF81A41CBADC1E`
- Blockmap SHA256: `711C04A393DBD9061A00568F1F16FB4E2CFD6D358915FF76DF0EE154457ABCF6`
- Package rebuilt on 2026-06-28 to include STATEDU environment variable rebranding, navbar static language translation, external Help links opening in system default browser, and startup optimizations (JIT compilation + static label cache for faster page refresh).
- `scripts/smoke_electron_release.ps1 -RepoRoot .`: passed on 2026-06-28 against the rebuilt 1.0.1 output.
- Website release manifest should be updated to 1.0.1 with the new installer SHA256 and `releases/release-notes/1.0.1.html`.

## Local Validation

The local stabilization checks are passing for the current branch.

- `scripts/validate_stabilization.ps1 -Full`: passed on 2026-06-28 (post-rebrand merge)
- `scripts/release_preflight.ps1`: passed on 2026-06-28 (post-rebrand rebuild)
- `scripts/smoke_electron_release.ps1 -RepoRoot .`: passed on 2026-06-28 against rebuilt 1.0.1 with bundled `R-4.5.3` (external link fix included)
- `scripts/smoke_shiny_app.ps1`: passed
- `scripts/smoke_electron_release.ps1 -SkipUnpackedChecks`: passed
- `scripts/smoke_electron_release.ps1` without `-SkipUnpackedChecks`: passed against the rebuilt 0.9.42 Electron output with bundled `R-4.5.3`.
- `scripts/release_preflight.ps1 -FullElectronSmoke`: passed
- `scripts/smoke_electron_app_lifecycle.ps1`: passed against `dist/electron/win-unpacked/StatEdu Studio Beta.exe`
- `scripts/release_preflight.ps1`: passed on 2026-06-25 before the 1.0.0 metadata bump.
- `scripts/smoke_electron_release.ps1`: passed on 2026-06-25 against the rebuilt 1.0.0 final Electron output.
- `scripts/smoke_electron_app_lifecycle.ps1`: passed on 2026-06-25 against `dist/electron/win-unpacked/StatEdu Studio.exe`.
- `scripts/release_preflight.ps1 -FullElectronSmoke`: passed on 2026-06-25 against the rebuilt 1.0.0 final Electron output.
- Git working tree: 1.0.0 release-candidate metadata changes committed; recheck after the latest manual QA record updates.

## Confirmed Local Release Hygiene

- Version metadata is synchronized across `VERSION`, `README.md`, `CITATION.cff`, and Electron package metadata for 1.0.0.
- Tracked generated artifacts, local settings, `.Rhistory`, `.RData`, logs, temporary files, and Electron staging directories are blocked by release hygiene validation.
- Shiny startup, Electron security settings, settings dialogs, UI layout contracts, data IO, data editor workflows, and core analysis outputs are covered by automated validation.
- Full Electron smoke checks that bundled app metadata and installer artifact names match the current `VERSION`.
- Packaged Electron lifecycle smoke confirms bundled Shiny loads and stops when the Electron window closes.
- The previous `dist/electron` output was rebuilt for 0.9.42 with bundled `R-4.5.3`; it is no longer the current public 1.0.0 package after the version bump.
- The current 1.0.0 Electron output was rebuilt with bundled `R-4.5.3`: `StatEdu_Studio_Setup_1.0.0.exe`, its `.blockmap`, and `win-unpacked/StatEdu Studio.exe`.
- Confirm the rebuilt Electron output uses final 1.0 release names across Electron display name, package metadata, installer artifact, shortcut name, executable resource strings, and smoke-test expectations: passed by `scripts/smoke_electron_release.ps1`.
- Installer SHA256: `5E9EC88A19ED99D79DF760E62DBE16C064073C642852785F19D6628436A2BDF7`.
- Blockmap SHA256: `3188EFF70C2F8372A186A1C7A3E33CEE0B632C81C9A2FA44597AAE1FF0952C31`.
- `dist/electron` contains only the final 1.0.0 setup file, its `.blockmap`, and `win-unpacked`.
- Packaged-app browser QA confirmed the final `win-unpacked/StatEdu Studio.exe`
  loads through `127.0.0.1`, imports data, completes Data Step 2 and Step 3,
  runs t-test / ANOVA, and exposes public 1.0 Save HTML and Save PDF buttons.
- Packaged startup timing was checked on 2026-06-27: the final
  `win-unpacked/StatEdu Studio.exe` reached the Shiny Data tab in 2.67 seconds
  with Shiny ready in 1807 ms and BrowserWindow load in 395 ms.
- `.studio` file association metadata is present in the Electron package
  configuration and verified by `scripts/smoke_electron_release.ps1`; packaged
  logs also showed cold-start and second-instance `.studio` open handling.
- The public 1.0 Electron build entry point is `scripts/build_electron_release.ps1`,
  which delegates to the compatibility build implementation and selects final
  package names from `VERSION`.
- The 1.0 feature-freeze rule is documented: no new analysis features before 1.0 unless required for correctness, data safety, packaging, or validation coverage.
- The 1.0 version-bump checklist is tracked in `docs/RELEASE_1_0_VERSION_BUMP_CHECKLIST.md` and enforced by `scripts/validate_version_metadata.R`.

## Items Still Required Before Public 1.0

These items summarize completed 1.0 release-candidate evidence, publication checks, and the verification items that remain relevant for the 1.0.1 stabilization patch.

- Complete manual packaged-app QA: manual QA against the rebuilt 1.0.0 release candidate is
  complete, including native Windows save-dialog confirmation for actual
  HTML/PDF file creation in the loaded data-file folder.
- `docs/RELEASE_MANUAL_QA.md` and `docs/RELEASE_1_0_MANUAL_QA_RECORD.md` now record visual consistency, file dialogs, packaged runtime behavior, and export handoffs.
- Keep the completed manual QA record with the release notes and validation artifacts.
- Confirm the rebuilt Electron output uses final 1.0 release names: launch the generated executable in `dist/electron/win-unpacked` and manually confirm app startup, About > Open Source Licenses, import, analysis, export, and close behavior. Startup timing, `.studio` open handling, import, analysis, About license display, actual HTML/PDF export file creation, and final release package naming have been confirmed.
- Build and publish `studio.statedu.com`; the initial product/citation landing page is live.
- Register and verify DOI `10.22934/statedu.studio`; DOI resolution has been verified at `https://doi.org/10.22934/statedu.studio`.
- The DOI resolves to the stable citation landing page at `https://studio.statedu.com/citation/`.
- README, `CITATION.cff`, and About may publish the DOI citation line after the current verification.
- Complete `docs/RELEASE_1_0_VERSION_BUMP_CHECKLIST.md` against the rebuilt 1.0.0 package.
- Free/Pro/Latent gates, license activation, and in-app updates are explicitly deferred for 1.0 in `docs/RELEASE_1_0_DECISION_LOG.md`; do not claim them in public release materials.
- Public 1.0 also hides Longitudinal / Panel Models, Excel result export, and Word result export through `STATEDU_PUBLIC_RELEASE=1`; do not claim those surfaces in public release materials.
- Decide whether remaining installer/download infrastructure items in `docs/RELEASE_1_0_DISTRIBUTION_LICENSE_PLAN_KO.md` are implemented for 1.0 or explicitly deferred.
- Record any additional implementation or deferral decisions in `docs/RELEASE_1_0_DECISION_LOG.md`.
- Prepare final public release notes from `docs/RELEASE_1_0_PUBLIC_NOTES_DRAFT.md`.
- Packaged validation evidence is tracked in `docs/RELEASE_1_0_PACKAGED_VALIDATION_NOTES.md`; refresh only if the final package is rebuilt again before publication.

## Repository Check

The configured GitHub repository is reachable by git:

`https://github.com/StatEdu/StatEdu_Studio.git`
