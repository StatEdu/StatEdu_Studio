# StatEdu Studio Release Readiness Status

Last reviewed: 2026-08-19

Current version: 1.2.0

Current release candidate source version: 1.2.0

## Current 1.2.0 Public Release Snapshot

- Source metadata has been promoted from the stabilized post-1.1.3 work to `1.2.0`.
- Electron packaging has been performed locally and is ready for public deployment.
- Built package: `dist/electron/StatEdu_Studio_Setup_1.2.0.exe`
- Installer SHA256: `AB68645EAB246C6AE88230214F24750462399D523A42A6100C1D30A833CCF0DA`
- Blockmap SHA256: `9BC490604CF57323264357B516046863C815647D3F2C4F082E5B04095CCDB8E2`
- `scripts/release_preflight.ps1 -FullElectronSmoke`: passed on 2026-08-08.
- `scripts/smoke_electron_release.ps1`: passed on 2026-08-07.
- `scripts/smoke_electron_app_lifecycle.ps1`: passed on 2026-08-07.
- `CHANGELOG.md` and `CHANGELOG_KO.md` now show `v1.2.0` as the first version
  history entry, followed by `v1.1.3`, with developer-version entries removed.
- The public Regression / Models menu includes `Mediation / Moderation Custom
  Model` / `매개·조절 사용자 정의 모델`; Electron now sets
  `STATEDU_ENABLE_CUSTOM_MODEL_CANVAS=1` by default.
- `README.md`, `README_KO.md`, `CITATION.cff`, `VERSION`,
  `modules/latent_mplus/app/VERSION`, and Electron package metadata are aligned
  to `1.2.0`.
- 1.2 release candidate documents:
  - `docs/RELEASE_1_2_VERSION_BUMP_CHECKLIST.md`
  - `docs/RELEASE_1_2_DECISION_LOG.md`
  - `docs/RELEASE_1_2_PUBLIC_NOTES_DRAFT.md`
  - `docs/RELEASE_1_2_PACKAGED_VALIDATION_NOTES.md`
  - `docs/RELEASE_1_2_MANUAL_QA_RECORD.md`
- Remaining before public deployment: upload/publish steps.

## Current 1.2.3-dev Development Snapshot

- Source version: `1.2.3-dev`.
- Branch: `codex/sem-model-canvas`.
- Built development package: `dist/electron/StatEdu_Studio_Dev_Setup_1.2.3-dev.exe`.
- Installer SHA256: `A52987D677866C9A726D36CAEDFCC56B6DF84654228FA0A9A62DB867EC276021`.
- Blockmap SHA256: `1E2D70DC4F881E09D0DFCD55988381DC094DFC155A4D74AB283422557D32D125`.
- `scripts/validate_sem_canvas.R` and `scripts/validate_cfa_all.R`: passed on 2026-08-19.
- `scripts/smoke_electron_release.ps1`: passed against the final 1.2.3-dev package.
- `scripts/smoke_electron_app_lifecycle.ps1`: passed with zero packaged Electron/R processes remaining after cleanup.
- Final packaged-app browser QA loaded a three-factor, nine-indicator, 180-observation SEM and confirmed that base results appear before the 5,000-resample HTMT and structural-effect jobs finish.
- The HTMT and structural-effect jobs expose independent live progress panels and Stop buttons. Both packaged workflows were canceled independently, retained base results and point estimates, and displayed explicit user-cancellation messages rather than estimation-failure messages.
- CFA higher-order-factor toolbar labeling, result downloads, default BC bootstrap intervals, and cancellable background bootstrap progress have packaged-app evidence in `docs/RELEASE_1_2_PACKAGED_VALIDATION_NOTES.md`.
- Packaged Windows UI Automation confirmed the actual native `열기` dialog, `StatEdu Model Canvas` filter, CFA/SEM snapshot restoration, Run-button enablement, and entry into the estimator-decision workflow.
- Packaged Windows UI Automation also ran a 360-row ordinal CFA and confirmed `Estimator = DWLS`, `Parameterization = Theta`, `Converged = Yes`, retained base results during background HTMT bootstrap, and zero packaged-process residue after cleanup.
- Packaged Windows UI Automation verified nondefault bootstrap methods end to end: AVE/reliability BCa completed 500 case resamples plus 45 jackknife fits and returned 468/500 valid replicates, while three-factor HTMT Percentile returned 1,000/1,000 valid replicates with populated two-sided and one-sided limits.
- Packaged Windows UI Automation also completed the 301-case `school` multigroup CFA. All four invariance stages converged and were admissible, group-specific reliability/AVE and HTMT rendered, and the live 33-sheet Excel export passed strict OpenXML import, all-sheet rendering, key-value verification, and dangling-relationship/error-token scans. The export now reports the requested MLR estimator consistently in both overview and report-summary sheets.
- Remaining before public promotion: packaged higher-order-model execution; external SmartPLS/ADANCO numerical comparison; final public versioning, manual QA, release notes, upload, and publication. Packaged SEM covariate-model comparison now has MLR robust/scaled end-to-end evidence.

## Historical 1.2.2-dev Development Snapshot

- Source version: `1.2.2-dev`.
- Built development package: `dist/electron/StatEdu_Studio_Dev_Setup_1.2.2-dev.exe`.
- Installer SHA256: `1503955BF64FA6E3D94310C86793ADEE8CF3175260386225BD2F73D320380783`.
- Blockmap SHA256: `D72D4B6DF1221F3827D0FA7C67974FAF5486473D49F3227693BAF595F7F77AD3`.
- `scripts/smoke_electron_release.ps1`: passed on 2026-08-12 after the CFA capture-hook rebuild.
- `scripts/smoke_shiny_app.ps1`: passed on 2026-08-12 with `STATEDU_CAPTURE_CFA_MODEL_FILE` and `STATEDU_CAPTURE_CFA_RUN` enabled.
- SEM/CFA hardening work has been merged into `codex/sem-model-canvas`.
- `scripts/validate_cfa_all.R` covers CFA canvas, reporting/export, and external-reference comparisons.
- `scripts/validate_cfa_all.R` also covers theta-parameterized WLSMV ordered-indicator AVE/reliability bootstrap.
- `scripts/validate_cfa_all.R` covers percentile and BCa CI paths for AVE/reliability bootstrap and HTMT bootstrap.
- `scripts/validate_cfa_all.R` covers CFA bootstrap progress callbacks and cooperative cancel paths; a packaged user-facing cancel button remains pending.
- This snapshot is retained as historical evidence. Its pending bootstrap-progress and cancel items were completed and superseded by the 1.2.3-dev package evidence above.

## Historical 1.0.1 Package Snapshot

- Built package: `dist/electron/StatEdu_Studio_Setup_1.0.1.exe`
- Installer SHA256: `6E408CE402D6DD802B745675485A81C7960E6B8676F44221FDEF81A41CBADC1E`
- Blockmap SHA256: `711C04A393DBD9061A00568F1F16FB4E2CFD6D358915FF76DF0EE154457ABCF6`
- Package rebuilt on 2026-06-28 to include STATEDU environment variable rebranding, navbar static language translation, external Help links opening in system default browser, and startup optimizations (JIT compilation + static label cache for faster page refresh).
- `scripts/smoke_electron_release.ps1 -RepoRoot .`: passed on 2026-06-28 against the rebuilt 1.0.1 output.
- Website release manifest should be updated to 1.0.1 with the new installer SHA256 and `releases/release-notes/1.0.1.html`.

## Local Validation

The local stabilization checks are passing for the current branch.

- `scripts/validate_stabilization.ps1 -Full`: passed on 2026-08-19, including all dedicated SEM validation scripts.
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

## Historical 1.0 Completion Notes

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
- Public 1.0 hides Excel result export and Word result export through `STATEDU_PUBLIC_RELEASE=1`; Longitudinal / Panel Models are exposed in the Analysis menu.
- Decide whether remaining installer/download infrastructure items in `docs/RELEASE_1_0_DISTRIBUTION_LICENSE_PLAN_KO.md` are implemented for 1.0 or explicitly deferred.
- Record any additional implementation or deferral decisions in `docs/RELEASE_1_0_DECISION_LOG.md`.
- Prepare final public release notes from `docs/RELEASE_1_0_PUBLIC_NOTES_DRAFT.md`.
- Packaged validation evidence is tracked in `docs/RELEASE_1_0_PACKAGED_VALIDATION_NOTES.md`; refresh only if the final package is rebuilt again before publication.

## Repository Check

The configured GitHub repository is reachable by git:

`https://github.com/StatEdu/StatEdu_Studio.git`
