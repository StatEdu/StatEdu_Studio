# StatEdu Studio Release Readiness Status

Last reviewed: 2026-09-24

Next source version: 1.3.1 (unreleased). Current development changes and the minimum-version policy are being prepared for the public edition, excluding meta-analysis and within-subject treatment repeated-measures ANOVA. No 1.3.1 installer build or publication is authorized at this stage. See `docs/RELEASE_1_3_1_PREPARATION_KO.md`. The 1.3.0 installer evidence below does not validate a 1.3.1 installer.

Latest upgrade fixes: default desktop result history now uses the user profile; valid surviving legacy histories are copied without changing their originals. Explicit unspecified construct types are preserved. Regression checks, the rebuilt installer, 337 packaged-source hashes, and a GUI restore without a history-path override passed. See `docs/UPGRADE_FIXES_20260915_KO.md`. Actual previous-version installation/upgrade remains unverified; users must save legacy histories outside the installation directory before updating. The prior findings in `docs/UPGRADE_READINESS_20260915_KO.md` are retained as history, not current unresolved source defects.

The 1.3.0 installer has been built and package/source verification passed. Controlled test distribution is supported by the current evidence; actual previous-version installation/upgrade preservation remains unverified. See `docs/RELEASE_FINAL_CHECK_20260915_KO.md`. Public PDF/Word/Excel export is enabled; meta-analysis and within-subject treatment repeated-measures ANOVA are excluded from the public menus.

Current version: 1.3.0

Current installer target: 1.3.0

Current SmartPLS gate: the fresh 1.3.0 HS100 PLS/PLSc rerun passed 6/6 displayed saturated-fit comparisons with 14 sealed private artifacts and exact StatEdu regeneration. Evidence is under `docs/evidence/release_1_3_0/pls/`. The 16-artifact statements below describe historical 1.2.4 evidence; its missing TAM originals are not counted as revalidated or silently substituted.

Release-line note: public 1.2.0 was published on 2026-08-08 and is retained below as historical release evidence. The 1.2.3 development package and promotion records and the 1.2.4 development package records are also retained as historical evidence. The current installer target is 1.3.0. Older completed checks below remain historical evidence and do not certify this installer.

The 1.2.3 fail-closed promotion validator remains scoped to the historical 1.2.3 promotion line. Its evidence and approval requirements must not be reused as proof for a future public build.

The external-program handoff is automated by `scripts/prepare_pls_external_handoff.R` and `scripts/finalize_pls_external_evidence.R`: the first packages the fixed data/model/specification and result template, and the second accepts only versioned, dated, convergence-confirmed SmartPLS/ADANCO results, recomputes all six comparisons, records actual output precision/provenance, and seals the evidence hashes. The operational default is the deterministic first-100-row Holzinger–Swineford profile with actual SmartPLS 4.1.1.8 Student-license displayed-output evidence. The explicit historical 301-row profile remains a legacy reference that fails closed under the current strict PLSc admissibility gate; it is not completed or replaced by the 100-row claim.

`scripts/package_pls_external_handoff.ps1` creates the default first-100 transfer ZIP from a fresh temporary staging directory, verifies the recorded profile and safe data basename, rejects stale files from another profile, embeds per-file SHA-256 values, and emits a checksum sidecar for the archive. The default archive is `outputs/StatEdu_1.2.4_PLS_external_handoff.zip`. This closes the evidence-transfer integrity step only for the recorded first-100 profile and does not create a 301-row claim.

## Historical 1.2.6-dev Development Snapshot

- Source version: `1.2.6-dev`.
- Branch: `codex/external-validation-stabilization`.
- Adds meta-analysis with input templates and one-group repeated-measures ANOVA, expands category-label handling, and refines analysis screens.
- Improves PLS bootstrap scheduling with serial/parallel reproducibility coverage.
- Fixes predictor truncation in long LMM/GLMM formulas and GLMM count-family screening.
- This is a source development update; no 1.2.6-dev installer has been built or assigned release checksums.

## Historical 1.2.5-dev Development Snapshot

- Source version: `1.2.5-dev`.
- Branch: `codex/external-validation-stabilization`.
- The development line reduces first-load time across all analysis menus by caching shared result-table labels and registering structural-model handlers only when their menu is opened.
- Penalized-regression selection stability retains 500 bootstrap resamples and exact per-draw seeds. Its first run avoids cold Windows worker startup; a recent subsequent run uses four workers with a serial fallback. Same-seed cold and warm result objects are required to be identical.
- The full performance audit and reproducible benchmark/validation scripts are recorded in `docs/FULL_ANALYSIS_PERFORMANCE_AUDIT_2026-08-27_KO.md`.
- No 1.2.5-dev installer has been built or assigned release checksums. The 1.2.4-dev installer evidence below is historical and does not validate the current source tree.

## Historical 1.2.4-dev Development Snapshot

- Source version: `1.2.4-dev`.
- Branch: `codex/external-validation-stabilization`.
- The development line adds exact, fail-open bootstrap execution control around lavaan for CFA, SEM, and latent-product moderation, while retaining public full-SE lavaan fits as the final numerical authority.
- Same-index regression gates require the optimized and prior lavaan paths to match in sample order, validity decisions, raw estimates, standardized estimates, and final result tables with tolerance 0.
- Analyst-facing CFA, CB-SEM, latent-moderation, PLS-SEM, and PLSc methodology is integrated in Korean and English in `docs/METHOD_NOTES_KO.md`, `docs/METHOD_NOTES_EN.md`, `docs/ANALYSIS_METHODS_KO.md`, and `docs/ANALYSIS_METHODS_EN.md`. The notes cover estimands, estimator selection, DMC resampling, bootstrap validity, reporting, causal limits, and cross-software comparison.
- SmartPLS 4.1.1.8 under the Student license (free limited, non-Professional) actually ran the deterministic first 100 Holzinger–Swineford rows with the fixed three-factor model. PLS and PLSc both converged in 26 initial PLS iterations; all six saturated SRMR/d_G/d_ULS values matched StatEdu at the three displayed decimal places. Public data/results/manifests are under `docs/evidence/release_1_2_4/pls/`; sixteen vendor UI/project/settings artifacts are retained outside the repository under `STATEDU_SMARTPLS_EVIDENCE_ROOT` and are mandatory for installer/release validation.
- The Student-license run was limited to 100 rows and no completed 301-row execution evidence was retained. Therefore only the first-100 profile is complete and is the operational default; the historical 301-row profile remains explicit legacy/fail-closed under strict PLSc admissibility. The separate TAM-100 manual displayed transcription remains an optional supplementary check without retained source/path evidence.
- A 1.2.4-dev development installer was built from artifact-provenance commit `269e9614362b955067700786fffdc85eb6895f77` on 2026-08-23. Its installer SHA-256 is `651B5A04FB6B8390CDB2884DEF238D69915F289758755DAB6FE28FC90F31615D` and its blockmap SHA-256 is `CB3BDECDAE6ECC476CB54BADDB758ED8CC71432269D2F0499EC225F43B324A8C`. Automated packaged validation passed; interactive manual QA and public promotion remain pending. Detailed evidence is recorded in `docs/RELEASE_1_2_4_DEV_PACKAGED_VALIDATION_NOTES.md` and `docs/RELEASE_1_2_4_DEV_MANUAL_QA_RECORD.md`.
- Source work added on 2026-08-25 through 2026-08-26, including PLS/PLSc
  latent moderation, moderated mediation, MICOM-gated multigroup comparisons,
  result round-trip/export contracts, and bundled cSEM validation support, is
  newer than the installer above. Focused source and bundled-runtime checks may
  be recorded as internal development evidence, but the 2026-08-23 installer
  hashes must not be cited as packaged evidence for these changes.
- Internal 2026-08-26 validation passed the bundled-runtime-only PLS/PLSc
  focused suite (18/18 validators in 32.14 seconds), including the public and
  synthetic external-comparator contract. The approved cSEM 0.6.1 recursive
  non-base dependency closure contains 72 exact Package/Version rows and is
  sealed by SHA-256
  `cf6e4175d47ea943cd6d017951357ee87b1b615f18c6f32799b046b179447674`.
  Missing, additional, changed-version, schema, order, checksum, and
  base/recommended-package mutations all fail closed.
- The core stabilization suite also passed after separating development-data
  validators from the exact bundled-runtime structural-bootstrap gate. The
  latter passed in 15.454 seconds (SEM 1.733, CFA 2.198, PLS-SEM 0.465) with
  bundled lavaan 0.7-2. These remain source/runtime-stage results until a new
  tracked-source installer is built and its packaged smoke/manual QA pass.
- On the current validation workstation, the 16 private SmartPLS artifacts
  referenced by the public manifest and the optional TAM 100-row source data
  are not available. The required private-evidence gate therefore remains a
  release blocker until the original files are supplied through
  `STATEDU_SMARTPLS_EVIDENCE_ROOT`; no replacement evidence is inferred from
  the public manifest or from the historical installer result.

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

## Historical 1.2.3-dev Development Snapshot

- Source version: `1.2.3-dev`.
- Branch: `codex/sem-model-canvas`.
- Built development package: `dist/electron/StatEdu_Studio_Dev_Setup_1.2.3-dev.exe`.
- Installer SHA256: `A6258BE4E1557F9AC426C78915144E37F47EAA228444822F26C41697A7EC33E2`.
- Blockmap SHA256: `4A4864B235E5BA9E4112793F8E085884D43AF8CB4572315FB4261E75FAA423CD`.
- `scripts/validate_sem_canvas.R` and `scripts/validate_cfa_all.R`: passed on 2026-08-19.
- `scripts/smoke_electron_release.ps1`: passed against the final 1.2.3-dev package.
- `scripts/smoke_electron_app_lifecycle.ps1`: passed with zero packaged Electron/R processes remaining after cleanup.
- Final packaged-app browser QA loaded a three-factor, nine-indicator, 180-observation SEM and confirmed that base results appear before the 5,000-resample HTMT and structural-effect jobs finish.
- The HTMT and structural-effect jobs expose independent live progress panels and Stop buttons. Both packaged workflows were canceled independently, retained base results and point estimates, and displayed explicit user-cancellation messages rather than estimation-failure messages.
- CFA higher-order-factor toolbar labeling, full higher-order MLR execution, omega-h/loading results, strict Excel export, default BC bootstrap intervals, and cancellable background bootstrap progress have packaged-app evidence in `docs/RELEASE_1_2_PACKAGED_VALIDATION_NOTES.md`.
- Packaged Windows UI Automation confirmed the actual native `열기` dialog, `StatEdu Model Canvas` filter, CFA/SEM snapshot restoration, Run-button enablement, and entry into the estimator-decision workflow.
- Packaged Windows UI Automation also ran a 360-row ordinal CFA and confirmed `Estimator = DWLS`, `Parameterization = Theta`, `Converged = Yes`, retained base results during background HTMT bootstrap, and zero packaged-process residue after cleanup.
- Packaged Windows UI Automation verified nondefault bootstrap methods end to end: AVE/reliability BCa completed 500 case resamples plus 45 jackknife fits and returned 468/500 valid replicates, while three-factor HTMT Percentile returned 1,000/1,000 valid replicates with populated two-sided and one-sided limits.
- Packaged Windows UI Automation also completed the 301-case `school` multigroup CFA. All four invariance stages converged and were admissible, group-specific reliability/AVE and HTMT rendered, and the live 33-sheet Excel export passed strict OpenXML import, all-sheet rendering, key-value verification, and dangling-relationship/error-token scans. The export now reports the requested MLR estimator consistently in both overview and report-summary sheets.
- Packaged Windows UI Automation also completed the 301-case higher-order CFA with MLR. The fit converged and was admissible; all three general-factor loadings, lower-order R2 values, and model-/score-conditional omega-h rendered. The 26-sheet live Excel export passed strict OpenXML import, all-sheet rendering, value matching, and spreadsheet-error scans.
- The external PLS benchmark is reproducibly prepared: a fixed data/model fixture generates full-precision PLS/PLSc saturated values, an external CSV template, and a SHA-256/version/settings manifest. It also identified and corrected omission of the PLSc disattenuated construct-correlation matrix from fit reconstruction. Comparator and SEM regression checks pass, but no proprietary-program equivalence is claimed without actual SmartPLS/ADANCO output.
- Independent formula evidence is complete: cSEM 0.6.1 reproduced StatEdu SRMR, d_G, and d_ULS on the fixed matrix pair at tolerance 1e-12. This narrows the remaining external risk to algorithm/model-implied-matrix implementation and version/settings differences rather than the three discrepancy formulas themselves.
- Remaining before public promotion: actual SmartPLS/ADANCO numerical results with recorded software versions; final public versioning, manual QA, release notes, upload, and publication. Packaged SEM covariate-model comparison and higher-order CFA execution/export now have end-to-end evidence.

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
