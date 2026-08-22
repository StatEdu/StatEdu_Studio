# StatEdu Studio 1.2.3 Final Packaged Validation Notes

This record applies only to the final public `1.2.3` Windows package. The existing 1.2.3-dev installer and its checksums are development evidence and cannot complete this record.

Overall status: blocked / pending final public package

## Package identity

```text
Product: StatEdu Studio
Release candidate: 1.2.3
Git commit:
Validation date:
Installer: dist/electron/StatEdu_Studio_Setup_1.2.3.exe
Installer SHA-256:
Installer bytes:
Blockmap: dist/electron/StatEdu_Studio_Setup_1.2.3.exe.blockmap
Blockmap SHA-256:
Blockmap bytes:
Executable: dist/electron/win-unpacked/StatEdu Studio.exe
R runtime: R-4.5.3
Electron:
electron-builder:
Residual packaged processes:
```

## Source and pre-package validation

| Check | Status | Evidence / notes |
|---|---|---|
| Git commit matches the promotion manifest | Pending | |
| `VERSION` and public metadata are `1.2.3` | Pending | |
| `scripts/validate_installer_regressions.ps1` | Pending | Attach the timed step output. |
| `scripts/validate_stabilization.ps1 -Full` | Pending | |
| `scripts/validate_sem_public_claims.R` | Pending | |
| `scripts/validate_sem_release_promotion.R` preconditions other than final package/approval | Pending | |
| `scripts/release_preflight.ps1` before packaging | Pending | |
| Working tree contains no unintended build inputs | Pending | |

## Build and package structure

| Check | Status | Evidence / notes |
|---|---|---|
| `scripts/build_electron_release.ps1` completes | Pending | |
| Installer name is `StatEdu_Studio_Setup_1.2.3.exe` | Pending | |
| Executable/product/shortcut name is `StatEdu Studio` without Dev/Beta | Pending | |
| App id/package metadata use the public profile | Pending | |
| Bundled app version is `1.2.3` | Pending | |
| Bundled Windows runtime is R 4.5.3 | Pending | |
| Runtime prune report contains only allowed `keep` rows | Pending | |
| Required SEM/CFA packages, including lavaan and seminr, load from the bundle | Pending | |
| ASAR/unpacked resource boundaries pass release smoke | Pending | |
| Third-party notices and Electron/Chromium license files are present | Pending | |
| npm dependency audit has no unresolved release-blocking finding | Pending | |

## Automated packaged validation

| Check | Status | Evidence / notes |
|---|---|---|
| `scripts/release_preflight.ps1 -FullElectronSmoke` | Pending | |
| `scripts/smoke_electron_release.ps1` | Pending | |
| `scripts/smoke_electron_app_lifecycle.ps1` | Pending | |
| Authenticated loopback Shiny session becomes ready | Pending | |
| Closing Electron terminates bundled R/Shiny | Pending | |
| Residual packaged Electron/R process count is zero | Pending | |
| Startup log contains no release-blocking error | Pending | |

## 2026-08-22 regression evidence

Complete `docs/INSTALLER_REGRESSION_CHECKLIST_2026-08-22_KO.md` against this exact installer hash. A failed or unmeasured required row blocks promotion.

| Check | Required result | Status | Evidence / measured time |
|---|---|---|---|
| First installed-app launch | First interactive screen within 15 seconds | Pending | |
| Small example CSV | Variable table within 5 seconds; one source-data read; no recursive workspace scan | Pending | |
| `.studio` settings restore | Variables and canvas/model restored within 5 seconds | Pending | |
| Mediation/moderation custom canvas | Toolbar and grid interactive within 3 seconds | Pending | |
| CFA/SEM/PLS-SEM/custom toolbars | No overlap or disappearance; PLS-SEM does not wrap to four rows; custom font is 13px | Pending | |
| Custom bootstrap fixture | 75 rows, two focal X variables, 5,000 samples per X complete within 20 seconds | Pending | |
| Bootstrap status and Stop | Exactly one monotonic status card; Stop owns and terminates the active worker | Pending | |
| Result transition | Result table/model visible within 5 seconds after bootstrap completion; source model remains recoverable | Pending | |
| Delta R-squared | Numeric value shown when defined; the entire row is absent when undefined | Pending | |
| Launcher/build identity | Current build fingerprint is served; same healthy BAT backend is reused; an unrelated listener is preserved | Pending | |

## Packaged SEM/CFA/PLS regression evidence

| Check | Status | Evidence / notes |
|---|---|---|
| CFA and SEM native `.stmodel` loading and execution | Pending | |
| Higher-order CFA results and strict Excel export | Pending | |
| Multigroup CFA invariance and group-specific diagnostics/export | Pending | |
| Ordinal WLSMV/theta workflow | Pending | |
| CFA covariate effects and research/adjusted/Delta comparison | Pending | |
| SEM covariate effects and robust/scaled model comparison | Pending | |
| BC default plus percentile/BCa bootstrap selection | Pending | |
| Bootstrap progress, independent cancellation, and retained base results | Pending | |
| Reflective PLS and eligible PLSc saturated diagnostics | Pending | |
| Composite/formative PLSc rejection and standard PLS fallback | Pending | |
| External PLS/PLSc evidence agrees within recorded output precision | Pending | |
| Audit/reproducibility downloads contain versions, seeds, and fingerprints | Pending | |

## Artifact integrity

| Check | Status | Evidence / notes |
|---|---|---|
| Installer SHA-256 independently recomputed | Pending | |
| Blockmap SHA-256 independently recomputed | Pending | |
| Installer/blockmap hashes match the promotion manifest | Pending | |
| Installer and blockmap byte sizes are recorded and positive | Pending | |
| Final manual QA uses this exact installer hash | Pending | |
| Only checksum-matching artifacts are selected for upload | Pending | |

## Failure record

| Validation row | Defect | Fix commit | Re-run command | Re-run result |
|---|---|---|---|---|
| | | | | |

## Final status

```text
Packaged validation status: Pending
Validator:
Validation completion time:
Open failures:
```

This record may be set to `Pass` only when every required row is `Pass` or has a justified `NA`, all failures are closed, the residual packaged-process count is zero, and the recorded hashes match both the files and the promotion manifest.
