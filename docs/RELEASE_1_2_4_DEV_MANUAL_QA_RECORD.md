# StatEdu Studio 1.2.4-dev Manual QA Record

This checklist applies only to the `1.2.4-dev` Windows installer whose commit
and SHA-256 are recorded below. It is a development-package QA record, not a
public-release sign-off.

Overall status: in progress

Use the timing, single-progress-card, cancellation, label, and result-transition
contracts in `docs/INSTALLER_REGRESSION_CHECKLIST_2026-08-22_KO.md` for this
exact installer hash.

## Candidate identity

```text
QA date:
Tester:
Git commit:
Installer: dist/electron/StatEdu_Studio_Dev_Setup_1.2.4-dev.exe
Installer SHA-256:
Blockmap SHA-256:
Windows version:
Display scale:
```

## Automated prerequisites

| Check | Status | Evidence |
|---|---|---|
| Full source stabilization suite | Pending | |
| `scripts/validate_installer_regressions.ps1` | Pending | |
| cSEM 0.6.1 required oracle | Pending | |
| Private SmartPLS evidence gate | Pending | No proprietary screenshots or project files are published. |
| `release_preflight.ps1 -FullElectronSmoke` | Pending | |
| Electron lifecycle smoke | Pending | |

## Identity, startup, and shutdown

| Manual check | Expected result | Status | Evidence / notes |
|---|---|---|---|
| Launch packaged executable | Opens without console or startup error | Pending | |
| First interactive screen | Ready within 15 seconds | Pending | |
| Navbar and About | `1.2.4-dev` and Dev identity are consistent | Pending | |
| Open-source licenses | Bundled notices open and are readable | Pending | |
| Authenticated local session | Uses authenticated `127.0.0.1` Shiny backend | Pending | |
| Close Electron | Packaged Electron and R/Shiny processes terminate | Pending | |

## Shared workflow and labels

| Manual check | Expected result | Status | Evidence / notes |
|---|---|---|---|
| Korean/English switch and restart | Selection and all main navigation labels persist | Pending | |
| CSV/Excel import | Data and variable labels render correctly | Pending | |
| `.studio` save/load | Variables, canvas, and settings restore | Pending | |
| CFA/SEM/PLS toolbar | No overlap, disappearance, or four-row PLS toolbar | Pending | |
| Label collisions | Results and Excel map each raw identifier once | Pending | |
| Syntax/audit raw names | Reproducibility sheets intentionally retain raw/internal identifiers | Pending | |

## CFA

| Manual check | Expected result | Status | Evidence / notes |
|---|---|---|---|
| Continuous CFA | Fit, loadings, reliability, AVE, and HTMT render | Pending | |
| Ordinal CFA | WLSMV/DWLS theta workflow is labeled consistently | Pending | |
| Higher-order CFA | Higher-order outputs and Excel sheets open without repair | Pending | |
| Multigroup invariance | Configural/metric/scalar/strict and group diagnostics render | Pending | |
| CFA bootstrap | Full-SE strict inference, monotonic progress, and Stop work | Pending | |

## SEM

| Manual check | Expected result | Status | Evidence / notes |
|---|---|---|---|
| Direct/indirect/total effects | Tables and labels match the drawn model | Pending | |
| Latent-product moderation | Product-aware bootstrap completes with lavaan authoritative final fits | Pending | |
| Bootstrap progress | One monotonic card; screening, validating, and summary do not regress | Pending | |
| Sampling-design gate | Unknown/unsupported designs are blocked; independent default is disclosed | Pending | |
| Audit download | Schema 1.7 records seed, fingerprints, quantile type, valid draws, and failures | Pending | |

## PLS-SEM and PLSc

| Manual check | Expected result | Status | Evidence / notes |
|---|---|---|---|
| Reflective PLS | Paths, loadings, weights, HTMT, reliability, and fit diagnostics render | Pending | |
| Eligible PLSc | Consistency-corrected results and diagnostics render | Pending | |
| Formative/composite PLSc request | Precise rejection with standard PLS still available | Pending | |
| Bootstrap sufficient | At least 80% whole-draw-valid repetitions show inference | Pending | |
| Bootstrap insufficient/canceled/error | Inference is suppressed; no dashed nonsignificance; failure counts are visible/audited | Pending | |
| External comparison wording | Only displayed-output precision is claimed and SmartPLS is cited | Pending | |

## Export and cleanup

| Manual check | Expected result | Status | Evidence / notes |
|---|---|---|---|
| Excel export | Opens without repair and matches visible result tables | Pending | |
| HTML/PDF native export | Nonempty files render and match the app | Pending | |
| Result reopen | Saved result restores tables, notes, estimator, and version | Pending | |
| Cancellation cleanup | Background job and worker tree terminate; base fit remains available | Pending | |
| Residual processes | Zero packaged Electron/R processes after close | Pending | |

## Failure record

| QA row | Defect | Fix commit | Re-run result |
|---|---|---|---|
| | | | |

## Final status

```text
Manual QA status: Pending
Open failures:
Tester sign-off:
Sign-off time:
```
