# StatEdu Studio 1.2.3 Version-Promotion Checklist

Use this checklist only after the external PLS/PLSc evidence gate passes. It promotes the development line from `1.2.3-dev` to public `1.2.3`; it does not publish the release by itself.

## Hard preconditions

- `docs/evidence/release_1_2_3/pls/external_run.json` identifies the actual SmartPLS/ADANCO software, version, valid run date, saturated target, standardized results, reported decimal places, and convergence before 300 iterations.
- The external comparison contains exactly six independently recomputed passing rows and all fixed fixture/result hashes verify.
- `docs/RELEASE_1_2_3_DECISION_LOG.md` is ready to change from blocked to approved only after every remaining gate is complete.
- Do not alter the published 1.2.0 release tag, artifacts, notes, dates, or checksums.

## Source version metadata

- Change `VERSION` from `1.2.3-dev` to `1.2.3`.
- Change `modules/latent_mplus/app/VERSION` to `1.2.3` and update its README shell-version wording.
- Update the root `README.md` and `README_KO.md`: public version `1.2.3`, remove the development-version line, summarize the validated scope, and update citation examples.
- Update `CITATION.cff` to version `1.2.3` and the actual release date.
- Add public `v1.2.3` sections to `CHANGELOG.md` and `CHANGELOG_KO.md`; keep 1.2.0 and earlier entries unchanged below them.

## Electron public profile

- Change `packaging/electron/package.json` and the root/package entries in `package-lock.json` to `1.2.3`.
- Synchronize the public metadata: package `statedu-studio`, description `StatEdu Studio desktop installer`, app id `com.statedu.studio`, product/shortcut `StatEdu Studio`, and artifact `StatEdu_Studio_Setup_${version}.${ext}`.
- Confirm the staged and unpacked executable is `StatEdu Studio.exe`, not a Dev/Beta executable.
- Confirm the final installer is `dist/electron/StatEdu_Studio_Setup_1.2.3.exe` with its matching blockmap.

## Release evidence and claims

- Replace every publication placeholder in `docs/RELEASE_1_2_3_PUBLIC_NOTES_DRAFT.md` with verified values; retaining `DRAFT` in the filename is acceptable, but the content must be final and approved.
- Run `scripts/validate_sem_public_claims.R` and resolve every unsupported causal, universal-fit, unidimensionality, bifactor-equivalence, or cross-program-equivalence claim.
- Complete `docs/RELEASE_1_2_3_MANUAL_QA_RECORD.md` against the final non-development installer; do not carry forward development-package Pass values.
- Create `docs/evidence/release_1_2_3/promotion_manifest.json` from its template and record the final installer/blockmap hashes and human approval.

## Validation order

```powershell
scripts\validate_stabilization.ps1 -Full
scripts\release_preflight.ps1
scripts\build_electron_release.ps1
scripts\release_preflight.ps1 -FullElectronSmoke
scripts\smoke_electron_app_lifecycle.ps1
scripts\get_release_checksums.ps1
scripts\validate_sem_release_promotion.R
```

After packaging, rerun the full 1.2.3 manual QA record against the checksum-recorded installer. A failed or pending row returns the release to blocked status.

## Publication boundary

- Record the exact commit, installer/blockmap hashes, QA sign-off, approver, and approval time before upload.
- Upload only the artifacts whose hashes match the promotion manifest.
- Keep the pull request/release in draft state until explicit publication approval is recorded.
- Never infer approval from passing automated checks.
