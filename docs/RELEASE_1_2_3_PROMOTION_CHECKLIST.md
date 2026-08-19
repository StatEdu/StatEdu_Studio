# StatEdu Studio 1.2.3 Promotion Checklist

This checklist promotes `1.2.3-dev` to public `1.2.3`. It does not modify or supersede the already published 1.2.0 release record.

## Gate 1: External PLS/PLSc evidence

- Generate the external handoff with `scripts/prepare_pls_external_handoff.R`.
- Package the handoff with `scripts/package_pls_external_handoff.ps1` and retain the archive SHA-256 alongside the external-run record.
- Run the identical saturated model in SmartPLS or ADANCO.
- Record program name, exact version, run date, standardized-result setting, weighting scheme, convergence, and fit target.
- Create the external-run record from `docs/RELEASE_1_2_3_EXTERNAL_PLS_RUN.template.json` and record the fixed data, model, StatEdu result, external result, and comparison SHA-256 values.
- Preserve full-precision PLS and PLSc SRMR, d_G, and d_ULS values.
- Record the decimal places actually available in the exported external values; describe a passing comparison as agreement within recorded output precision, not equality of inaccessible internal values.
- Run `scripts/finalize_pls_external_evidence.R` and retain a comparison CSV in which every independently recomputed row passes the declared tolerances.
- Verify data/model/result SHA-256 values in the external-run record; the promotion validator must recompute the comparison instead of trusting a recorded `Pass` column alone.

## Gate 2: Final metadata and public claims

- Change `VERSION` and all version-controlled product metadata from `1.2.3-dev` to `1.2.3` only after Gate 1 passes.
- Complete `docs/RELEASE_1_2_3_VERSION_BUMP_CHECKLIST.md`; do not edit or supersede historical 1.2.0 release evidence.
- Finalize `docs/RELEASE_1_2_3_PUBLIC_NOTES_DRAFT.md` without unsupported causal, model-fit, unidimensionality, or cross-program-equivalence claims.
- Add 1.2.3 entries to the English and Korean changelogs while preserving 1.2.0 as historical content.
- Confirm README, citation, About, Electron metadata, and latent-module version alignment.
- Run `scripts/validate_sem_public_claims.R` and retain every interpretation boundary.

## Gate 3: Final package validation

- Complete `docs/RELEASE_1_2_3_PACKAGED_VALIDATION_NOTES.md` against the final public package; development-package results are corroborating evidence only.
- Run `scripts/validate_stabilization.ps1 -Full`.
- Run `scripts/release_preflight.ps1`.
- Build with `scripts/build_electron_release.ps1` and final non-dev names.
- Run `scripts/release_preflight.ps1 -FullElectronSmoke`.
- Run `scripts/smoke_electron_app_lifecycle.ps1`.
- Record installer and blockmap SHA-256 values.
- Confirm the final package has zero residual Electron/R processes after close.

## Gate 4: Manual packaged QA

- Complete a new `docs/RELEASE_1_2_3_MANUAL_QA_RECORD.md` against the final public installer.
- Keep every row `Pending` until it is repeated against the checksum-recorded final non-development package; development-build automation is corroborating evidence only.
- Recheck native model loading, CFA/SEM/PLS execution, Korean/English switching and restart, HTML/PDF public exports, license notices, paths with spaces/Korean text, and clean shutdown.
- Recheck the higher-order CFA, multigroup CFA, covariate comparison, bootstrap method/progress/cancel, and saturated PLS/PLSc diagnostic labeling.

## Gate 5: Approval and publication

- Create `docs/evidence/release_1_2_3/promotion_manifest.json` from the template and set a gate to true only after its evidence exists.
- After the tracked `VERSION` file and release metadata are finalized as `1.2.3`, run `scripts/validate_sem_release_promotion.R`; it must pass.
- Record approver and approval time.
- Upload the exact checksum-verified installer/blockmap and publish the GitHub release only after explicit approval.
