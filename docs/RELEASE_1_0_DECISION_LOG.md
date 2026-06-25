# StatEdu Studio 1.0 Decision Log

Last reviewed: 2026-06-24

Current version: 0.9.42

This file records release decisions that must be made before changing the project from the 0.9.x stabilization line to a public 1.0 release. It is intentionally separate from feature planning so stabilization work does not drift into new product scope.

## Current Stabilization Decision

- No new analysis features before 1.0 unless required for correctness, data safety, packaging, or validation coverage.
- UI work before 1.0 should be limited to shared layout contracts, consistency fixes, accessibility/readability fixes, and regressions found during validation.
- Release work before 1.0 should focus on repeatable validation, packaging smoke tests, documentation, source/license notices, and explicit deferral decisions.

## Must Decide Before Public 1.0

| Area | Decision Needed | Current Status | Default Until Decided |
|---|---|---|---|
| Electron package | Whether the packaged Windows build passes full unpacked-output smoke checks | 0.9.42 Electron package rebuilt with bundled `R-4.5.3`; `scripts/release_preflight.ps1 -FullElectronSmoke` and `scripts/smoke_electron_app_lifecycle.ps1` passed on 2026-06-24 | Block public 1.0 installer until manual packaged-app QA is complete |
| Electron naming | Whether 0.9.x beta packaging names are replaced with final 1.0 names across app display name, package metadata, installer artifact, shortcut name, executable resource strings, and smoke-test expectations | Build and smoke scripts switch from beta to final names when `VERSION` starts with `1.`; current 0.9.42 package metadata intentionally remains beta | Do not publish public 1.0 installer with beta naming unless explicitly approved |
| DOI | Whether `10.22934/statedu.studio` is registered and resolves to `https://studio.statedu.com` | Intended DOI is present in README/CITATION/About metadata, but `doi.org` returns HTTP 404 as of the 2026-06-24 local network check | Block public citation claim |
| Website | Whether `studio.statedu.com` is live and points to the product site | DNS resolves to `statedu.com`, but the 2026-06-24 local network check could not establish the TLS connection | Block public website claim |
| Free/Pro/Latent gates | Whether edition gates are implemented for 1.0 or deferred | Deferred for 1.0; do not add edition gates during the stabilization freeze | Do not claim gated editions |
| License server | Whether activation, validation, device management, and grace-period flows are implemented for 1.0 or deferred | Deferred for 1.0; license activation remains outside the 1.0 stabilization scope | Do not claim license activation |
| Update system | Whether in-app update checks and installer handoff are implemented for 1.0 or deferred | Deferred for 1.0; in-app update checks remain outside the 1.0 stabilization scope | Do not claim in-app updates |
| Public release notes | Whether final notes and validation artifacts are ready for public download | Pending packaging | Block public release announcement |

## Decisions For 1.0

```text
Decision: Remove beta packaging names for public 1.0
Date: 2026-06-25
Owner: StatEdu Studio
Item: Electron display name, package metadata, installer artifact, shortcut name, executable resource strings, app data folder, and smoke-test expectations
Reason: 1.0 is a stable public release line; keeping beta naming would create user confusion and weaken release credibility.
User-visible claim removed or adjusted: Public 1.0 installer and app surfaces should say StatEdu Studio, not StatEdu Studio Beta.
Validation/checklist update: Keep 0.9.42 beta metadata unchanged until the actual 1.0 version bump; Electron packaging and smoke scripts now select final names automatically for VERSION 1.x, and docs/RELEASE_1_0_VERSION_BUMP_CHECKLIST.md verifies the generated output.
Follow-up version: 1.0.0
```

## Deferred For 1.0

```text
Decision: Hide deferred public features for 1.0
Date: 2026-06-25
Owner: StatEdu Studio
Deferred item: Longitudinal / Panel Models public menu exposure, Excel result export, and Word result export
Reason: These surfaces expand public validation and support scope; keep the code path available for internal validation, but do not expose it in the public 1.0 package.
User-visible claim removed or adjusted: Do not claim Longitudinal / Panel Models, Excel result export, or Word result export as public 1.0 features.
Validation/checklist update: Public 1.0 packaging sets `STATEDU_PUBLIC_RELEASE=1`, which hides Longitudinal / Panel Models, Excel export, and Word export while preserving internal override flags for validation builds.
Follow-up version: 1.1 or later
```

```text
Decision: Defer edition gates for 1.0
Date: 2026-06-24
Owner: StatEdu Studio
Deferred item: Free/Pro/Latent feature gates and gated edition enforcement
Reason: 1.0 is now in stabilization; adding entitlement logic would expand product scope and validation risk.
User-visible claim removed or adjusted: Do not claim gated editions, Pro-only import/export, or Latent Add-in entitlement in 1.0 release materials.
Validation/checklist update: Release checklist keeps gated editions as a deferred decision and blocks gated-edition claims.
Follow-up version: 1.1 or later
```

```text
Decision: Defer license activation for 1.0
Date: 2026-06-24
Owner: StatEdu Studio
Deferred item: License server, activation, validation, device management, and offline grace
Reason: License infrastructure is operationally high risk and not required for validating the local statistical application.
User-visible claim removed or adjusted: Do not claim license activation, device activation management, or offline Pro grace in 1.0 release materials.
Validation/checklist update: Release checklist keeps license activation as deferred and blocks license-server claims.
Follow-up version: 1.1 or later
```

```text
Decision: Defer in-app update system for 1.0
Date: 2026-06-24
Owner: StatEdu Studio
Deferred item: In-app update checks, update manifest, checksum download flow, and installer handoff
Reason: Update delivery should be validated after the 1.0 installer and public download path are stable.
User-visible claim removed or adjusted: Do not claim in-app update checks or automatic update download in 1.0 release materials.
Validation/checklist update: Release checklist keeps update checks as deferred and blocks update-system claims.
Follow-up version: 1.1 or later
```

## Future Deferrals

If a distribution/license/update item is intentionally deferred for 1.0, record the decision here before publishing:

```text
Decision:
Date:
Owner:
Deferred item:
Reason:
User-visible claim removed or adjusted:
Validation/checklist update:
Follow-up version:
```
