# StatEdu Studio 1.0 Decision Log

Last reviewed: 2026-06-24

Current version: 1.0.0

This file records release decisions that must be made before changing the project from the 0.9.x stabilization line to a public 1.0 release. It is intentionally separate from feature planning so stabilization work does not drift into new product scope.

## Current Stabilization Decision

- No new analysis features before 1.0 unless required for correctness, data safety, packaging, or validation coverage.
- UI work before 1.0 should be limited to shared layout contracts, consistency fixes, accessibility/readability fixes, and regressions found during validation.
- Release work before 1.0 should focus on repeatable validation, packaging smoke tests, documentation, source/license notices, and explicit deferral decisions.

## Must Decide Before Public 1.0

| Area | Decision Needed | Current Status | Default Until Decided |
|---|---|---|---|
| Electron package | Whether the packaged Windows build passes full unpacked-output smoke checks | 1.0.0 metadata has been prepared; final 1.0 Electron package still needs rebuild, full smoke, lifecycle smoke, and manual packaged-app QA | Block public 1.0 installer until final package smoke and manual packaged-app QA are complete |
| Electron naming | Whether 0.9.x beta packaging names are replaced with final 1.0 names across app display name, package metadata, installer artifact, shortcut name, executable resource strings, and smoke-test expectations | Source metadata now uses final 1.0 package names; generated Electron output still needs final rebuild and verification | Do not publish public 1.0 installer until generated output uses final non-beta naming |
| DOI | Whether `10.22934/statedu.studio` is registered and resolves to `https://studio.statedu.com` | DOI/homepage infrastructure is not built yet; README, CITATION.cff, and About no longer publish a DOI citation line | Block public citation claim |
| Website | Whether `studio.statedu.com` is live and points to the product site | Homepage infrastructure is not built yet | Block public website claim |
| Free/Pro/Latent gates | Whether edition gates are implemented for 1.0 or deferred | Deferred for 1.0; do not add edition gates during the stabilization freeze | Do not claim gated editions |
| License server | Whether activation, validation, device management, and grace-period flows are implemented for 1.0 or deferred | Deferred for 1.0; license activation remains outside the 1.0 stabilization scope | Do not claim license activation |
| Update system | Whether in-app update checks and installer handoff are implemented for 1.0 or deferred | Deferred for 1.0; in-app update checks remain outside the 1.0 stabilization scope | Do not claim in-app updates |
| Public release notes | Whether final notes and validation artifacts are ready for public download | Pending packaging | Block public release announcement |

## Decisions For 1.0

```text
Decision: Add final release Electron build wrapper
Date: 2026-06-25
Owner: StatEdu Studio
Item: Electron build command naming
Reason: 1.0 release instructions should not expose a beta-named build command, while keeping the already validated build implementation stable.
User-visible claim removed or adjusted: Public 1.0 release docs use scripts/build_electron_release.ps1.
Validation/checklist update: scripts/build_electron_release.ps1 delegates to the compatibility build script, which selects beta or final package names from VERSION.
Follow-up version: 1.0.0
```

```text
Decision: Remove beta packaging names for public 1.0
Date: 2026-06-25
Owner: StatEdu Studio
Item: Electron display name, package metadata, installer artifact, shortcut name, executable resource strings, app data folder, and smoke-test expectations
Reason: 1.0 is a stable public release line; keeping beta naming would create user confusion and weaken release credibility.
User-visible claim removed or adjusted: Public 1.0 installer and app surfaces should say StatEdu Studio, not StatEdu Studio Beta.
Validation/checklist update: Source metadata has been bumped to 1.0.0 with final package names; Electron packaging and smoke scripts select final names for VERSION 1.x, and docs/RELEASE_1_0_VERSION_BUMP_CHECKLIST.md verifies the generated output.
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
