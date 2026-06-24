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
| Electron package | Whether the packaged Windows build passes full unpacked-output smoke checks | Not yet packaged in this readiness pass | Block public 1.0 installer |
| DOI | Whether `10.22934/statedu.studio` is registered and resolves | `doi.org` currently returns HTTP 404 | Block public citation claim |
| Website | Whether `studio.statedu.com` is live and points to the product site | Needs normal browser/network confirmation | Block public website claim |
| Free/Pro/Latent gates | Whether edition gates are implemented for 1.0 or deferred | Pending decision | Do not claim gated editions |
| License server | Whether activation, validation, device management, and grace-period flows are implemented for 1.0 or deferred | Pending decision | Do not claim license activation |
| Update system | Whether in-app update checks and installer handoff are implemented for 1.0 or deferred | Pending decision | Do not claim in-app updates |
| Public release notes | Whether final notes and validation artifacts are ready for public download | Pending packaging | Block public release announcement |

## If Deferred

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

