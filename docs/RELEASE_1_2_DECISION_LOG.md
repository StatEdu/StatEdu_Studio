# StatEdu Studio 1.2 Decision Log

Current version: 1.2.0

This file records release decisions for promoting the stabilized post-1.1.3
work to the public 1.2.0 release.

## Decisions

| Area | Decision | Release impact |
|---|---|---|
| Versioning | Promote the stabilized post-1.1.3 work to `1.2.0` instead of another 1.1.x patch. | The mixed repeated-measures ANOVA workflow and validation expansion are minor-release scope. |
| Public scope | Include the Mediation / Moderation Custom Model canvas in the public Regression / Model workflow group. | Public 1.2 advertises and validates the custom model canvas together with the existing mediation/moderation engine. |
| Packaging | Build and publish `StatEdu_Studio_Setup_1.2.0.exe` from the public Electron release profile. | Public release uses final StatEdu Studio names and bundled R-4.5.3 runtime. |
| Validation | Require preflight before publication and full Electron smoke checks after packaging. | `docs/RELEASE_1_2_PACKAGED_VALIDATION_NOTES.md` records the final package hashes and validation evidence. |
| Deferred claims | Do not claim edition gates, license activation, in-app updates, or broad Excel/Word public export. | Public notes list these as not included in 1.2. |

## Current Status

- Source version metadata: prepared for `1.2.0`.
- Public release notes: prepared.
- Packaged validation evidence: complete for automated public deployment checks.
- Public deployment: approved for upload/publish.
