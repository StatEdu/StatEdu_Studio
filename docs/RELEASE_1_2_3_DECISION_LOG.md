# StatEdu Studio 1.2.3 Decision Log

Current development version: 1.2.3-dev

Target public version: 1.2.3

Public promotion: blocked pending required evidence

## Release lineage

- Version 1.2.0 was published on 2026-08-08 and remains an immutable historical release record.
- The current SEM/CFA hardening branch belongs to the 1.2.3 development line. Its evidence must not be appended to the 1.2.0 release claims or checksums.
- Changing `VERSION` from `1.2.3-dev` to `1.2.3` is a release action, not a documentation cleanup step.

## Decisions

| Area | Decision | Release impact |
|---|---|---|
| External PLS evidence | Require version-recorded SmartPLS or ADANCO saturated-model output and a passing machine comparison before public promotion. | cSEM formula equality alone cannot satisfy the end-to-end external-program gate. |
| SEM/CFA claims | Public notes may describe only packaged and automated checks recorded in the 1.2.3 evidence files. | No claim of universal fit, causal identification, unidimensionality, or SmartPLS/ADANCO equivalence. |
| Manual QA | Require a completed 1.2.3 packaged manual-QA record against the final non-dev installer. | Development-package QA cannot substitute for final public-package QA. |
| Versioning | Keep `1.2.3-dev` until every promotion-manifest gate is complete. | Prevents accidental creation of a public-named installer from incomplete evidence. |
| Publication | Upload and release publication require a separately recorded human approval after checksum verification. | The automation prepares and verifies artifacts but does not infer publication approval. |

## Current blockers

- Actual SmartPLS/ADANCO output, software version, run date, settings record, and passing comparison CSV.
- Final 1.2.3 version metadata and public release notes.
- Final non-dev installer build, checksums, release smoke, lifecycle smoke, and packaged manual QA.
- Explicit publication approval.
