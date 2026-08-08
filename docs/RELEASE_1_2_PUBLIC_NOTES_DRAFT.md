# StatEdu Studio 1.2 Public Release Notes

## Release

```text
Product: StatEdu Studio
Version: 1.2.0
Release date: 2026-08-08
Download page: https://github.com/StatEdu/StatEdu_Studio/releases/tag/v1.2.0
DOI: https://doi.org/10.22934/statedu.studio
Source repository: https://github.com/StatEdu/StatEdu_Studio
```

## Summary

StatEdu Studio 1.2 promotes the stabilized post-1.1.3 work to a public release.
It adds a mixed repeated-measures ANOVA workflow for pre-post and multi-time
group comparisons, includes the Mediation / Moderation Custom Model canvas in
the public Regression / Model workflow group, refines inter-rater agreement
output, cleans custom-model result tables, and expands statistical validation
coverage across the analysis suite.

## Included In 1.2

- Mixed repeated-measures ANOVA with PP/ITT paths, covariate-adjusted summaries,
  assumption review, post-hoc comparisons, and report exports.
- Mediation / Moderation Custom Model canvas for drawing supported mediation and
  moderation structures.
- Inter-rater agreement output that prioritizes the recommended agreement index
  while retaining supporting indices.
- Custom-model result table cleanup for reused fitted models.
- Expanded validation coverage for analysis, calculator, data-editor, and
  repeated-measures workflows.
- Existing public data import, data editing, guided analysis, HTML/PDF export,
  and bundled Windows runtime support.

## Not Included In 1.2

- Free/Pro/Latent edition gates.
- License activation, device management, or offline grace-period enforcement.
- In-app update checks or automatic update handoff.
- Public Excel or Word result export outside the already documented public
  exceptions.

## Package Checksums

- Installer SHA256: `AB68645EAB246C6AE88230214F24750462399D523A42A6100C1D30A833CCF0DA`
- Blockmap SHA256: `9BC490604CF57323264357B516046863C815647D3F2C4F082E5B04095CCDB8E2`

## Validation

- `scripts/release_preflight.ps1 -FullElectronSmoke` passed.
- `scripts/smoke_electron_release.ps1` passed.
- `scripts/smoke_electron_app_lifecycle.ps1` passed.
- Packaged validation notes are recorded in `docs/RELEASE_1_2_PACKAGED_VALIDATION_NOTES.md`.
- Public text contains no deferred feature claims.
