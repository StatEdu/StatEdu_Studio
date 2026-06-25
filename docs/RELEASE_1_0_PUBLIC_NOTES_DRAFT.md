# StatEdu Studio 1.0 Public Release Notes Draft

This draft is a source for public-facing 1.0 release notes. Do not publish it
until the final 1.0.0 package is rebuilt, manual QA is complete, DOI resolution
is verified, and the website is live.

## Release

```text
Product: StatEdu Studio
Version: 1.0.0
Release date:
Download page:
DOI:
Source repository:
```

## Summary

StatEdu Studio 1.0 is the first stable public release of the StatEdu desktop
statistics application. It focuses on data import, data editing, common
statistical analyses, reproducible result export, and a bundled Windows runtime
based on R 4.5.3.

## Included In 1.0

- Data import for supported SPSS/SAS/Stata, Excel, CSV, and DAT files.
- Variable selection, labels, measurement review, and worksheet-style data
  preview.
- Data Editor workflows for rename, recode, missing-value handling, coding
  checks, and wide-to-long reshaping.
- Core analysis workflows for descriptive tables, group comparisons,
  association/measurement, regression, GLM, sample size, effect size, and
  selected calculators.
- Result export to HTML, PDF, and the in-app result collection.
- Windows desktop package with bundled R 4.5.3 runtime and open-source notices.

## Validation

- Automated stabilization validation must pass before release.
- Electron release smoke checks must pass against the final package.
- Manual QA must be recorded in `docs/RELEASE_1_0_MANUAL_QA_RECORD.md`.
- Packaged validation notes must be completed in
  `docs/RELEASE_1_0_PACKAGED_VALIDATION_NOTES.md`.

## Public Citation

Do not publish the DOI line until the DOI/homepage infrastructure is built and
`10.22934/statedu.studio` resolves to `https://studio.statedu.com`.

```text
LEE, I. H. (2026). StatEdu Studio (Version 1.0.0) [Computer software].
https://doi.org/10.22934/statedu.studio
```

## Not Included In 1.0

The following items are explicitly deferred for 1.0 and must not be described as
available in public release text:

- Free/Pro/Latent edition gates.
- License activation, device management, or offline grace-period enforcement.
- In-app update checks or automatic update handoff.
- Longitudinal / Panel Models public menu exposure.
- Excel result export.
- Word result export.
- Public installer infrastructure beyond the verified download path.

## Final Publication Checklist

- Final 1.0.0 package rebuilt.
- `scripts/release_preflight.ps1 -FullElectronSmoke` passed.
- Manual QA record complete.
- Packaged validation notes complete.
- `studio.statedu.com` live.
- DOI resolves to `https://studio.statedu.com`.
- Public text contains no deferred feature claims.
