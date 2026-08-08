# StatEdu Studio 1.2 Version Bump Checklist

Use this checklist when promoting the stabilized post-1.1.3 work to the
public 1.2.0 release candidate. This checklist prepares source metadata and
documentation only; it does not publish or distribute an installer.

## Preconditions

- Keep the bundled Windows runtime on `R-4.5.3`.
- Do not publish a release artifact until automated preflight, Electron
  packaged smoke checks, and manual QA are complete.
- Confirm the Mediation / Moderation Custom Model canvas is visible in the
  public Regression / Model workflow group.

## Version Metadata

- `VERSION`: change to `1.2.0`.
- `modules/latent_mplus/app/VERSION`: change to `1.2.0`.
- `README.md` and `README_KO.md`: update current version, release scope,
  validation summary, and citation example.
- `CITATION.cff`: update `version` and `date-released`.
- `packaging/electron/package.json` and `package-lock.json`: update the root
  version to `1.2.0`.
- `CHANGELOG.md` and `CHANGELOG_KO.md`: add the `v1.2.0` official release
  candidate section.

## Documentation Scope

- Update current public documentation from public 1.0 wording to public 1.2
  wording where it describes the active app, user guide, analysis methods,
  method notes, and validation summaries.
- Keep older 1.0 release evidence files as historical records unless they are
  explicitly superseded by 1.2 release candidate documents.
- Mention Mixed Repeated-Measures ANOVA and Mediation / Moderation Custom Model
  in the 1.2 scope and validation notes.

## Required Validation Before Packaging

```powershell
scripts\validate_stabilization.ps1 -Full
scripts\release_preflight.ps1
```

## Required Validation After Packaging

```powershell
scripts\build_electron_release.ps1 -RHome 'D:\Program\R\R-4.5.3'
scripts\release_preflight.ps1 -FullElectronSmoke
scripts\smoke_electron_app_lifecycle.ps1
```

Packaging, checksum recording, manual QA completion, release upload, and public
announcement remain pending until explicitly performed.
