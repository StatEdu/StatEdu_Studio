# StatEdu Studio Latent Mplus Module

This module is developer-only by default.

The current integration keeps StatEdu Studio Latent Mplus as a separate local Shiny app and exposes it from StatEdu Studio only when `EFS_ENABLE_LATENT_MPLUS` is enabled.

Default development path inside this repository:

```text
<repo>/modules/latent_mplus/app
```

Override path:

```text
EASYFLOW_LATENT_MPLUS_PATH=D:/Program/Latent_Mplus
```

Enable in development:

```text
EFS_ENABLE_LATENT_MPLUS=true
```

Mplus is not bundled, distributed, or licensed by StatEdu Studio. Latent estimation requires a separately licensed local Mplus installation on the user's PC.

The release packaging script includes `R/latent_mplus_module.R` and `modules/latent_mplus/` in the standard Electron package. Public builds should document the separate Mplus requirement, and user-facing Excel/PDF export actions follow the StatEdu Studio Free/Pro save policy.
