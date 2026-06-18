# EasyFlow Latent Mplus Module

This module is developer-only by default.

The current integration keeps EasyFlow Latent Mplus as a separate local Shiny app and exposes it from EasyFlow Statistics only when `EFS_ENABLE_LATENT_MPLUS` is enabled.

Default development path inside this repository:

```text
D:/Program/EasyFlow_Statistics/EasyFlow_Statistics_dev/modules/latent_mplus/app
```

Override path:

```text
EASYFLOW_LATENT_MPLUS_PATH=D:/Program/Latent_Mplus
```

Enable in development:

```text
EFS_ENABLE_LATENT_MPLUS=true
```

Mplus is not bundled, distributed, or licensed by EasyFlow Statistics. Latent estimation requires a separately licensed local Mplus installation on the user's PC.

The release packaging script excludes `R/latent_mplus_module.R` and `modules/latent_mplus/` from the standard Electron beta package. Future public latent-enabled releases should intentionally remove that exclusion and document the separate Mplus requirement.
