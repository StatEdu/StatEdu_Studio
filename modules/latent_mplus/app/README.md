# EasyFlow Latent Mplus

EasyFlow Latent Mplus is a local Shiny interface for Mplus-based latent class, latent profile, mixed indicator, latent transition, and state transition workflows.

The app bundles the Latent R/Mplus pipeline, dataset configuration folders, Mplus temporary workspace, and result output folders under the EFS repository `modules/latent_mplus/app` folder, so it can run as a standalone local program or from the EFS developer-only Latent Mplus menu.

## Current Version

Current development version: `0.9.35`

This build uses the StatEdu Studio 0.9.35 app shell structure for startup logging, package checks, data import helpers, settings handling, shared UI assets, local browser launching, and B5-oriented result display. The latent/Mplus workflow modules remain specific to EasyFlow Latent Mplus.

## Local Run

Double-click `easyflow_latent_mplus.bat`, or run:

```r
shiny::runApp("D:/Program/EasyFlow_Statistics/EasyFlow_Statistics_dev/modules/latent_mplus/app")
```

The default local port is `3867`. Set `EASYFLOW_PORT` before launch to override it.
