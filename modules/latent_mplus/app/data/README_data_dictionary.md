# Data Dictionary Guide

`data/<dataset_id>/config/data_dictionary.csv` is the primary metadata file for the pipeline.

This project now uses one shared dictionary schema across datasets.
The canonical header template is:

- [C:\StatEdu\Latent\data\_templates\data_dictionary_HEADER.csv](C:/StatEdu/Latent/data/_templates/data_dictionary_HEADER.csv)

The writing rules and column meanings are documented here:

- [C:\StatEdu\Latent\data\_templates\data_dictionary_GUIDE.md](C:/StatEdu/Latent/data/_templates/data_dictionary_GUIDE.md)

## Rule

- Keep `data_dictionary.csv` as pure CSV without inline comments.
- Use the shared guide for role/type/reference conventions.
- Keep column order identical across datasets.
- When adding a new dataset, start from the shared header template.
- Prefer updating `label_ko`, `label_en`, `description`, `reference`, and `display_order` in a consistent way rather than inventing dataset-specific column variants.

## Current Scope

The following datasets already use the same header structure:

- `01_LPA`
- `02_LCA`
- `03_KSWL`
- `04_LCA_wt`
- `05_mixed`
- `06_Mo`
- `11_SSK`
- `12_MYJ`
- `13_LTA_DEMO`
