# Common variable drag ordering (2026-09-14)

Ordered variable lists now share pointer dragging, insertion indicators, multi-selection and edge auto-scrolling. Server handlers explicitly register each list through `register_analysis_reorder`; `analysis_reorder_items` rejects duplicate, missing or unknown items. Existing button handlers remain available. Each callback updates the same state and side effects as its up/down handler.

Coverage: regression/hierarchical, regularized regression, logistic, generalized and longitudinal regression, frequency, correlation, crosstabs, PCA/factor, reliability/agreement, t-test/ANOVA/ANCOVA, nonparametric, paired/repeated/grouped designs, complex samples, mediation/moderation variable lists, and ordered data-editing lists. Hierarchical block 2 reorders its own positions without changing block 3. Paired groups and wide/long specifications move as intact items.

Validation: all R source files parse; actual browser tests in `scripts/validate_common_reorder.cjs` against `scripts/validate_common_reorder_app.R` verify a ten-variable first-to-last move with automatic scrolling, multiple selected items, rejection of invalid payloads, and dependent-variable ordering. This changes setup state only; results retain their existing rendering/export paths.
