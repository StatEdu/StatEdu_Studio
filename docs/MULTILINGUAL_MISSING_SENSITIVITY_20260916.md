# Missing-data sensitivity localization and menu ordering

Source-only continuation; installer not built or installed.

- Added 14 catalog sources with Japanese, Chinese, Spanish, French, German and Vietnamese translations. Existing Korean handling remains unchanged.
- Added supplementary templates for MI pooling, dependent-variable missingness policy, IPW clipping/normalization, IPW fallback, strategy summaries and result counts.
- Template formatting supports multiple captures. Nested MI policy prose and known strategy labels are translated; variable identifiers, numeric bounds and underlying error text retain their original values.
- Moved Ridge/LASSO/Elastic Net immediately before Logistic Regression in both the R tab declarations and JavaScript regression submenu order. Tab IDs and handlers are unchanged.

Validation: all eight language passes in `validate_longitudinal_structural_i18n.R`, including actual GEE English main-table equivalence and supplemental fixtures with label collisions. Numeric clipping bounds, dotted Korean identifiers and original failure details are preserved. Catalog contract, JavaScript syntax, R menu parsing and targeted whitespace checks pass. Current and accumulated five-format export checks include the updated supplementary fixture.

Limit: the MI/IPW messages are tested using the exact generated message forms, not new end-to-end MI/IPW/WGEE fits. MI messages with appended weight/failure suffixes, other model-specific prose and full manuscript paragraphs remain for subsequent review. No whole-application completion claim.
