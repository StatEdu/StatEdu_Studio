# StatEdu Studio UI Layout Contract

This document defines the layout baseline for the StatEdu Studio 1.0
stabilization phase. The goal is to prevent repeated per-menu spacing fixes
by keeping common menu structures on shared geometry.

## Principles

- Freeze new analysis features unless they are required to fix correctness.
- Prefer one shared layout rule over per-menu width, height, or margin tweaks.
- Use `t-test / ANOVA` as the baseline for standard three-block analysis tools.
- Keep Data Editor tools that follow the same three-block structure aligned to
  the same baseline.
- Treat structurally different screens as explicit exceptions instead of
  forcing them into the standard layout.

## Standard Three-Block Layout

Standard three-block tools use this structure:

- Block 1: source variable list
- Block 2: selected or target variables
- Block 3: options
- Header: `View selected data` aligns with the right edge of the last block.
- Footer buttons: each command sits under the block it affects.

Standard analysis menus use the shared analysis geometry in `www/style.css`:

- Setup grid width: `1140px`
- Action row width: `1140px`
- Grid columns: `326px 50px 326px 20px 310px`
- Gap: `18px`

Data Editor standard three-block tools use the same panel geometry, with
internal grid padding included in the total width. The shared CSS variables
live in `www/style.css` under `.data-editor-workspace`:

- `--se-standard-setup-width`: setup grid and action row width; standard
  Data Editor three-block tools use `1176px`, including setup-grid padding
- `--se-standard-panel-width`: Block 1 and Block 2 panel width
- `--se-standard-options-width`: Block 3 options panel width
- `--se-standard-panel-height`: panel height
- `--se-standard-gap`: spacing between blocks
- `--se-standard-transfer-width`: transfer button column width
- `--se-standard-inner-button-width`: Block 1 and Block 2 footer button width
- `--se-standard-options-button-width`: Block 3 footer button width
- `--se-standard-panel-padding`: setup grid internal padding

## Known Exceptions

These screens do not use the standard three-block contract directly:

- Longitudinal / Panel Models: four-block analysis structure
- Latent / Mplus: step-based workflow
- Data tab: data loading and review workflow
- Calculators: input-focused tools

Exception screens should still keep their own internal geometry consistent.

## Change Checklist

Before changing menu spacing:

1. Decide whether the screen is a standard three-block tool or an exception.
2. For standard three-block tools, use the shared `.data-editor-workspace`
   variables first.
3. Add per-menu overrides only when the menu has a documented structural reason.
4. Compare against `t-test / ANOVA` and the affected Data Editor menu.
5. Run `scripts/validate_stabilization.ps1` for the core stabilization checks,
   or `scripts/validate_stabilization.ps1 -Full` before release packaging.
6. Check that `View selected data`, setup panels, and footer buttons align with
   the shared geometry.
