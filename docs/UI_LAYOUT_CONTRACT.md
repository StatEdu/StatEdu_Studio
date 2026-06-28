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
- Button row: Block 1 commands use column 1, Block 2 commands use column 3,
  and Block 3 commands use column 5 of the same grid. Do not center footer
  buttons across the whole workspace.
- Footer button widths: Block 1 and Block 2 commands use
  `--se-standard-inner-button-width`; Block 3 commands use
  `--se-standard-options-button-width`.

Standard analysis and Data Editor menus use shared geometry variables declared
on `body` in `www/style.css`:

- `--se-analysis-workspace-width`: standard analysis workspace, heading, data
  viewer, setup-grid, and action-row width; analysis tools use `1140px`
- `--se-analysis-grid-columns`: standard analysis three-block column structure
- `--se-standard-setup-width`: setup grid and action row width; standard
  Data Editor three-block tools use `1176px`, including setup-grid padding
  where relevant
- `--se-standard-panel-width`: Block 1 and Block 2 panel width
- `--se-standard-options-width`: Block 3 options panel width
- `--se-standard-panel-height`: panel height
- `--se-standard-gap`: spacing between blocks
- `--se-standard-transfer-width`: transfer button column width
- `--se-standard-inner-button-width`: Block 1 and Block 2 footer button width
- `--se-standard-options-button-width`: Block 3 footer button width
- `--se-standard-panel-padding`: setup grid internal padding

Data Editor standard three-block tools use the same panel geometry, with
internal grid padding included in the total width. `.data-editor-workspace`
is still required so the Data Editor-specific overrides apply only inside the
Data Editor menu:

- Every standard Data Editor tool must render inside the
  `.data-editor-workspace` wrapper so the shared geometry applies.
- Data Editor-specific width, panel height, transfer alignment, and action-row
  placement must reference the standard variables instead of hard-coded copies.

### Wide to Long

`Wide to Long` is a standard Data Editor three-block tool, not a layout
exception. Block 2 may contain both `Repeated columns` and
`Configured long variables`, but the outer grid still follows the standard
three-block geometry.

- `Run` sits under Block 1.
- `Remove` sits under Block 2.
- `Preview` sits under Block 3.
- `Set variable` stays inside Block 3, uses the standard Block 3 button width,
  and is anchored to the bottom of the options content instead of moving when
  the active tab changes.
- The two Block 2 list panels should share the available Block 2 height evenly
  enough that their list bottoms align with the source variable panel.

## Known Exceptions

These screens do not use the standard three-block contract directly:

- Longitudinal / Panel Models: four-block analysis structure
- Latent / Mplus: step-based workflow
- Data tab: data loading and review workflow
- Calculators: input-focused tools
- Auto Likert conversion: detection/review workflow with grouped result tables
  and dictionary controls
- Variable transformation: formula-builder workflow with a source variable
  browser and expression editor

Exception screens should still keep their own internal geometry consistent.
The only remaining hard-coded copy of the standard three-block width is
`.calculator-action-row`, because calculators are explicitly excluded from the
standard analysis/Data Editor layout contract.

## Navigation Contract

Grouped top-level menus must keep their active state synchronized with the
visible page:

- Analysis, Sample Size, and Effect Size use grouped dropdown sections.
- Selecting a nested item must activate the owning top-level menu and clear
  stale active state from other top-level menus.
- Clicking a navbar item that is already marked active must still call the tab
  navigation path so users can return to that page after visiting another
  top-level menu.
- Data Editor lazy menu items must map one menu item to one server-side lazy
  output target.

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
