# Four hierarchical regression blocks

The regression setup now supports four cumulative blocks. The final model's complete cases determine the sample for every stage. Existing three-block function calls and commands remain valid.

Block 4 participates in assignment, removal, up/down and shared pointer reordering, scope-variable exclusion, reset, variable rename, command capture/execution, and settings save/restore. Wide result tables render Model 1 through Model 4 using the existing dynamic renderer.

Validation: `scripts/validate_hierarchical_four_blocks.R` checks numerical parity against independent `lm` fits, common sample size, scope exclusions, legacy three-block behavior, command round trips, navigation, assignment, transfer, valid/invalid reordering, and removal. `scripts/validate_regression_syntax.R` also passes.

Current and accumulated HTML, Excel, Word and PDF exports were exercised. HWPX conversion was attempted for both scopes but the installed Hancom conversion timed out; this format remains unverified. No Hancom security settings were changed.

The analysis task's captured six-entry history and exports are retained separately from the user's existing Studio history.
