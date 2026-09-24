# Unified regularized regression menu

One menu, `릿지·라소·엘라스틱넷`, follows Regression. Its method option selects Ridge, LASSO or Elastic Net. The chosen method alone runs; the existing combined VIF-triggered workflow still defaults to all three methods.

The menu owns outcome/predictor assignments, bootstrap resamples and seed, and does not require an earlier ordinary regression. The shared model-data conversion handles categorical predictors and reference levels. The scope dispatcher and scoped data exclusions apply to case selection and split runs. OLS coefficients provide the existing comparison reference only.

Blocks 1 and 2 reuse the regression layout and `hierarchical_target_panel`: the available-variable list, separate dependent/predictor panels, count/type labels, aligned transfer arrows, and up/down ordering controls. Multiple continuous outcomes run separately. Selecting a target variable and pressing its arrow returns it to the available list; double-click also removes it. Block 3 retains the regularized-regression method options.

Current HTML/PDF/Excel and figure saves capture displayed tables and images. Result addition uses the same snapshot for accumulated Word/HWPX and other formats. Plot IDs are unique to this menu. UI setup changes do not alter previous calculated results.

Validation: `validate_penalized_menu.R`, `validate_penalized_menu_startup.R`, `validate_penalized_menu_ui.cjs`, and `validate_penalized_menu_exports.R` check all three methods, the single menu, initial lazy registration, scope exclusion, actual rendered plots, and current/accumulated five-format content preservation. Tests use two bootstrap resamples for speed; the user-facing default is 500.
