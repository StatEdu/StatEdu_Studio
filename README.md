# easyflow_statistics

easyflow_statistics is a local Shiny application for assumption-guided regression analysis.

The app runs on the user's own Windows PC and opens in a local browser session. Data are analyzed locally and are not sent to an external server.

All statistical analyses use CRAN packages only.

## Current Scope

- Multiple regression
- Residual normality test using the Lilliefors corrected Kolmogorov-Smirnov test
- Homoscedasticity test using the Breusch-Pagan test
- Automatic selection among OLS, HC3 robust standard errors, bootstrap confidence intervals, and HC3 plus bootstrap
- Durbin-Watson decision using dL and dU critical values from the easyflow_statistics workbook

For the full current method inventory, see [docs/ANALYSIS_METHODS_KO.md](docs/ANALYSIS_METHODS_KO.md).

## Local Run

1. Install R.
2. Unzip the easyflow_statistics folder.
3. Double-click `easyflow_statistics.bat`.

The app will open at `127.0.0.1` in the default browser.

## Citation

If you use easyflow_statistics in your research, please cite:

LEE, I. H. (2026). easyflow_statistics (Version 0.5.7) [Computer software]. https://doi.org/10.22934/statedu.easyflow.statistics

## Development Model

This project is developed privately and released publicly after validation. Public releases should include source code, documentation, example data, and validation notes.

