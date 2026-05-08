# EasyFlow Regression

EasyFlow Regression is a local Shiny application for assumption-guided regression analysis.

The app runs on the user's own Windows PC and opens in a local browser session. Data are analyzed locally and are not sent to an external server.

## Current Scope

- Multiple regression
- Residual normality test using the Lilliefors corrected Kolmogorov-Smirnov test
- Homoscedasticity test using the Breusch-Pagan test
- Automatic selection among OLS, HC3 robust standard errors, bootstrap confidence intervals, and HC3 plus bootstrap
- Durbin-Watson decision using dL and dU critical values from the EasyFlow statistics workbook

## Local Run

1. Install R.
2. Unzip the EasyFlow Regression folder.
3. Double-click `EasyFlow_Regression.bat`.

The app will open at `127.0.0.1` in the default browser.

## Development Model

This project is developed privately and released publicly after validation. Public releases should include source code, documentation, example data, and validation notes.

