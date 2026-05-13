# Method Notes

EasyFlow Statistics applies regression diagnostics before selecting the inferential output.

## Normality

Residual normality is evaluated using the Lilliefors corrected Kolmogorov-Smirnov test.

## Homoscedasticity

Homoscedasticity is evaluated using the Breusch-Pagan test.

## Robust Standard Errors

When heteroscedasticity is detected, HC3 robust standard errors are reported.

## Bootstrap

When residual normality is not supported, bootstrap confidence intervals and bootstrap p values are reported.

