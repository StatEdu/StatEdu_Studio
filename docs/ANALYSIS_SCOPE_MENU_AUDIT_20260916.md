# Case-selection / split-file menu audit — 2026-09-16

The mediation/moderation worker failure was reproduced with scoped data:
`could not find function "analysis_scope_prepare_variables"`. Its fresh callr
process now sources `analysis_scope.R` before preparing analysis roles.

## Additional menu checks

No additional failure was found in 72 engine comparisons (36 configurations,
two independent 90-row subsets). Identical random seeds were used for scoped
and manually subsetted inputs; complete returned objects were compared with
`all.equal(..., check.attributes = FALSE)`.

- Frequencies, crosstabs, t-test/ANOVA, ANCOVA, paired tests, mixed and one-group
  repeated ANOVA, independent and paired nonparametric tests.
- Pearson/Spearman correlation, reliability, inter-rater agreement, EFA, PCA.
- Linear, hierarchical, logistic and generalized linear regression.
- Kaplan–Meier, Cox regression, competing risks, longitudinal GEE and LMM.
- Complex-sample frequencies, crosstabs, group comparisons, correlation,
  linear regression and logistic regression.
- CFA, CB-SEM, PLS-SEM, Ridge, LASSO and Elastic Net.

Reproduction scripts:
`scripts/validate_analysis_scope_menus.R` and
`scripts/validate_analysis_scope_structural.R`.
CSV evidence: `tmp/analysis-scope/menu-engine-audit.csv` and
`tmp/analysis-scope/structural-penalized-audit.csv`.

Additional passing checks:

- `validate_analysis_scope_startup.R`: real app server and both scope panels.
- `validate_analysis_scope_conditions.R`: category expressions, missing values,
  inclusive/exclusive bounds, OR conditions and overlapping split rejection.
- `validate_scope_variable_exclusion.R`: excluded terms match manual omission,
  clearing scope restores eligibility, canvas pruning preserves source nodes.
- Prior fix validation: actual mediation/moderation callr workers match manual
  subsets; shared sequential dispatch and current/accumulated five-format export
  tests pass.

## Coverage limits

These are synthetic engine and server checks, not a browser click-through of
every menu or every estimator/option. The structural comparisons use fitted
models without exhaustively rerunning all background bootstrap variants.
Penalized tests use two bootstrap draws and one validation repeat to exercise
scope handling, not to establish inferential accuracy. One synthetic LMM subset
reported a singular-fit warning in both scoped and manual runs; results matched.
Meta-analysis study-entry data and sample-size calculators are separate inputs
and are outside loaded-file row filtering. No installer was rebuilt or running
app restarted during this audit.
