# Method recommendation panel

Localized 32 panel/candidate strings for six additional languages, including reasons, limitations, roles, non-estimator candidate labels and summary templates. Added missing Korean reasons/limitations. The recommendation computation and method identifiers remain unchanged.

Nominal-indicator limitations contain user variable names rather than program prose. They explicitly bypass translation, including names equal to catalog entries such as `Review` and `Primary`.

`scripts/validate_method_panel_i18n.R` passed all eight languages with ten actual recommendation cases and two selected methods each. It covers nominal and ordered indicators, unspecified/common-factor/composite/mixed constructs, predictive and confirmatory objectives, all candidate reasons/limitations, and user-name preservation. Generated panel markup was evaluated; no browser interaction was performed. Common multilingual coverage and MI/estimator dialog regression passed.

Logs: `tmp/method-panel-validation.log`, `tmp/method-panel-coverage.log`, `tmp/method-panel-dialog-regression.log`.

Only setup UI changed. No result snapshot/export changes or installer build occurred; five-format export checks were not repeated.

## Remaining MI skipped-reason work

`structural_canvas_mi_evaluation.R` currently combines user path labels with admissibility reasons or raw lavaan errors into `path [reason]`, then joins candidates with ` | `. Both the interaction modal and supplementary result table consume this text. A blanket string replacement could alter user names. This needs a separate structured/protected presentation change with current and accumulated export validation if the shared result table is changed. It remains unmodified in this pass.
