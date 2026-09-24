# Precision, McNemar and rate planning descriptions

Date: 2026-09-18

Seven exact-source descriptions localized in eight languages: mean/proportion/correlation precision, McNemar, negative-binomial rates, single Poisson rate precision and two independent Poisson rates. Formula tokens, normal/Wald approximation qualifications, discordant-pair probabilities and person-time units are preserved. Dynamic and other method notes remain outside this batch.

Implementation: `scripts/fill_precision_rates_planning_i18n.py`, integrated by the shared dictionary owner; rendering lookup in `R/sample_size_ui.R`. No calculation changes.

Verification:

- `fixtures_precision_rates_planning_i18n.R` runs fourteen actual wrapper cases: seven designs in sample-size and achieved-power/precision modes.
- Mean/proportion sample sizes and achieved half-widths are compared with normal-quantile formulas. Single-rate required person-time and achieved half-width are also checked. Positive negative-binomial dispersion must increase required person-time over the matched Poisson case. Actual powers must be finite in [0,1]. Precision cases are tested as half-widths, not as power.
- Mathematical tokens passed in eight languages. Current/accumulated Korean/Japanese content comparisons passed all fourteen results through the five shared writers, with immutable serialized results. Separate PDF extracted-text checks passed: 9 current / 17 accumulated pages in each language.
- Existing numerical regression suite and three actual validation errors across eight languages passed. Scoped whitespace checks passed.

Artifacts: `tmp/precision-rates-planning-i18n`. Automated content/structure checks, not a new browser or Word/Hancom visual review. Production sessions were not restarted. Cluster, reliability, SEM and remaining method-note descriptions need further work.
