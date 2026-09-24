# GEE and LMM planning descriptions

Date: 2026-09-18

Four exact-source descriptions now use eight-language entries: GEE working-correlation adjustment, GLIMMPSE-style `nlme::gls` simulation, closed-form `longpower::diggle.linear.power`, and random-intercept `nlme::lme` simulation. Upper-triangle correlation examples, engine names and method distinctions are retained. Numeric engines and input behavior are unchanged. Dynamic method notes containing simulation counts remain outside this batch.

Implementation: `scripts/fill_longitudinal_planning_i18n.py`, merged by the shared dictionary owner; exact-source rendering lookup in `R/sample_size_ui.R`.

Verification:

- `fixtures_longitudinal_planning_i18n.R`: eleven actual calculations through `sample_size_calculate`. GEE exchangeable/AR1/unstructured cases run in sample-size and achieved-power modes (six cases). LMM uses longpower in both modes, one-group lme simulation, one-group gls simulation and unstructured two-group gls simulation (five cases).
- All four source descriptions and engine/correlation tokens checked across eight languages. Longpower results must identify the actual engine; achieved powers must be finite in [0,1]. Simulation fixtures use 20 replicates to exercise their code paths, not to establish Monte Carlo precision.
- Current and accumulated Korean/Japanese snapshots include all eleven results. Content checks passed for HTML, PDF, Word, native HWPX and Excel with unchanged serialized results. Separate PDF text checks passed: Korean 10/19 pages and Japanese 11/21 pages (current/accumulated).
- Existing numerical regression suite and three actual validation errors across eight languages passed. Scoped whitespace checks passed.

Artifacts: `tmp/longitudinal-planning-i18n`. Automated content/structure checks, not a new browser or Word/Hancom visual review. No LMM simulation-based sample-size search was tested in this batch; closed-form LMM sample size was tested. Production sessions were not restarted. Survival/equivalence/diagnostic and other planning descriptions remain.
