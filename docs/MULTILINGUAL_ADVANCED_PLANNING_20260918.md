# Cluster, reliability and SEM planning descriptions

Date: 2026-09-18

Ten exact-source descriptions localized in eight languages: stepped-wedge simulation, WebPower parallel clusters, standard cluster design effect, four reliability precision methods, SEM complexity heuristic, approximate parameter-power simulation and RMSEA power. Formula tokens and the limitation that parameter-power draws do not generate/refit a complete SEM are retained. Other static/dynamic method notes remain outside this batch.

Implementation: `scripts/fill_advanced_planning_i18n.py`, merged by the shared dictionary owner; exact-source lookup in `R/sample_size_ui.R`. No numerical algorithm or menu changes.

Verification:

- `fixtures_advanced_planning_i18n.R` runs ten actual `sample_size_calculate` cases: stepped-wedge achieved power; parallel continuous WebPower and binary design-effect sample sizes; Bland–Altman/alpha/ICC/kappa sample sizes; SEM complexity and RMSEA sample sizes; approximate SEM parameter power.
- Actual WebPower engine identification, Bland–Altman closed-form sample-size reference, finite power bounds and eight-language mathematical/engine tokens checked. Stepped-wedge uses 20 replicates and SEM parameter power 100 draws for path coverage, not simulation-precision evidence.
- All ten results passed Korean/Japanese current and accumulated content checks through the five common writers, with serialized calculation results unchanged. Separate PDF extracted-text checks passed: 11 current / 21 accumulated pages in each language.
- Existing numerical regression suite and three actual errors across eight languages passed. Scoped whitespace checks passed.

Artifacts: `tmp/advanced-planning-i18n`. Automated content/structure checks, not a fresh browser or Word/Hancom visual review. Stepped-wedge sample-size searching is not covered by this batch. Production sessions were not restarted. Remaining English method notes and coverage gaps require a further audit before claiming full translation completion.
