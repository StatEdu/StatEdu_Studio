# Basic sample-size planning descriptions

Date: 2026-09-18

Six exact-source planning descriptions are localized in eight languages: t-test, rank-based omnibus approximation, Wilcoxon/Mann–Whitney approximation, one/two proportions, chi-square and Pearson correlation. Exact/approximate method distinctions and the unequal-allocation qualification are retained. Calculations, references and stored numeric results are unchanged. This does not complete all planning descriptions or method notes.

Implementation: `scripts/fill_basic_planning_i18n.py`, merged by the shared dictionary owner, and exact-source rendering lookup in `R/sample_size_ui.R`.

Validation scope:

- `scripts/fixtures_basic_planning_i18n.R` uses eleven real `sample_size_calculate` cases in both sample-size and achieved-power modes: independent, one-sample, paired and unequal-allocation t-tests; two-independent rank, Kruskal–Wallis and Friedman tests; one/two proportions; chi-square; correlation.
- Eight-language rendering is checked in both modes, with nonempty exact-source dictionary mapping. An independent t-test is checked against `stats::power.t.test`; all achieved powers must be finite and between zero and one.
- The common result/export validator passed unchanged serialized results and Korean/Japanese current and accumulated sample-size result content in HTML, PDF, Word, native HWPX and Excel. Separate PDF extraction checks passed: 6 pages current and 11 accumulated in each language. Achieved-power results were rendered in eight languages, not separately exported in this batch.
- Existing numerical regression suite and three actual error translations across eight languages passed. Scoped whitespace check passed.

Artifacts: `tmp/basic-planning-i18n`. Automated content/structure checks; no fresh browser or Word/Hancom visual review. Production sessions were not restarted. ANOVA/ANCOVA, regression and other planning descriptions remain.
