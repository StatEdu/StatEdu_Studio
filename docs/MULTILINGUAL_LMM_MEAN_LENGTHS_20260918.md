# LMM mean-list lengths and minimum time points

Date: 2026-09-18

Four existing messages now display in eight languages: minimum two time points and equal group mean-list lengths in the effect calculator, plus their field-specific counterparts in sample-size planning. Exact lookup in `R/sample_size_ui.R`; shared dictionary owner applied `scripts/fill_lmm_mean_lengths_i18n.py`. Numeric validation, calculations and original errors are unchanged.

`scripts/fixtures_lmm_mean_lengths_i18n.R` checks eight actual failures across eight languages: one time point in one-/two-group designs and shorter/longer Group 2 lists in both calculation paths. Two valid two-time-point effect estimates match the independent .4 reference; valid two-group LMM power is retained. Existing seven matrix-error cases, three matrix references, three LMM outputs and unknown-text preservation pass.

Six valid outputs pass Korean/Japanese current and accumulated HTML, PDF, DOCX, native HWPX and XLSX content verification (PDF 7/13 pages). Existing numerical regression/validation tests and scoped whitespace checks pass. Errors remain transient warnings. Artifacts: `tmp/lmm-mean-lengths-i18n`.

Automated content/structure checks only; no fresh browser/editor visual inspection, production restart, or installer rebuild. Other validation translations remain unfinished.
