# Assumption summaries and repeated-measures REML notes

Source-only continuation, 2026-09-16. No installer was built or installed.

Added 14 catalog entries in all eight languages: 11 assumption labels, the flagged-assumption summary template, the repeated-measures marginal-model rationale and its UN/AR(1) sensitivity override. Residual normality already had a translation, giving 12 tested check labels.

Flagged-assumption summaries now localize the embedded application check names before inserting them into the translated sentence. The common path also fixes Korean summaries with English check labels. REML rationale translates the sentence while preserving the covariance identifier and method names.

Tests generate an Assumptions paragraph through the production manuscript builder with all 12 check labels flagged. Every check label translates in all seven non-English locales, and none of the original English labels remain in that paragraph. REML fixtures preserve AR(1)/UN and user labels identical to source prose. Existing actual GEE/LMM/panel-FE main-table language and user-label checks pass in all eight locales.

Current/accumulated HTML, PDF, Word, HWPX and Excel export fixtures include both additions. Catalog and targeted whitespace checks pass; actual PDF text is checked separately.

Limitations: REML rendering was checked using production message forms, not a new mmrm fit. Remaining work includes weighted-summary prose and outstanding UI/result strings across other analysis families. Whole-application multilingual completion is not claimed.
