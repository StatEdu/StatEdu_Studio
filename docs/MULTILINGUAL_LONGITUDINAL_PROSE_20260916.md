# Longitudinal diagnostic prose — 2026-09-16

Source-only continuation; no installer built or installed.

Added 22 English catalog sources and their Japanese, Chinese, Spanish, French, German and Vietnamese translations. Existing Korean translations remain in the longitudinal helper.

The added sources cover weights, assumption screening, Shapiro-Wilk decisions, sensitivity-reporting guidance, GEE rationale, and generated messages for working correlation, random effects, excluded observations, software versions and diagnostic counts.

Non-Korean supplementary tables now call the longitudinal text helper, including its complete-message templates. Captured variable names, counts, structure names and software versions are inserted after translation. Only the known two-sentence GEE rationale is split; arbitrary prose is not split at periods, preserving identifiers and version numbers. Existing identity-column protection restores user labels even when they equal diagnostic prose. Main-table output is unchanged.

Validation:

- Actual Gaussian GEE result rendered in all eight languages; main-table cells, headings and notes match English.
- Six foreign-language prose tests cover original variable labels, dotted Korean identifiers, counts, version strings, and the combined GEE rationale.
- Existing structural supplementary and NA text checks pass.
- Current and accumulated HTML/PDF/Word/HWPX/Excel exports preserve the changed fixture; actual PDF text checks pass.
- Catalog contract and targeted whitespace checks pass.

Remaining work includes full generated Methods paragraphs, MI/IPW/WGEE detail and failure messages, weighted-analysis summaries, model-specific recommendations, and other longitudinal/structural diagnostic branches. This change does not establish whole-application multilingual completion.
