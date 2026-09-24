# Panel diagnostic guidance — 2026-09-16

Source/catalog-only continuation; no installer built or installed.

Added 23 source phrases with Japanese, Chinese, Spanish, French, German and Vietnamese translations. Existing Korean text remains in the localizer. Coverage includes insufficient residual pairs, unavailable plm/lmtest packages, unavailable Hausman/Pesaran CD tests, cross-sectional dependence decisions, random-effects independence decisions, FE/RE recommendations, singular-fit recommendations, and GEE/LMM/GLMM/panel interpretation notes.

The translations retain the distinction between failure to reject an assumption and establishing that assumption. Statistical calculations, tab identities, publication tables and user labels are unchanged.

Validation: all eight language runs of the actual Gaussian GEE/LMM/panel-FE renderers preserve English publication content. The supplementary fixture now tests these 23 messages alongside the previous 20 model-diagnostic phrases and deliberately identical user labels. Catalog and targeted whitespace checks pass. Current/accumulated exports exercise the expanded fixture in HTML/PDF/Word/HWPX/Excel, with actual PDF text checked separately.

Limitations: unavailable-package/test messages are exercised as representative generated strings, without removing installed packages. Other recommendations, full composed manuscript paragraphs, weighted summaries and other analysis families remain in the audit backlog. This is not a whole-application completion claim.
