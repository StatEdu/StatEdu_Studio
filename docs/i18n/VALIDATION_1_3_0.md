# 1.3.0 documentation validation

Scope: Analyses, Method Notes and Version History in ko, en, ja, zh, es, fr, de and vi. Analyses has 18 topics; Method Notes has 27 chapters covering assumptions, formulas, diagnostics and interpretation. Current 1.3.0 release highlights have the same five entries in every language.

| Check | Result |
| --- | --- |
| Generated-source consistency (`generate_localized_docs.py --check`) | Pass: 25 generated files |
| Actual About document resolver and Markdown rendering | Pass: 24 distinct language-specific document paths |
| Topic order, unique anchors and rendered contents links | Pass: 18 analysis topics and 27 methodology chapters per language |
| Methodological equations and comparison scope | Pass: equations retained; SPSS, AMOS and SmartPLS covered in all 8 languages |
| Public validation presentation | Pass: development comparisons grouped as incorporated into 1.3.0; private detailed-evidence links removed; remaining differences retained |
| App directory lookup from a different working directory | Pass: all 8 languages |
| Current-version metadata | Pass: 1.3.0 |
| Complete Version History (2026-09-16) | Pass: 93 releases from 1.3.0 through 0.1.0 in all 8 languages; identical version/date ordering |
| Six translated historical archives | Pass: each has 92 historical releases, 460 bullets, 114 subsections and 47 inline technical literals; per-release counts and literal values match English |
| History browser rendering | Pass: actual About Markdown renderer output opened in Chrome for all 8 languages; 93 headings and accessible 0.1.0 section, no horizontal overflow at 1280px |
| Documentation UTF-8 validation | Pass, including all 16 current analysis/method documents |
| Existing PLS missing-data documentation contract | Pass |
| Installer regression gate contract | Pass |
| Packaging selection against localized manifest | Pass: all 24 document paths selected |
| Whitespace errors in modified tracked documentation/integration files | None |

Commands: `scripts/validate_localized_about_docs.R`, `scripts/validate_version_metadata.R`, `scripts/validate_document_encoding.R`, `scripts/validate_pls_missing_policy.R`, `scripts/validate_installer_regression_gate.R`. Windows R checks use `LANG` and `LC_ALL=Korean_Korea.utf8`.

Historical validation commands: `scripts/validate_changelog_history.py`, `scripts/validate_changelog_render.R`, `scripts/validate_changelog_browser.cjs`. Chrome screenshots are generated under `tmp/changelog-history`; Japanese and Chinese first-release sections were visually inspected.

Limits: these checks validate source content structure, routing, encoding, HTML generation and standalone browser rendering; they are not a native-speaker review or an installed-application screenshot review. Full Korean/English Method Notes are restored and updated; the other six languages contain expanded explanations and the original calculator equations, not a sentence-by-sentence translation of the full Korean/English text. Historical Version History entries are fully translated into all six additional languages. Other About tabs retain their existing coverage. The existing installer has not been rebuilt with this documentation revision.
