# MI history and holdout UI

Localized history/holdout headings, six explanatory paragraphs, sample-count and seed templates, all five validation-gate labels, blank-justification placeholder, three table headings and two comparison-status messages. History cells bypass translation after formatting, preserving user parameters and substantive justification even when equal to `Review` or another dictionary entry.

`validate_mi_history_holdout_i18n.R` exercises actual CFA/holdout fits and renders eight languages for reserved, evaluated and inadmissible states. The inadmissible display case is a modified fixture; the evaluated case uses fitted results. It also checks five gate labels, user strings, counts/seed and missing-code fallback. Common multilingual coverage and prior MI candidate/notes checks passed.

Japanese current/accumulated export fixtures include history plus all three holdout states. Five-format content checks, PDF extracted text/cover and table order/count are recorded in `tmp/mi-history-holdout-exports.log` and the export validation command results. Artifacts: `tmp/mi-history-holdout-i18n/ja-{current,accumulated}.*`.

No installer was built. Remaining limitation: dynamic diagnostic text inside the holdout table's admissibility-reasons cell is still inherited from its stored engine result. Arbitrary engine/user text is not globally replaced. This pass does not certify every application screen or runtime branch.
