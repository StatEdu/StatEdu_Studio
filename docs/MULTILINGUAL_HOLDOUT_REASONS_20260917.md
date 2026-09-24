# Holdout admissibility reasons

Holdout comparisons now retain per-model admissibility reason vectors alongside their original English table strings. A shared reason renderer handles both MI skipped candidates and holdout models. Known diagnostics follow the UI language; variable/group names, numeric details and unknown engine text remain literal. Protected reason cells bypass subsequent generic table translation.

Legacy saved comparisons without separate reason vectors keep their joined original text. Estimation, admissibility decisions, change-statistic suppression and English result values are unchanged.

Validation:
- `validate_holdout_reasons_i18n.R`: all eight languages, ten diagnostic forms, literal names containing separators/brackets/percent signs, raw unknown text, legacy results and actual rendered cells.
- An actual holdout CFA with a fixed negative residual variance confirms inadmissibility, recorded reasons and suppressed difference statistics. Additional diagnostic forms use supplied fixtures.
- Existing MI skipped-reason and CFA/holdout checks passed; common multilingual coverage passed.
- Current and accumulated Japanese snapshots are checked in HTML, PDF, Word, HWPX and Excel, with PDF text/cover and table order/count verification.

The generic PDF contiguous-text check initially failed for the deliberately long combined-reasons cells. Inspection showed exact repeated table headers inserted at page breaks inside the extracted cell text, not missing content. `validate_holdout_reasons_pdf.py` passes current and accumulated PDFs while allowing only complete exported table-header strings as interruptions; all cell text, numbers and punctuation must still match. Table counts/order passed (six current, eight accumulated).

Artifacts: `tmp/holdout-reasons-i18n/ja-{current,accumulated}.*`. Logs: `tmp/holdout-reasons-{validation,mi-regression,coverage,exports}.log`. No installer was built. This completes the previously recorded holdout-reason gap for known diagnostics, not application-wide multilingual certification.
