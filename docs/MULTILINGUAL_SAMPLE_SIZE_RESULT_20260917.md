# Sample-size result prose, first batch — 2026-09-17

## Scope

Three mean-difference effect-size formula explanations (paired, one-sample and independent means), three sample-size/group-count validation messages, and the References heading are localized at presentation time. Exact English source matching preserves unknown/external messages. Numeric calculation objects and original references are unchanged. Remaining method/formula descriptions and errors are not claimed complete.

Dictionary: `scripts/fill_sample_size_result_i18n.py`, six keys × eight languages, integrated by the shared dictionary task.

## Verification

- `validate_sample_size_result_i18n.R`: actual `sample_size_calculate` results for three designs, translated prose/reference headings across eight languages, serialized source-result invariance.
- Current and accumulated captures in Korean and Japanese were written through common HTML, DOCX, native HWPX, XLSX and PDF writers. Checks cover table cells, reference entries/headings and translated formulas in HTML/DOCX/HWPX/XLSX.
- `validate_sample_size_result_pdf.py`: PDF text extraction confirms expected content in all four PDFs (four pages current, seven accumulated). Unicode NFKC normalization is required because the PDF extractor maps Japanese 文 to its compatible Kangxi radical; no source/output replacement was applied.
- `validate_sample_size_result_errors.R`: three errors triggered from actual calculation functions × eight languages; existing sample-size/effect-size numerical suite passed.
- `git diff --check` passed for the changed R file.

Artifacts are under `tmp/sample-size-result-i18n`. PDF rendering initially hit sandbox process restrictions; the approved local renderer run completed. These are content/structure checks, not a fresh visual review in Word/Hancom. No production restart or installer build occurred. Sample-size panels have no new save buttons; current/accumulated capture compatibility was verified via shared writers.
