# Direct HWPX export

User authorization: implement the previously planned 1.3.1 direct HWPX writer now. Paper-size selection remains deferred; no installer has been built or released by this change.

`result_document_model()` parses captured result entries into ordered editable tables, paragraphs, notes, gaps and images. It reuses shared table layout and B5 page geometry. Word and HWPX each consume this model; neither reruns analysis. Temporary images are cleaned up on success and failure.

`write_result_collection_hwpx_native()` emits OWPML XML and a ZIP package directly using R's xml2/zip libraries. There is no DOCX staging document, PowerShell converter, COM automation or installed-Hancom requirement in production. Text, cell spans, superscripts, column widths, alignment, section orientation and original image bytes are preserved. The native template consists of sanitized empty-document defaults. The installer staging checks require those assets.

Word/HWPX remain available only in accumulated Results; HWPX remains Korean-only. Native documents contain editable tables and paragraphs. Pagination can differ between editors; PDF remains the fixed-layout reference.

Validation:

- `scripts/validate_native_hwpx.R`: current/accumulated packages; stored first mimetype; merged-cell grid; precision, Korean, escaping, notes and superscripts. Word writer and external-process launch are replaced with failures during native save.
- `scripts/validate_ipa_export_actions.R`: real snapshot/add-result/save handlers; five formats for current/accumulated snapshots.
- `scripts/validate_ipa_i18n.R`: eight-language report invariants and current/accumulated five-format exports.
- `scripts/validate_word_cell_widths.R`: Word merged-cell widths and markers after sharing the document model.
- User history `sample/StatEdu_Studio_result_history_20260917_154949.efs-result`: 25 tables, 1,657 cells and five figures. Native HWPX opened, saved back to editable DOCX/HWPX and rendered to PDF in an isolated hidden Hancom instance. All text, table order and image bytes matched the reference. All section dimensions and column widths matched. Narrow separator padding is reduced to prevent Hancom from expanding the column. Table bottom margin separates notes from the border.
- Word generated through the shared model matches the prior writer's text, cell properties, grids, paragraphs and sections.

On the supplied history, native whole-document saving measured 3.26–4.36 seconds over development runs, versus the prior measured 17.16 seconds for Word-to-HWPX conversion. These are local observations, not a fixed performance guarantee. Benchmark and round-trip artifacts are under `tmp/user-history-hwpx-benchmark/`.

References: [Hancom OWPML model](https://github.com/hancom-io/hwpx-owpml-model), [Hancom HWPX format explanation](https://tech.hancom.com/python-hwpx-parsing-1/).
