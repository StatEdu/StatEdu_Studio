# Word export performance — 2026-09-17

The Word writer repeatedly discovered XML namespaces while visiting every table cell to add explicit Hancom-compatible cell widths. Cache the document namespace map once and pass it to XPath and qualified attribute reads. Keep the existing cell-width repair and all document formatting.

Fixture: three multilingual grouped pre/post IPA entries from tmp/ipa-i18n/entries.rds. Profiled wall time on the same machine: 23.18 seconds before, 17.17 seconds after (26% reduction; timing varies with system load). Cached namespace reads remove the dominant repeated discovery cost; flextable construction and serialization remain necessary work.

Before/after DOCX checks: identical text, table grids, cell properties, paragraph properties, sections, and embedded image bytes. scripts/validate_word_cell_widths.R verifies colspan/rowspan widths, superscript markers, displayed precision, and notes. scripts/validate_ipa_export_actions.R covers current and accumulated export paths across HTML/PDF/Word/HWPX/Excel without rerunning analysis.

## User-provided result history benchmark

Input: sample/StatEdu_Studio_result_history_20260917_154949.efs-result (unchanged). Four entries, 25 tables, 1,657 table cells, five images. Same process, same stored snapshots, one timed run per variant; baseline restores the former namespace lookup inside the width repair. File chooser time excluded. Initial package warm-up may affect the first individual result.

| Result | Before (s) | After (s) |
| --- | ---: | ---: |
| Frequencies / Descriptives | 2.09 | 0.81 |
| t-test / ANOVA | 12.31 | 4.21 |
| Hierarchical regression | 4.03 | 3.20 |
| Mediation / moderation | 5.24 | 3.46 |
| All four accumulated entries | 84.94 | 17.59 |

The accumulated document is 79.3% faster (4.8 times the throughput). Repeated document-wide namespace discovery scales poorly with a larger combined XML tree. All five before/after pairs preserve text, table grids, cell properties, paragraph properties, sections, and embedded image bytes. No SEM entry is present in this supplied file; mediation/moderation is not treated as a SEM benchmark.

Reproduce: scripts/benchmark_user_history_word.R and scripts/verify_user_history_word.py. Artifacts: tmp/user-history-word-benchmark.

HWPX benchmark of the same supplied history (current optimized writer, one run each): Descriptives 4.75 s; t-test/ANOVA 7.35 s; Hierarchical regression 5.38 s; Mediation/moderation 6.21 s; all entries 17.16 s. HWPX timing includes intermediate DOCX generation and conversion through installed Hancom. Runs are not simultaneous with the Word benchmarks; system/cache variation precludes treating small cross-format timing differences as significant. All five files pass package validation and retain all reference Word text. Artifacts: tmp/user-history-hwpx-benchmark.


## Follow-up: selective Word and direct HWPX export

The earlier Hancom timings above describe the previous conversion path. Current HWPX writes directly, without Word or Hancom. A further optimization batches Word cell property lookup/removal per table and uses direct child traversal for merged spans. HWPX skips Word-only per-cell border/alignment operations while retaining identical font, padding, merged-cell and superscript geometry used by autofit; its native writer still emits all borders and alignments.

Same supplied history, one profiled run per case (seconds; includes Rprof overhead and normal system/cache variation):

| Selection / format | Before | After |
| --- | ---: | ---: |
| Main tables / Word | 10.36 | 7.90 |
| All / Word | 17.70 | 10.22 |
| Main tables / direct HWPX | 2.64 | 2.50 |
| All / direct HWPX | 3.95 | 3.37 |

Main-only contains 16 tables and no images; all contains 25 tables and 5 images. Word tables (including all cell properties), paragraphs, sections and image bytes compare identically; native HWPX section/header XML and image bytes also compare identically. No result caching or recalculation is introduced.

Measurement: `scripts/benchmark_document_selection_speed.R before` on the pre-change source and `... final` on the final source. Comparison: `scripts/verify_document_speed_fidelity.py`. Artifacts are in `tmp/document-speed`. Remaining cost includes editable table layout, Word XML generation/package finalization, and font metrics. Further changes need measured evidence and fidelity checks; the small main-only HWPX difference should not be treated as a guaranteed speedup.
# Further optimization: repeated saves in the Results session

The Results Word/HWPX confirmation handler now owns a private, bounded document
artifact cache (`result_document_export_cache`). It retains at most one document
per format, with a 32 MiB bound on each artifact and its input signature. Ending
the session removes its temporary files. Uncached writers are unchanged.

Reuse requires identical captured entries (including their order and images),
normalized content selection, language, application configuration, table style,
flextable defaults and export asset hashes. Changed inputs rebuild the document.
Missing/damaged artifacts rebuild too; failed writes are not cached. Existing
open-file protection is retained. Different sessions never share artifacts.

Fixture: `sample/StatEdu_Studio_result_history_20260917_154949.efs-result`.
Measured generation/copy time, excluding the save dialog and application startup:

| Selection | Format | First save | Two repeated saves |
|---|---|---:|---:|
| Main | Word | 7.47 s | 0.01 / 0.00 s |
| Main | HWPX | 2.30 s | 0.00 / 0.00 s |
| All | Word | 10.04 s | 0.00 / 0.00 s |
| All | HWPX | 3.27 s | 0.00 / 0.00 s |

Zero means below the timer resolution, not literally zero work. This optimization
improves repeated saves; first-save timing is essentially unchanged. First-save
measurements are single-run observations, not a claimed new cold-save speedup.

Validation: `scripts/validate_document_export_cache.R` covers repeat byte fidelity,
content/order/selection/language invalidation, isolation, corruption, failed writes,
size limits and cleanup. `scripts/benchmark_document_export_cache.R` records the
fixture timings and verifies byte-identical repeated files. Cold artifacts were
also compared against the previous writer: unchanged table/paragraph/section XML,
styles and image bytes. Selection UI tests and current/accumulated exports for
HTML, PDF, Word, HWPX and Excel passed.

## First-save optimization: header measurements and batched cell formatting

The shared table builder now creates header rows through `add_header_row` instead
of `set_header_df`. The latter measured header text before applying the final
font, padding and superscripts, then `autofit` measured it again. Only the final
measurement is retained. Full final fitting remains necessary for row heights
and uncaptured column widths; it is not replaced with approximate dimensions.

Word cell alignments, vertical alignments and borders are collected over the
captured cell grid and applied to groups with identical row/column properties.
This reduces repeated flextable copies while preserving merged-cell geometry.
No changes were made to the raw Word/HWPX package generation in this step.

Three uncached complete-document generations per variant, alternating before/
after order, on the same 25-table, 5-image supplied fixture:

| Format, all content | Before median | After median | Reduction |
|---|---:|---:|---:|
| Word | 10.58 s | 10.15 s | 4.1% |
| HWPX | 3.28 s | 3.01 s | 8.2% |

This is a modest improvement, with observable run-to-run variation (one Word
pair was slightly slower). It does not use the repeated-save artifact cache.
Warm application libraries are used, as in an already-running Results screen;
application startup and the save dialog are excluded.

Reproduce with `scripts/benchmark_document_cold_batches.R <pre-change-source>`.
Measurements are in `tmp/document-cold-batches/timings.csv`; the local baseline
source is `tmp/result_saved_ui_before_cold.R`. Single profiled generations of
main-only/all output are in `tmp/document-speed/cold-{before,after}-*`.
`scripts/verify_document_speed_fidelity.py cold-before cold-after` verifies
identical Word table/paragraph/section XML, HWPX header/section XML and image bytes.
`scripts/validate_document_cell_batches.R <pre-change-source>` additionally
compares styles, spans, row heights and column widths on 27 tables, including a
mixed-alignment, multiline, Korean, merged-cell and superscript fixture.

## Shared Word styles at table serialization

Word now reuses three paragraph layout styles (alignment, paragraph borders,
spacing and indents) and one character style (fonts, size, color and underline)
on the supplied fixture. Bold, italic, strike-through and superscript remain
direct properties, preserving OOXML toggle semantics. Cell borders, padding,
merged-cell widths, sections and images remain explicit and unchanged. Styles
are private to each generated document, with collision-free IDs.

The shared table writer intercepts the bundled flextable `gen_raw_wml` output
before `body_add_xml`. It preserves post-processing hooks and border repair;
captioned tables and an unavailable serializer use the standard path. This is
a version-sensitive internal serializer boundary, covered by output comparisons.
Post-processing already assembled XML was tested and discarded because its
extra parsing cost outweighed the saving.

Three uncached Word generations per variant, alternating order: before
14.97/9.48/9.56 s; after 11.45/8.98/9.03 s. Medians are **9.56 → 9.03 s (5.5%)**.
The first pair includes initialization effects; do not interpret it as the
steady-state gain. XML body size falls from 4,163,988 to 2,600,356 bytes (37.6%);
the compressed DOCX changes much less, from 358,458 to 345,454 bytes. This step
does not change the already shared native HWPX styles.

Reproduce: `scripts/benchmark_word_shared_styles.R`; results under
`tmp/word-shared-styles`. `scripts/verify_word_shared_styles.py before.docx
after.docx` expands the generated styles and compares the complete effective
document, original style definitions and image bytes. All three paired whole
reports and the main-only report match. Selection, merged-width/superscript and
repeat-cache regression checks also pass, as do current/accumulated five-format
export checks.

Visual verification limitation: the packaged renderer could not run because
LibreOffice is absent. Installed Word and Hancom automation attempts did not
complete and were cancelled; the owned Hancom process was cleaned up, and the
existing user Word process was left untouched. Thus structural fidelity is
verified, but live Office rendering/pagination is not yet visually verified.
`scripts/validate_word_style_render.ps1` is available for a later isolated Word
render comparison; it refuses to run while another Word process exists.

## Repeated-work audit in both document exports

Removed three small redundancies in the shared path:

- Images reuse the HTML tree already parsed for the entry. Diagram replacement
  copies the tree only when needed, without reparsing HTML or mutating the
  shared source. The four-entry fixture now parses HTML four times, not eight.
- Tables and the document model share one extracted paragraph list; headings
  and notes are extracted four times, not eight. Note text used for membership
  checks is also collected once per entry rather than once per table note.
- Native HWPX reads its fixed table font size/padding once per writer invocation
  instead of twice per cell. Table layout still obtains its own shared theme.

`scripts/validate_document_shared_parse.R` checks parse counts for Word/HWPX,
diagram/image bytes and order, orientation, table-note equivalence, and the
immutability of the input tree. The actual browser diagram rasterizer is stubbed
only in the isolated tree-copy test; the supplied fixture retains original PNGs.
`scripts/verify_document_speed_fidelity.py reuse-before reuse-after` verifies
unchanged table/paragraph/section/style XML and image bytes in main/all exports.

One profiled uncached run per format/selection showed no meaningful timing gain:
Word all 9.20 → 9.28 s; HWPX all 2.98 → 2.97 s (main-only Word 6.95 → 7.08 s,
HWPX 2.17 → 2.20 s). These differences are run-to-run noise; do not advertise a
new percentage speedup from this cleanup.

Remaining candidates, not changed here:

- Tables are read as a generic data frame and as a captured merged-cell grid.
  The data-frame headers still participate in legacy main/appendix classification
  and correlation-matrix orientation, so dropping that read requires preserving
  those contracts explicitly.
- Table orientation and following-table lookup recur during heading placement
  and table fitting. These are inexpensive compared with table construction.
- HWPX still creates a flextable solely to obtain geometry before native XML
  writing. In this profile, most HWPX CPU samples are in that construction and
  fitting path, not native package assembly. Removing that intermediate object
  needs equivalent font metrics, wrapping, superscript and merged-cell sizing.
- Word still serializes table properties and finalizes/repackages the document.
  Sharing style IDs reduces the result size but does not eliminate serialization
  of the initial table markup inside the bundled flextable serializer.

## Native HWPX geometry fast path

Simple grids now obtain geometry directly using the same `gdtools` font metrics,
padding and autofit arithmetic as the previous flextable path. Repeated strings
are measured once per table. Merged cells, superscripts, line breaks, tabs and
custom flextable themes retain the existing layout calculation. HWPX model nodes
retain dimensions rather than the intermediate flextable, including on fallback.
Word rendering is unchanged by this optimization.

The supplied four-entry history was written without the repeat-save cache in
three alternating before/after rounds. Median elapsed seconds:

| Selection | Previous | Direct geometry | Reduction |
| --- | ---: | ---: | ---: |
| Main tables | 1.94 | 1.66 | 14.4% |
| All content | 2.71 | 1.98 | 26.9% |

These measure uncached export operations in a loaded R process, not application
startup or file-dialog time. Timings and six output pairs are under
`tmp/hwpx-geometry`; reproduce with `scripts/benchmark_hwpx_geometry.R`.

`scripts/verify_hwpx_geometry.py` confirms every uncompressed ZIP member is
byte-identical for all six before/after pairs, including XML and images.
`scripts/validate_hwpx_geometry.R` checks 35 fixture/edge-case tables (16 direct
layouts, 19 fallbacks), equivalent dimensions and exact HWPX integer units,
multilingual text, custom themes and absence of retained Word table objects.
Current and accumulated HTML/PDF/Word/HWPX/Excel export checks also pass, as do
content-selection, repeat-save cache and shared-parse regression checks.

## Follow-up remaining-cost audit

Profiled the supplied history again after the native geometry optimization using
`benchmark_document_selection_speed.R remaining-audit` and
`summarize_document_remaining_profile.R`. This audit does not change production
export behavior. One uncached all-content run took 9.22 s for Word and 2.19 s for
HWPX. Rprof sampled 6.21 s and 1.63 s respectively; sampled function totals are
nested and must not be added or treated as predicted wall-clock savings.

Priorities identified:

- Word table creation: 1.56 sampled seconds, including 0.59 s in autofit. The
  guarded direct font-metric path could also supply dimensions for simple Word
  grids, but must preserve complete flextable state and custom theme behavior.
- Word XML serialization/insertion: 1.72 sampled seconds, including 1.18 s in
  the serializer. Properties are expanded by flextable before common style IDs
  replace them. A direct Word table writer could avoid this round trip but would
  require explicit merged-cell, superscript, image, border and Office validation.
- Word finalization: 1.56 sampled seconds in print.rdocx. Namespace discovery
  appears repeatedly (0.36 s across the complete export). This partly belongs to
  dependency code; do not bypass relationship/package validation just for speed.
- HWPX complex-table geometry fallback: 0.87 sampled seconds, about 53% of this
  profile. Extending exact geometry to merged/superscript tables remains the
  largest native-export candidate. Native table XML creation sampled only 0.19 s.
- Generic HTML table parsing duplicated alongside captured-cell parsing sampled
  just 0.02 s per format. Default theme application sampled 0.01 s in Word and
  0.03 s in HWPX. These are real repeated work, but low-priority optimizations.

Prefer extending exact geometry before replacing the Word serializer. The above
numbers identify costs, not savings already achieved; no additional speedup is
claimed by this audit.

## Word simple-grid geometry reuse

Word now uses the same exact direct geometry helper as HWPX for eligible plain
grids, assigning measured row heights through flextable's height API and retaining
the existing fixed-width table properties. Column key names are retained as in
autofit. Complex grids and custom themes continue through the previous autofit
path. The helper's `fallback = FALSE` allows Word to reuse its already constructed
table instead of constructing a second one for ineligible grids.

Three alternating uncached before/after rounds on the supplied history:

| Selection | Previous median | Updated median |
| --- | ---: | ---: |
| Main tables | 5.66 s | 5.33 s |
| All content | 8.83 s | 8.81 s |

Main-table median decreased 0.33 s (5.8%), but all-content improvement was within
timing noise. The first before run was slower (6.70 s); do not interpret these
numbers as isolated fresh-process startup timings or promise a universal gain.
Word serialization/finalization remains the larger optimization opportunity.

`validate_word_geometry.R` compares all 35 table objects and their serialized
Word XML, plus a custom italic theme fallback, with the saved previous function.
`verify_word_geometry.py` verifies document/style/numbering/settings/font XML
and embedded image bytes across all six actual export pairs in
`tmp/word-geometry`. Content selection, cache behavior, explicit merged-cell
widths/superscripts and current/accumulated exports in all five formats pass.
This verifies structural fidelity; no new live Office visual rendering is claimed.
