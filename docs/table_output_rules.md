# **StatEdu Studio** Table Output Rules

These rules apply to every analysis result table in **StatEdu Studio**.

## Language and Publication Roles

- Main result tables are publication-ready SCI journal tables and are always written in English, regardless of the UI language.
- Appendix, diagnostic, sensitivity, and input-review tables follow the current UI language.
- Analysis menus, setup blocks, options, and appendix headings follow the UI language. Main-table titles, headers, system-generated cells, and notes remain English, including when the UI is Korean.
- User-defined variable names, variable labels, and category/value labels retain their original text in both main and appendix tables. A label matching an application phrase (for example `Normality`, `Yes`, or `Status`) must not be translated. Mark nonstandard user-data columns/headers with `result_user_columns` / `result_user_headers` when the shared table helper cannot infer their role.
- Publication figures, including forest plots and funnel plots, are always written in English.
- Every new analysis method must classify each new table as either `main` or `appendix` and apply the corresponding language rule through the shared result-table contract.
- Bootstrap confidence intervals use a grouped `95% CI` header above separate `LLCI` and `ULCI` columns. Other headers span both rows; existing model/effect groups remain above this pair. Apply the same geometry to screen, HTML, PDF, Word, HWPX, and Excel, including accumulated results. Preserve values, precision, footnote markers, and interval method. Define `95% CI = 95% confidence interval` before LLCI/ULCI in notes; hierarchical regression retains its single shared note after the final model.

## Current Export Rules

- The public release exposes HTML result saving, 300 dpi figure-file saving, and the in-app Result collection.
- PDF, Excel, and Word result saving are planned for the general public 1.3.0 release, independently of Pro. They remain disabled in public 1.2.x and enabled in non-public development builds for implementation and validation.
- Pro is tentatively planned around 1.5.0; its scope and release timing are not final. See [release roadmap](RELEASE_ROADMAP_KO.md).
- Free-edition PDF covers display `FREE`, the StatEdu logo, and `Prepared with / StatEdu, Institute of Statistics`; user and institution metadata are omitted. Development covers use `DEVELOPMENT` / `Development owner`.

- Every analysis with computed results should show `Save tables` and `Save figures` in the same action area.
- `Save tables` exports Excel workbooks (`.xlsx`).
- Excel table export must preserve the displayed analysis table structure. If the result table has a two-level header, the Excel sheet must also use a two-level header with the same merged header groups.
- Excel table borders follow the displayed table rule: solid line at the table top, solid line at the table bottom, solid line below the final header row, and transparent/no vertical borders unless a result table explicitly displays them.
- Excel cell `A1` contains the table title and is merged across the used table width.
- A long title in `A1` must not determine the width of column A. Column widths are fixed from the result table layout, not from title text length.
- Excel notes and footnotes are written in a row merged across the exact table width and wrap inside that merged width.
- PDF report export uses A4 paper.
- Word report export uses B5 paper.
- PDF and Word tables must fit inside the printable page width with a small left and right safety margin so no table content is clipped.
- PDF and Word exports should preserve the displayed result-table structure, ordering, and alignment rules from the analysis result screen.
- Analysis tables should be exported independently. When an analysis has multiple tables, each table keeps its own portrait or landscape decision instead of forcing the whole report into one orientation.
- Cross-tabulation primary tables use a width-based PDF orientation rule: compact tables remain portrait, while wide primary tables print landscape. Supporting tables such as expected counts remain portrait unless explicitly given their own landscape rule.
- Each exported table is written to its own worksheet.
- For Excel exports, cell `A1` contains the table title and is merged across the used table width.
- The exported table starts on row 3.
- Exported tables use thin solid lines at the table top, header bottom, and table bottom.
- Figure exports use a folder picker and save separate PNG files.
- Public figure PNG files are saved at 300 dpi. Development and Pro editions support 600 dpi output.
- New analysis modules should call the shared export helpers in `R/result_export.R` and `R/result_export_files.R`.

## Table Lines

- Draw a solid line at the top of each table.
- Draw a solid line below the header.
- Draw a solid line at the bottom of each table.

## Table Notes

- Regression's note convention applies to every analysis and every table. Omit the redundant `Note.` / `주.` prefix.
- Put definitions before estimation methods, reference coding, adjustments, and explanations. Use the shared order M/SD, SE (including robust/bootstrap variants), 95% CI, LLCI, ULCI, Tol, VIF, d, z(p), χ²(p), f², sr²; retain the relative order of other definitions and explanatory clauses.
- Explain only statistics displayed in the relevant table. Optional columns and their definitions must appear/disappear together. Preserve numerical footnote markers, HTML superscripts, symbols, and all substantive explanations.
- Hierarchical regression retains its existing exception: shared definitions appear once after the final model for each outcome and cover earlier models in that group. Model/method labels remain with their models.
- Use `result_note_tag`, `result_note_paragraph`, `result_note_div`, or a table helper's `note_line`; do not bypass the shared normalizer with raw note markup.
- The captured screen is authoritative for all five exports. Apply [RESULT_EXPORT_CONTRACT.md](RESULT_EXPORT_CONTRACT.md) if older release or cover guidance above conflicts: no newly invented cover or rewritten note, and retain the displayed per-table orientation.

- Analysis result notes and footnotes must use the same visual width as the table they describe.
- Notes must never extend beyond the table width and must never be narrower than the table width.
- Long note text must wrap inside the table width.
- New HTML table output should place notes in the shared table-note wrapper, for example with `result_table_with_notes()` or `coefficient_html_table(note_line = ...)`.

## Labels

- Standard regression and per-model hierarchical regression reserve 28–42% of table width for Variable, depending on the number of displayed statistics (42% for the six-column bootstrap table). Statistics share the remaining width, with extra allowance for SE and `reference`. Preserve these column proportions in every export.

- Variable names: show `var_label` when it exists; otherwise show the variable name.
- Value names: show `value_label` when it exists; otherwise show the raw value.
- Do not show `variable(label)` or `value(label)` in result tables unless explicitly requested for a specific feature.

## Numeric Formatting

- Integers: show as integers.
- Percentages: show one decimal place.
- Percentages below 10.0 in compact `n(%)` output: pad one leading space, for example `37( 9.2)`.
- Mean, standard deviation, minimum, maximum, median, and IQR: show two decimal places.
- IQR: show as `IQR(Q1~Q3)`, for example `1.50 (1.75~3.25)`.
- Skewness and kurtosis: show three decimal places.
- p-values: show three decimal places, omit the leading zero, for example `.027`.
- p-values below .001: show `<.001`.

PDF cover labels, standard titles, descriptions and edition names follow the current UI language (ko/en/ja/zh/es/fr/de/vi). Preserve logos, the full name `StatEdu, Institute of Statistics`, and custom titles/footers.

- Regression and mediation/moderation notes omit the `Note.` prefix and define displayed statistics in this order: SE/HC3 SE/Boot SE, 95% CI, LLCI, ULCI, Tol, VIF, d, z(p), chi-square(p), f-squared. Additional method-specific explanations follow.
- Hierarchical regression shows shared notes only after the final model table for each outcome, including definitions needed by preceding models. Model/method labels remain visible; predictor formulas are omitted from model notes.

- Optional statistics and their note definitions must appear together: unchecked sr-squared, f-squared, or VIF options omit both the columns and definitions. When f-squared alone is selected, its note must not introduce sr-squared.

- Regression-family coefficient tables use the mediation/moderation publication appearance (shared font, text color, padding, header and horizontal rules). Standard hierarchical results use the same model-table renderer as mediation/moderation, with outcome and estimation method retained below each model title.
- Conditional effects allocate space to Path and Moderator and wrap long labels inside their own cells. Do not add nowrap markup to Path. Preserve the explicit column proportions and landscape orientation in every export.
