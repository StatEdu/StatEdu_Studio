# EasyFlow Statistics Table Output Rules

These rules apply to every analysis result table in EasyFlow Statistics.

## Export Rules

- Every analysis with computed results should show `Save tables` and `Save figures` in the same action area.
- `Save tables` exports Excel workbooks (`.xlsx`).
- Each exported table is written to its own worksheet.
- Cell `A1` contains the table title and is merged across the used table width.
- The exported table starts on row 3.
- Exported tables use thin solid lines at the table top, header bottom, and table bottom.
- Figure exports use a folder picker and save separate PNG files.
- Figure PNG files are saved at 600 dpi by default.
- New analysis modules should call the shared export helpers in `R/result_export.R` and `R/result_export_files.R`.

## Table Lines

- Draw a solid line at the top of each table.
- Draw a solid line below the header.
- Draw a solid line at the bottom of each table.

## Table Notes

- Analysis result notes and footnotes must use the same visual width as the table they describe.
- Notes must never extend beyond the table width and must never be narrower than the table width.
- Long note text must wrap inside the table width.
- New HTML table output should place notes in the shared table-note wrapper, for example with `result_table_with_notes()` or `coefficient_html_table(note_line = ...)`.

## Labels

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
