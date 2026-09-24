for (format in c("word", "hwpx")) {
  profile <- summaryRprof(paste0("tmp/document-speed/remaining-audit-all-", format, ".prof"))
  cat("\n", format, "sampled seconds:", profile$sampling.time, "\n")
  print(head(profile$by.total, 40L))
  selected <- grepl("html_table|result_entry_tables|result_html_table_cells|theme_booktabs|autofit|serializer|wml_rows|print.rdocx|xml_ns|result_document_table|result_docx_shared|table_writer", rownames(profile$by.total))
  print(profile$by.total[selected, , drop = FALSE])
}
