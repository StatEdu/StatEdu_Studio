source("scripts/validate_regression_publication_style.R", encoding="UTF-8")
entries <- c(entries, list(list(id="hierarchical", title="Hierarchical regression", html=hierarchical)))
normalize <- function(x) gsub("[[:space:]\u00a0]+", "", paste(x, collapse=""), perl=TRUE)
for (mode in c("current", "accumulated")) {
  selected <- if(mode=="current") list(list(id="current",title="Current results",html=paste(vapply(entries, `[[`, character(1), "html"),collapse="\n"))) else entries
  stem <- file.path(out,mode)
  write_result_collection_html(selected,paste0(stem,".html"))
  write_result_collection_docx(selected,paste0(stem,".docx"))
  save_result_collection_excel_file(selected,paste0(stem,".xlsx"))
  write_result_collection_pdf(selected,paste0(stem,".pdf"))
  write_result_collection_hwpx(selected,paste0(stem,".hwpx"))
  expected <- unlist(lapply(selected,function(entry) {
    doc <- xml2::read_html(entry$html)
    xml2::xml_text(xml2::xml_find_all(doc,"//th|//td|//h3|//h4|//*[contains(@class,'coefficient-note')]"))
  }))
  expected <- expected[nzchar(trimws(expected))]
  word <- xml2::read_xml(unz(paste0(stem,".docx"),"word/document.xml"))
  word_text <- normalize(xml2::xml_text(word))
  members <- unzip(paste0(stem,".hwpx"),list=TRUE)$Name
  hwpx_text <- normalize(vapply(members[grepl("Contents/section[0-9]+[.]xml$",members)],function(s) xml2::xml_text(xml2::read_xml(unz(paste0(stem,".hwpx"),s))), character(1)))
  wb <- openxlsx::loadWorkbook(paste0(stem,".xlsx"))
  excel_text <- normalize(unlist(lapply(seq_along(names(wb)),function(i) as.matrix(openxlsx::read.xlsx(wb,sheet=i,colNames=FALSE)))))
  for(value in expected) for(actual in list(word_text,hwpx_text,excel_text)) stopifnot(grepl(normalize(value),actual,fixed=TRUE))
  grids <- xml2::xml_find_all(word,".//w:tblGrid",xml2::xml_ns(word))
  stopifnot(length(grids)==length(names(wb)))
  conditional_widths <- as.numeric(xml2::xml_attr(xml2::xml_children(grids[[3]]),"w"))
  stopifnot(abs(conditional_widths[1]/sum(conditional_widths)-.28)<.005,abs(conditional_widths[2]/sum(conditional_widths)-.16)<.005)
  stopifnot(any(xml2::xml_attr(xml2::xml_find_all(word,".//w:pgSz",xml2::xml_ns(word)),"orient")=="landscape",na.rm=TRUE))
  jsonlite::write_json(expected,paste0(stem,"-expected.json"),auto_unbox=FALSE)
  message("PASS: ",mode," five exports; all table cells, headings/subtitles, notes, conditional widths and landscape section")
}
