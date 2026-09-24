Sys.setlocale("LC_CTYPE", "English_United States.utf8")
out <- Sys.getenv("STATEDU_I18N_EXPORT_FIXTURE", "")
if (nzchar(out)) {
  Sys.setenv(STATEDU_MODULE_CACHE = "false")
  source("R/app_bootstrap.R", encoding = "UTF-8")
  load_app_packages(check = FALSE); source_app_modules()
  options(statedu.app_language = "ja")
} else {
  source("scripts/validate_multilingual_rendering.R", encoding = "UTF-8")
}
entries <- readRDS(file.path(out, "entries.rds"))
normalize <- function(x) gsub("[[:space:]\u00a0]+", "", paste(x, collapse = ""), perl = TRUE)
for (mode in c("current", "accumulated")) {
 selected <- if (mode == "current") entries else c(entries, list(list(id="ja-second",title="追加した結果",html=entries[[1]]$html)))
 stem <- file.path(out, paste0("ja-", mode))
 write_result_collection_html(selected, paste0(stem,".html"))
 write_result_collection_docx(selected, paste0(stem,".docx"))
 save_result_collection_excel_file(selected, paste0(stem,".xlsx"))
 write_result_collection_pdf(selected, paste0(stem,".pdf"))
 write_result_collection_hwpx(selected, paste0(stem,".hwpx"))
 expected <- xml2::xml_text(xml2::xml_find_all(xml2::read_html(entries[[1]]$html),"//th|//td|//h3|//h4|//h5"))
 word <- normalize(xml2::xml_text(xml2::read_xml(unz(paste0(stem,".docx"),"word/document.xml"))))
 members <- unzip(paste0(stem,".hwpx"),list=TRUE)$Name
 hwpx <- normalize(vapply(members[grepl("Contents/section[0-9]+[.]xml$",members)],function(s) xml2::xml_text(xml2::read_xml(unz(paste0(stem,".hwpx"),s))),character(1)))
 workbook <- openxlsx::loadWorkbook(paste0(stem,".xlsx"))
 excel <- normalize(unlist(lapply(seq_along(names(workbook)),function(i) as.matrix(openxlsx::read.xlsx(workbook,sheet=i,colNames=FALSE)))))
 html <- normalize(xml2::xml_text(xml2::read_html(paste0(stem,".html"))))
 for (value in expected[nzchar(trimws(expected))]) for (actual in list(word,hwpx,excel,html)) stopifnot(grepl(normalize(value),actual,fixed=TRUE))
 jsonlite::write_json(expected,paste0(stem,"-expected.json"),auto_unbox=FALSE)
 cat("PASS:",mode,"HTML/Word/HWPX/Excel content and PDF generated\n")
}
