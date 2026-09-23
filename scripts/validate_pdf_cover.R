Sys.setlocale('LC_ALL','Korean_Korea.utf8')
library(shiny)
source('R/result_export_files.R', encoding='UTF-8')
source('R/result_saved_ui.R', encoding='UTF-8')
`%||%` <- function(x,y) if(is.null(x)) y else x
out <- 'tmp/pdfs/cover-check'
dir.create(out, recursive=TRUE, showWarnings=FALSE)
make_table <- function(n, orientation) div(`data-result-table-sheet`='true', `data-result-table-orientation`=orientation,
  tags$table(tags$thead(tags$tr(tags$th('Result'), tags$th('Value'))),
    tags$tbody(lapply(seq_len(n), function(i) tags$tr(tags$td(paste('ROW',i)),tags$td(i/10))))))
content <- tagList(make_table(5,'portrait'), make_table(5,'landscape'), make_table(100,'portrait'))
for (edition in c('development','personal','institution')) {
  Sys.setenv(STATEDU_EDITION=edition, STATEDU_REPORT_USER='홍길동', STATEDU_REPORT_ORGANIZATION='통계교육연구소')
  html <- saved_results_document('분석 결과 보고서',content,report_mode=TRUE)
  doc <- xml2::read_html(html)
  stopifnot(length(xml2::xml_find_all(doc,".//div[@class='report-cover']"))==1L,
    length(xml2::xml_find_all(doc,".//section"))==3L,
    grepl('report-cover',xml2::xml_attr(xml2::xml_find_first(doc,'.//body/*[1]'),'class')))
  if(edition=='development') {
    writeLines(html,file.path(out,'cover.html'),useBytes=TRUE)
    write_pdf_from_html(html,file.path(out,'cover.pdf'))
  }
}
viewer <- xml2::read_html(saved_results_document('Viewer',content))
stopifnot(length(xml2::xml_find_all(viewer,".//div[@class='report-cover']"))==0L)
cat('Cover, editions, content preservation and HTML viewer checks passed.\n')
