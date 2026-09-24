if (.Platform$OS.type == 'windows') Sys.setlocale('LC_CTYPE','Korean_Korea.utf8')
Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
baseline <- commandArgs(TRUE)[1]
if(is.na(baseline))stop('Provide the pre-change result_saved_ui.R snapshot path.')
old <- new.env(parent=globalenv())
expressions <- parse(baseline,encoding='UTF-8')
for(expr in expressions) if(is.call(expr) && identical(expr[[1]],as.name('<-')) &&
  identical(expr[[2]],as.name('result_document_table'))) eval(expr,old)
stopifnot(is.function(old$result_document_table))
entries <- read_result_snapshot_store('sample/StatEdu_Studio_result_history_20260917_154949.efs-result')
entries <- c(entries,list(list(title='Mixed styles',html=paste0(
  '<table><thead><tr><th rowspan="2" style="text-align:left;vertical-align:top">Label</th><th colspan="2">95% CI</th></tr>',
  '<tr><th style="text-align:right">Lower</th><th>Upper</th></tr></thead><tbody>',
  '<tr><td rowspan="2" style="vertical-align:top">한글 Label<br>second line</td><td style="text-align:right">1.234<sup>a</sup></td><td style="text-align:center">2.5</td></tr>',
  '<tr><td colspan="2" style="text-align:center">Merged explanation</td></tr>',
  '<tr><td>F (2, 30)</td><td>3.45</td><td style="text-align:right;vertical-align:top">.012</td></tr>',
  '</tbody></table><table><tr><td style="text-align:right">No header</td><td>4.5</td></tr></table>'))))
count <- 0L
for(i in seq_along(entries)) for(info in result_entry_tables(entries[[i]],i,include_docx=FALSE)) {
  for(layout_only in c(FALSE,TRUE)) {
    before <- old$result_document_table(info,layout_only)
    after <- result_document_table(info,layout_only)
    for(part in c('header','body','footer')) for(field in c('styles','spans','colwidths','rowheights','hrule'))
      stopifnot(isTRUE(all.equal(before[[part]][[field]],after[[part]][[field]])))
  }
  count <- count+1L
}
cat('PASS',count,'tables: all cell styles, spans, column widths and row heights match for Word and HWPX layout\n')
