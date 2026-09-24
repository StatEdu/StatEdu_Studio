if (.Platform$OS.type == 'windows') Sys.setlocale('LC_CTYPE','Korean_Korea.utf8')
Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
entries <- read_result_snapshot_store('sample/StatEdu_Studio_result_history_20260917_154949.efs-result')
cases <- unlist(lapply(seq_along(entries),function(i)result_entry_tables(entries[[i]],i,include_docx=FALSE)),recursive=FALSE)
fixture <- function(html) result_entry_tables(list(title='Geometry',html=html),include_docx=FALSE)[[1]]
cases <- c(cases,list(
  fixture('<table><tr><th>한글</th><th>EN</th></tr><tr><td>日本語 中文</td><td>−1.234 ± 0.56</td></tr><tr><td></td><td>العربية ภาษาไทย</td></tr></table>'),
  fixture('<table><tr><td>No header</td><td>0.123</td></tr></table>'),
  fixture('<table><thead><tr><th rowspan="2">Label</th><th colspan="2">95% CI</th></tr><tr><th>Lower</th><th>Upper</th></tr></thead><tbody><tr><td>X</td><td>1.23<sup>a</sup></td><td>5.67</td></tr></tbody></table>')))
# Raw captured line breaks/tabs, unlike HTML whitespace, exercise fallback.
for(value in c('line one\nline two','tab\tvalue','<br>','<tab>')) {
  item <- cases[[length(cases)-1L]]
  item$screen$values[1,1] <- value
  cases[[length(cases)+1L]] <- item
}
plain <- cases[[26L]]
for(widths in list(c(0,80),c(NA,80),c(30,70))) {
  item<-plain;item$screen$column_widths<-widths;cases[[length(cases)+1L]]<-item
}
original <- result_document_table
calls <- 0L
result_document_table <- function(...) {calls<<-calls+1L;original(...)}
fast <- fallback <- 0L
for(i in seq_along(cases)) {
  info <- cases[[i]]
  reference <- original(info,layout_only=TRUE)
  prior <- calls
  actual <- result_document_table_geometry(info)
  if(calls==prior) fast<-fast+1L else fallback<-fallback+1L
  expected <- list(widths=unname(reference$body$colwidths),heights=unname(c(reference$header$rowheights,reference$body$rowheights)))
  if(!isTRUE(all.equal(expected,actual,tolerance=1e-12))) {print(list(case=i,title=info$title,expected=expected,actual=actual));stop('Geometry mismatch')}
  stopifnot(isTRUE(all.equal(expected,actual,tolerance=1e-12)),
    identical(result_hwpx_units(expected$widths),result_hwpx_units(actual$widths)),
    identical(result_hwpx_units(expected$heights),result_hwpx_units(actual$heights)))
}
stopifnot(fast>0L,fallback>0L)
cat('PASS',length(cases),'tables:',fast,'direct metric layouts;',fallback,'complex fallbacks; identical dimensions and HWPX units\n')
saved_defaults <- flextable::get_flextable_defaults()
flextable::set_flextable_defaults(theme_fun=function(x)x)
prior <- calls
invisible(result_document_table_geometry(plain))
stopifnot(calls==prior+1L)
do.call(flextable::set_flextable_defaults,saved_defaults)
model <- result_document_model(entries,layout_only=TRUE)
stopifnot(all(vapply(Filter(function(n)n$kind=='table',model$nodes),function(n)is.null(n$table),logical(1))))
result_document_cleanup(model)
cat('PASS custom themes retain legacy layout; native model retains no Word table objects\n')
