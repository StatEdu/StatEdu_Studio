.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules();statedu_apply_preferences()
root <- 'output/common-marker-lookup-20260914'
old <- new.env(parent=.GlobalEnv);sys.source(file.path(root,'baseline.R'),old)
baseline <- old$result_cell_note_marker;environment(baseline) <- .GlobalEnv
current <- result_cell_note_marker
capture <- function(fn, table, row, column) {
  conditions <- character()
  value <- withCallingHandlers(tryCatch(fn(table,row,column),error=function(e)list(error=conditionMessage(e))),
    warning=function(w){conditions<<-c(conditions,conditionMessage(w));invokeRestart('muffleWarning')},
    message=function(m){conditions<<-c(conditions,conditionMessage(m));invokeRestart('muffleMessage')})
  list(value=value,conditions=conditions)
}
table <- data.frame(a=1:5,b=6:10)
set.seed(942);count <- 0L
for (iteration in 1:100) {
  markers <- data.frame(row=sample(c(1:6,NA_integer_),20,TRUE),
    column=sample(c('a','b','missing',NA_character_),20,TRUE),
    marker=sample(c('a','†','',NA_character_),20,TRUE))
  for (variant in 1:4) {
    m <- markers
    if (variant==2) {m <- m[complete.cases(m),];m$marker <- factor(m$marker)}
    if (variant==3) m <- m[complete.cases(m),]
    if (variant==4) class(m) <- c('custom_markers','data.frame')
    attr(table,'note_markers') <- m
    for (row in 1:5) for(column in c('a','b','missing')) {
      stopifnot(identical(capture(baseline,table,row,column),capture(current,table,row,column),num.eq=FALSE))
      count <- count+1L
    }
  }
}
for(m in list(NULL,list(),data.frame(),data.frame(row=1:2,column='a'),
  data.frame(row=c(1,1),column='a',marker=c('first','second')))) {
  attr(table,'note_markers') <- m
  stopifnot(identical(capture(baseline,table,1,'a'),capture(current,table,1,'a'),num.eq=FALSE))
  count <- count+1L
}
attr(table,'note_markers') <- data.frame(row=c(1L,1L,3L),column=c('a','a','b'),marker=c('first','second','†'))
result <- readRDS('output/server-first-analysis-20260913/current-1.rds')$results$prepare_km_analysis_result
for (language in c('ko','en')) {
  outputs <- list()
  for (variant in c('baseline','current')) {
    result_cell_note_marker <- if(variant=='baseline')baseline else current
    panel <- htmltools::renderTags(coefficient_html_table(table,table_language=language))
    html <- saved_survival_results_html(result,language)
    marked_html <- saved_results_document('Marker test',htmltools::HTML(panel$html))
    excel <- file.path(root,paste0(variant,'-',language,'.xlsx'))
    save_screen_excel_file(marked_html,excel)
    sheets <- openxlsx::getSheetNames(excel)
    outputs[[variant]] <- list(panel=panel,html=html,marked_html=marked_html,
      report=saved_survival_results_html(result,language,report_mode=TRUE),
      accumulated=result_entry_tables(list(title='Marker test',html=marked_html),1L),
      sheets=sheets,cells=lapply(sheets,function(s)openxlsx::read.xlsx(excel,sheet=s)))
  }
  stopifnot(identical(outputs$baseline,outputs$current,num.eq=FALSE))
}
result_cell_note_marker <- current
cat('PASS:',count,'marker value/condition cases; bilingual marked-table HTML/Excel/accumulated extraction and complete saved KM HTML/report identical.\n')

