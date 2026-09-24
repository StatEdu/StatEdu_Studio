.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules();statedu_apply_preferences()
root <- 'output/survival-table-style-20260913'
old <- new.env(parent=.GlobalEnv);sys.source(file.path(root,'baseline.R'),old)
baseline <- old$survival_simple_table;environment(baseline) <- .GlobalEnv
current <- survival_simple_table
capture <- function(fn, table, language, role) {
  conditions <- character()
  set.seed(91)
  value <- withCallingHandlers(tryCatch(htmltools::renderTags(fn(table,
    table_language=language,table_role=role,note_line='CI = confidence interval.')),
    error=function(e)list(error=conditionMessage(e))),
    warning=function(w){conditions<<-c(conditions,conditionMessage(w));invokeRestart('muffleWarning')},
    message=function(m){conditions<<-c(conditions,conditionMessage(m));invokeRestart('muffleMessage')})
  list(value=value,conditions=conditions,rng=.Random.seed)
}
count <- 0L
for (n in c(0L,1L,2L,5L,50L)) for (language in c('ko','en')) for (role in c('main','appendix')) {
  table <- data.frame(Group=rep(c('A','<B>&'),length.out=n),
    Value=rep(c(NA_real_,0,-1.234,Inf,NaN),length.out=n),
    p=rep(c('.001','< .001'),length.out=n),check.names=FALSE)
  names(table) <- c('집단 / Group','Median (95% CI)','Chi-square (df)')
  for (markers in list(NULL,data.frame(row=c(1L,1L,2L),
       column=rep(names(table)[2],3),marker=c('a','b','†')))) {
    attr(table,'note_markers') <- markers
    stopifnot(identical(capture(baseline,table,language,role),
                       capture(current,table,language,role),num.eq=FALSE))
    count <- count+1L
  }
}
for (table in list(NULL,list(),data.frame(row.names=1:2),
  setNames(data.frame(a=1:2,b=3:4),c('same','same')))) {
  stopifnot(identical(capture(baseline,table,'en','main'),capture(current,table,'en','main'),num.eq=FALSE))
  count <- count+1L
}
# Conditions must still be repeated when class formatting was not quiet.
original_class <- survival_column_class
survival_column_class <- function(name) {warning('class warning');original_class(name)}
stopifnot(identical(capture(baseline,data.frame(a=1:3),'en','main'),
                   capture(current,data.frame(a=1:3),'en','main'),num.eq=FALSE))
survival_column_class <- original_class
result <- readRDS('output/server-first-analysis-20260913/current-1.rds')$results$prepare_km_analysis_result
for (language in c('ko','en')) {
  outputs <- list()
  for (variant in c('baseline','current')) {
    survival_simple_table <- if (variant=='baseline') baseline else current
    html <- saved_survival_results_html(result,language)
    report <- saved_survival_results_html(result,language,report_mode=TRUE)
    excel <- file.path(root,paste0(variant,'-',language,'.xlsx'))
    save_screen_excel_file(html,excel)
    sheets <- openxlsx::getSheetNames(excel)
    outputs[[variant]] <- list(screen=htmltools::renderTags(survival_km_results_panel(result,language=language)),
      html=html,report=report,tables=result_entry_tables(list(title='KM',html=html),1L),
      sheets=sheets,cells=lapply(sheets,function(s)openxlsx::read.xlsx(excel,sheet=s)))
  }
  stopifnot(identical(outputs$baseline,outputs$current,num.eq=FALSE))
}
survival_simple_table <- current
cat('PASS:',count,'table cases plus diagnostic fallback; bilingual full KM screen, saved/report HTML, accumulated table extraction and Excel cells identical.\n')
