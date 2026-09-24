source('output/table-render-combined-20260914/common.R')
set.seed(932)
plain <- as.data.frame(setNames(rep(list(seq_len(200)),6),LETTERS[1:6]))
annotated <- plain
attr(annotated,'spanning_cells') <- data.frame(row=1:200,start_column='B',end_column='D',value='Merged value')
attr(annotated,'note_markers') <- data.frame(row=1:200,column='E',marker='a')
attr(annotated,'cell_styles') <- data.frame(row=1:200,column='E',style='color:#123456;')
correlations <- list()
for(n in c(30L,50L)) {
  d <- as.data.frame(matrix(rnorm(150*n),150,n));names(d)<-paste0('V',1:n)
  correlations[[as.character(n)]] <- prepare_correlation_results(d,names(d),options=list(continuous_method='pearson',p_ci=TRUE))
}
km <- readRDS('output/server-first-analysis-20260913/current-common-marker-verified.rds')$results$prepare_km_analysis_result
renderers <- list(plain_200=function()coefficient_html_table(plain),
  annotated_200=function()coefficient_html_table(annotated),
  correlation_30=function()correlation_results_ui(correlations[['30']]),
  correlation_50=function()correlation_results_ui(correlations[['50']]),
  km_fixture=function()survival_km_results_panel(km,language='ko'))
capture <- function(v,fn) {
  select_variant(v);conditions<-character();set.seed(933)
  value <- withCallingHandlers(htmltools::renderTags(fn()),
    warning=function(w){conditions<<-c(conditions,conditionMessage(w));invokeRestart('muffleWarning')},
    message=function(m){conditions<<-c(conditions,conditionMessage(m));invokeRestart('muffleMessage')})
  list(value=value,conditions=conditions,rng=.Random.seed)
}
timings <- list()
for(name in names(renderers)) {
  fn<-renderers[[name]]
  stopifnot(identical(capture('baseline',fn),capture('current',fn),num.eq=FALSE))
  for(round in 1:3)for(v in if(round %% 2L)c('baseline','current')else c('current','baseline')) {
    select_variant(v)
    elapsed<-system.time(htmltools::renderTags(fn()))[['elapsed']]
    timings[[length(timings)+1L]]<-data.frame(workload=name,round=round,variant=v,elapsed=elapsed)
  }
  cat('Verified and measured:',name,'\n')
}
timings<-do.call(rbind,timings);write.csv(timings,file.path(root,'timings.csv'),row.names=FALSE)
summary<-aggregate(elapsed~workload+variant,timings,median);print(summary)
write.csv(summary,file.path(root,'summary.csv'),row.names=FALSE)
# Check document inputs and accumulated table extraction with all five helpers switched together.
for(name in c('annotated_200','correlation_30','km_fixture')) {
  outputs<-list()
  for(v in names(variants)) {
    select_variant(v)
    html <- switch(name,
      annotated_200=saved_results_document('Annotated table',coefficient_html_table(annotated)),
      correlation_30={path<-file.path(root,paste0(v,'-correlation.html'));write_correlation_results_html(correlations[['30']],path);paste(readLines(path,warn=FALSE,encoding='UTF-8'),collapse='\n')},
      km_fixture=saved_survival_results_html(km,'ko'))
    path<-file.path(root,paste0(v,'-',name,'.xlsx'));save_screen_excel_file(html,path)
    sheets<-openxlsx::getSheetNames(path)
    outputs[[v]]<-list(html=html,tables=result_entry_tables(list(title=name,html=html),1L),
      sheets=sheets,cells=lapply(sheets,function(s)openxlsx::read.xlsx(path,sheet=s)))
  }
  stopifnot(identical(outputs$baseline,outputs$current,num.eq=FALSE))
  cat('Verified saved/accumulated/Excel:',name,'\n')
}
select_variant('current')
cat('PASS: all combined helper-set comparisons.\n')
