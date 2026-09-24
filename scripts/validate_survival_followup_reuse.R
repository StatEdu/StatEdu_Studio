source('output/survival-followup-reuse-20260913/common.R')
capture <- function(variant,r,language) {
  select_variant(variant);conditions <- character();set.seed(91)
  value <- withCallingHandlers(tryCatch(htmltools::renderTags(survival_reporting_guidance_panel(r,language)),
    error=function(e)list(error=conditionMessage(e))),
    warning=function(w){conditions<<-c(conditions,conditionMessage(w));invokeRestart('muffleWarning')},
    message=function(m){conditions<<-c(conditions,conditionMessage(m));invokeRestart('muffleMessage')})
  list(value=value,conditions=conditions,rng=.Random.seed)
}
results <- list(result)
for(method in c('km','life_table')) for(group in list('', 'sex', c('sex','ph.ecog'))) {
  results[[length(results)+1L]] <- prepare_km_analysis_result(fixture,'time','status',group=group,
    analysis_method=method,plot_types='survival')
}
empty <- result;empty$data <- empty$data[FALSE,];results[[length(results)+1L]] <- empty
entry <- result;entry$data$entry <- entry$data[[entry$time]]/2;entry$entry <- 'entry';results[[length(results)+1L]] <- entry
subjects <- result;subjects$subject_id <- 'id';results[[length(results)+1L]] <- subjects
count <- 0L
for(r in results)for(language in c('ko','en')) {
  stopifnot(identical(capture('baseline',r,language),capture('current',r,language),num.eq=FALSE));count <- count+1L
}
original <- survival_followup_diagnostics
for(kind in c('quiet','warning','message')) {
  calls <- 0L
  survival_followup_diagnostics <- function(r) {
    calls <<- calls+1L
    if(kind=='warning')warning('follow-up warning')
    if(kind=='message')message('follow-up message')
    original(r)
  }
  before <- capture('baseline',result,'en');old_calls <- calls;calls <- 0L
  after <- capture('current',result,'en')
  stopifnot(identical(before,after,num.eq=FALSE),old_calls==2L,
    calls==if(kind=='quiet')1L else 2L)
}
survival_followup_diagnostics <- original
for(language in c('ko','en')) {
  outputs <- list()
  for(variant in names(variants)) {
    select_variant(variant)
    html <- saved_survival_results_html(result,language)
    path <- file.path(root,paste0(variant,'-',language,'.xlsx'));save_screen_excel_file(html,path)
    sheets <- openxlsx::getSheetNames(path)
    outputs[[variant]] <- list(html=html,report=saved_survival_results_html(result,language,report_mode=TRUE),
      screen=htmltools::renderTags(survival_km_results_panel(result,language=language)),
      accumulated=result_entry_tables(list(title='KM',html=html),1L),
      sheets=sheets,cells=lapply(sheets,function(s)openxlsx::read.xlsx(path,sheet=s)))
  }
  stopifnot(identical(outputs$baseline,outputs$current,num.eq=FALSE))
}
select_variant('current')
cat('PASS:',count,'guidance HTML/condition/RNG comparisons, call-count/diagnostic checks, bilingual complete screen/saved/report HTML and accumulated/Excel content.\n')
