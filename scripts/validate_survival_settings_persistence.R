Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
dir.create('tmp/survival-settings-persistence',recursive=TRUE,showWarnings=FALSE)
fixture<-survival_settings_defaults()
fixture$km$time<-'time';fixture$km$event<-'event';fixture$km$group<-c('Review','None');fixture$km$entry<-'entry';fixture$km$data_shape<-'entry_exit';fixture$km$show_ci<-FALSE;fixture$km$output_tables<-character(0);fixture$km$rate_times<-'사용자 <&> %s';fixture$km$rmst_tau<-'365'
fixture$cox$time<-'time';fixture$cox$event<-'event';fixture$cox$covariates<-c('Review','None');fixture$cox$data_shape<-'start_stop';fixture$cox$start<-'entry';fixture$cox$stop<-'time';fixture$cox$subject_id<-'id';fixture$cox$ties_method<-'exact';fixture$cox$spline_df<-5L;fixture$cox$adjusted_bootstrap_reps<-400L
fixture$competing$time<-'time';fixture$competing$event<-'event';fixture$competing$regression<-'both';fixture$competing$censoring_group<-'Review';fixture$competing$event_values<-'2, 3'
dataset<-data.frame(time=2:5,event=c(0,1,2,1),entry=0:3,id=1:4,Review=1:4,None=4:1)
for(language in c('en','ko','ja','zh','es','fr','de','vi')) {
 required<-names(formals(create_current_settings_fn))[vapply(formals(create_current_settings_fn),function(x)identical(x,quote(expr=)),logical(1))]
 args<-setNames(lapply(required,function(n)function(...)NULL),required)
 args$app_version<-'test';args$input<-list();args$app_language_fn<-function()language
 args$current_data_step_fn<-function()'load_data';args$active_step_fn<-function()'step1';args$data_view_fn<-function()'info';args$restored_data_file_fn<-function()''
 args$measurement_overrides<-function(...)character(0);args$var_label_overrides<-function(...)character(0)
 args$survival_settings_fn<-function()fixture
 collected<-do.call(create_current_settings_fn,args)()
 stopifnot(identical(collected$survival,fixture))
 path<-paste0('tmp/survival-settings-persistence/',language,'.studio')
 write_settings_json_file(collected,path)
 loaded<-read_settings_json_file(path)
 stopifnot(identical(normalize_survival_settings(loaded$survival),fixture))
 server<-function(input,output,session) {
  request<-reactiveVal(list(revision=1L,settings=loaded$survival))
  api<-register_survival_handlers(input,output,session,function()names(dataset),function()dataset,function()NULL,function()character(0),function()data.frame(),function()NULL,app_language_fn=function()language,restore_request_fn=request)
 }
 shiny::testServer(server,{
  session$flushReact()
  stopifnot(identical(isolate(api$settings()),fixture))
  # Edit after restoration; no action, plot or result object is serialized.
  session$setInputs(survival_competing_regression='none',survival_km_show_ci=TRUE,survival_cox_spline_df=3L)
  updated<-isolate(api$settings());stopifnot(updated$competing$regression=='none',updated$km$show_ci,updated$cox$spline_df==3L)
  request(list(revision=2L,settings=loaded$survival));session$flushReact()
  stopifnot(identical(isolate(api$settings()),fixture))
  request(list(revision=3L,settings=NULL));session$flushReact()
  stopifnot(identical(isolate(api$settings()),survival_settings_defaults()))
 })
 cat('PASS:',language,'file roundtrip; cold/hot module restore; edits after restore; old-file defaults; empty vectors and labels\n')
}
stopifnot(identical(normalize_survival_settings(list(km='invalid')),survival_settings_defaults()))
