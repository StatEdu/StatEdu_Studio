Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
dir.create('tmp/survival-settings-audit',recursive=TRUE,showWarnings=FALSE)
required<-names(formals(create_current_settings_fn))[vapply(formals(create_current_settings_fn),function(x)identical(x,quote(expr=)),logical(1))]
inputs<-list(survival_km_data_shape='entry_exit',survival_km_rmst_tau='365',survival_cox_ties_method='exact',survival_cox_spline_df=5L,survival_competing_regression='both',survival_competing_event_values='2, 3')
report<-list()
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 args<-setNames(lapply(required,function(n)function(...)NULL),required)
 args$app_version<-'audit';args$input<-inputs;args$app_language_fn<-function()lang
 args$current_data_step_fn<-function()'load_data';args$active_step_fn<-function()'step1';args$data_view_fn<-function()'info'
 args$restored_data_file_fn<-function()''
 args$measurement_overrides<-function(...)character(0)
 args$var_label_overrides<-function(...)c(Review='사용자 <&> %s')
 collect<-do.call(create_current_settings_fn,args)
 settings<-collect()
 path<-paste0('tmp/survival-settings-audit/',lang,'.studio')
 write_settings_json_file(settings,path);restored<-read_settings_json_file(path)
 stopifnot(identical(restored$app_language,lang))
 text<-paste(readLines(path,encoding='UTF-8'),collapse='\n')
 missing<-names(inputs)[!vapply(names(inputs),function(k)grepl(k,text,fixed=TRUE),logical(1))]
 report[[lang]]<-data.frame(language=lang,field=names(inputs),saved=names(inputs)%in%setdiff(names(inputs),missing))
 cat('AUDIT:',lang,'language roundtrip PASS; survival option fields absent:',paste(missing,collapse=', '),'\n')
}
write.csv(do.call(rbind,report),'tmp/survival-settings-audit/coverage.csv',row.names=FALSE,fileEncoding='UTF-8')
