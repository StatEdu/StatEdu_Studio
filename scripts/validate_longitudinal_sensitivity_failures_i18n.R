Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
tables<-list();missing<-character()
messages<-c(known='Weights must be positive finite values.',external='Warning\nFailed\n사용자.x: 1.2300e-09 (50%)')
for(kind in names(messages))for(model in c('gee','lmm','panel_fe')) {
 message<-messages[[kind]]
 env<-new.env(parent=environment(longitudinal_sensitivity_analysis_results))
 env$longitudinal_fit_model<-function(...)stop(message,call.=FALSE)
 fn<-longitudinal_sensitivity_analysis_results;environment(fn)<-env
 tab<-fn(data.frame(),'y','id','time','x',model,'gaussian','exchangeable',hausman_provider=function()stop(message,call.=FALSE))
 stopifnot(nrow(tab)>0,all(tab$Status=='Failed'));before<-serialize(tab,NULL)
 for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
  translated<-longitudinal_appendix_table(tab,lang)
  expected<-if(kind=='external')message else longitudinal_input_error_text(simpleError(message),lang)
  stopifnot(all(translated[[match('Note',names(tab))]]==expected),identical(before,serialize(tab,NULL)))
  if(lang!='en')stopifnot(all(translated[[match('Status',names(tab))]]!='Failed'))
 }
 tables[[paste(kind,model)]]<-tab
}
metrics<-c('QIC','AIC / BIC','Robust SE','Group-clustered HC1 SE','SE ratio vs HC1','p-value')
tab<-data.frame(Metric=metrics,Value=c('1.2300','AIC=1.230; BIC=2.340','','Available','1.250','<.001'))
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 translated<-longitudinal_appendix_table(tab,lang)
 stopifnot(identical(translated[[1]][1:2],metrics[1:2]),identical(translated[[2]][-4],tab$Value[-4]))
 if(lang!='en')for(i in 3:6)if(translated[[1]][i]==metrics[i])missing<-c(missing,paste(lang,metrics[i]))
}
if(length(missing))stop(paste(missing,collapse='\n'))
tables$metrics<-tab
dir.create('tmp/longitudinal-sensitivity-failures',recursive=TRUE,showWarnings=FALSE)
saveRDS(tables,'tmp/longitudinal-sensitivity-failures/tables.rds')
cat('PASS 48 injected failure-table checks and 8 metric-table checks\n')
