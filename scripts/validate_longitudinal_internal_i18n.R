Sys.setlocale('LC_CTYPE','English_United States.utf8')
source('scripts/validate_longitudinal_input_errors_i18n.R',encoding='UTF-8')
d<-data.frame(y=1:4,x=1:4,.statedu_gee_id=c(1,1,2,2),.statedu_gee_waves=c(1,1,1,2))
errors<-list(duplicate=capture(longitudinal_gee_adjusted(y~x,d,gaussian())),singular=capture(longitudinal_gee_check_unstructured(1)))
lmm<-longitudinal_repeated_lmm
environment(lmm)<-list2env(list(requireNamespace=function(...)FALSE),parent=environment(longitudinal_repeated_lmm))
errors$mmrm<-capture(lmm(d,'y','.statedu_gee_id','.statedu_gee_waves','x'))
# Execute original defensive conditions with invalid intermediate values.
guard_for<-function(fn,text){
 found<-NULL
 walk<-function(node){
  if(missing(node))return()
  if(is.call(node)&&identical(node[[1]],as.name('if'))&&grepl(text,paste(deparse(node),collapse=' '),fixed=TRUE))found<<-node
  if(is.call(node)||is.expression(node))for(child in as.list(node))walk(child)
 }
 walk(body(fn));stopifnot(!is.null(found));found
}
v<-NA_real_;corr<-matrix(NA_real_);se<-c(.1,-1);ratios<-numeric(0)
for(key in c('variance','correlation','coefficient_variance','comparison')) {
 fn<-if(key=='comparison')longitudinal_panel_driscoll_kraay_summary else longitudinal_gee_adjusted
 errors[[key]]<-capture(eval(guard_for(fn,statedu_t(paste0('longitudinal.internal_error.',key),'en'))))
}
for(key in names(errors))stopifnot(inherits(errors[[key]],'error'),conditionMessage(errors[[key]])==statedu_t(paste0('longitudinal.internal_error.',key),'en'))
# Audit literal stop messages in this engine, rather than treating keys as coverage.
literals<-character(0)
collect<-function(node){
 if(missing(node))return()
 if(is.call(node)&&identical(node[[1]],as.name('stop'))&&is.character(node[[2]]))literals<<-c(literals,node[[2]])
 if(is.call(node)||is.expression(node))for(child in as.list(node))collect(child)
}
collect(parse('R/analysis_longitudinal.R',encoding='UTF-8'));literals<-unique(literals)
for(lang in c('en','ko','ja','zh','es','fr','de','vi','ko')) {
 app_language_fn<-function()lang
 for(key in names(errors)) {
  expected<-statedu_t(paste0('longitudinal.internal_error.',key),lang)
  stopifnot(longitudinal_input_error_text(errors[[key]],lang)==expected)
  cleared<-FALSE;eval(callback)(errors[[key]])
  stopifnot(cleared,notice$text==paste(statedu_t('analysis.status.longitudinal_failed',lang),expected),notice$type=='error',notice$duration==8)
 }
 if(lang!='en')for(message in literals)stopifnot(longitudinal_input_error_text(simpleError(message),lang)!=message)
 cat('PASS internal:',lang,'7 conditions;',length(literals),'literal stop messages covered at notification boundary\n')
}
