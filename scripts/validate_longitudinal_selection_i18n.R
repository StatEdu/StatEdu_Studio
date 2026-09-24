Sys.setlocale('LC_CTYPE','English_United States.utf8')
source('scripts/validate_longitudinal_input_errors_i18n.R',encoding='UTF-8')
d<-data.frame(y=1:6,id=rep(1:3,each=2),time=rep(1:2,3))
errors<-list(
 outcome=capture(prepare_longitudinal_analysis_result(d,character(0),'id','time')),
 id=capture(prepare_longitudinal_analysis_result(d,'y',character(0),'time')),
 time=capture(prepare_longitudinal_analysis_result(d,'y','id',character(0))),
 terms=capture(prepare_longitudinal_analysis_result(d,'y','id','time',include_time=FALSE)))
# Normalization currently falls back to no weights; exercise its defensive guard independently.
fit<-prepare_longitudinal_analysis_result
environment(fit)<-list2env(list(longitudinal_resolve_context_weight_type=function(...)'sampling'),parent=environment(prepare_longitudinal_analysis_result))
errors$weight<-capture(fit(d,'y','id','time',weight_type='sampling'))
for(key in names(errors))stopifnot(inherits(errors[[key]],'error'),inherits(errors[[key]],'validation'),conditionMessage(errors[[key]])==statedu_t(paste0('longitudinal.selection_error.',key),'en'))
for(lang in c('en','ko','ja','zh','es','fr','de','vi','ko')) {
 app_language_fn<-function()lang
 for(key in names(errors)) {
  expected<-statedu_t(paste0('longitudinal.selection_error.',key),lang)
  stopifnot(longitudinal_input_error_text(errors[[key]],lang)==expected)
  cleared<-FALSE;eval(callback)(errors[[key]])
  stopifnot(cleared,notice$text==paste(statedu_t('analysis.status.longitudinal_failed',lang),expected),notice$type=='error',notice$duration==8)
 }
 raw<-'Select one outcome variable. User: 한글 %s <&>'
 stopifnot(longitudinal_input_error_text(simpleError(raw),lang)==raw)
 cat('PASS selection:',lang,'5 Shiny validation conditions and actual failure callback\n')
}
