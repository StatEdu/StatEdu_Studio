Sys.setlocale('LC_CTYPE','English_United States.utf8')
source('scripts/validate_longitudinal_input_errors_i18n.R',encoding='UTF-8')
d<-data.frame(y=c(1,3,2,5),x=1:4,id=c(1,1,2,2),time=c(1,2,1,2))
fit<-function(...)longitudinal_fit_model(d,'y','id','time','x',family='gaussian',corstr='reml_un',...)
errors<-list(lmm_prediction=capture(predict.statedu_repeated_lmm(list(),se.fit=TRUE)),gee_prediction=capture(predict.statedu_adjusted_gee(list(),se.fit=TRUE)),model_weights=capture(fit(model_type='lmm',weights=c(1,0))),reml_options=capture(fit(model_type='lmm',random_slope=TRUE)),unknown_model=capture(fit(model_type='unknown')))
# Isolate optimizer warm-up, then run the real finite-derivative guard.
newton<-longitudinal_reml_newton
environment(newton)<-list2env(list(optim=function(...)list(par=0)),parent=environment(longitudinal_reml_newton))
errors$reml_derivatives<-capture(newton(0,function(x)x^2,function(x)NaN,function(x)matrix(1)))
# Run the exact post-fit validation expression with invalid coefficient inference.
guard<-NULL
walk_guard<-function(node){
 if(missing(node))return()
 if(is.call(node)&&identical(node[[1]],as.name('if'))&&grepl('Repeated LMM returned invalid coefficient inference.',paste(deparse(node),collapse=' '),fixed=TRUE))guard<<-node
 if(is.call(node)||is.expression(node))for(child in as.list(node))walk_guard(child)
}
walk_guard(body(longitudinal_repeated_lmm));stopifnot(!is.null(guard))
cf<-matrix(c(1,-1,10,1,.5),nrow=1)
errors$lmm_inference<-capture(eval(guard))
for(key in names(errors))stopifnot(inherits(errors[[key]],'error'),conditionMessage(errors[[key]])==statedu_t(paste0('longitudinal.execution_error.',key),'en'))
for(lang in c('en','ko','ja','zh','es','fr','de','vi','ko')) {
 app_language_fn<-function()lang
 for(key in names(errors)) {
  expected<-statedu_t(paste0('longitudinal.execution_error.',key),lang)
  stopifnot(longitudinal_input_error_text(errors[[key]],lang)==expected)
  cleared<-FALSE;eval(callback)(errors[[key]])
  stopifnot(cleared,notice$text==paste(statedu_t('analysis.status.longitudinal_failed',lang),expected),notice$type=='error',notice$duration==8)
 }
 raw<-paste(conditionMessage(errors$reml_derivatives),'User %s <&>')
 stopifnot(longitudinal_input_error_text(simpleError(raw),lang)==raw)
 cat('PASS limits:',lang,'7 guards, actual notification callback, external detail preservation\n')
}
# Supported prediction paths still match the original lm/glm results.
lm_fit<-lm(y~x,d);adapter<-lm_fit;class(adapter)<-c('statedu_repeated_lmm',class(adapter))
stopifnot(identical(unname(predict(adapter)),unname(predict(lm_fit))))
glm_fit<-glm(y~x,d,family=gaussian());adapter<-glm_fit;class(adapter)<-c('statedu_adjusted_gee',class(adapter))
stopifnot(identical(unname(predict(adapter,type='response')),unname(predict(glm_fit,type='response'))))
