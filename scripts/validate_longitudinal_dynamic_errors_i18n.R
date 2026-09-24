Sys.setlocale('LC_CTYPE','English_United States.utf8')
source('scripts/validate_longitudinal_input_errors_i18n.R',encoding='UTF-8')
d<-data.frame(y=c(-1,0,2,3),x=1:4,.statedu_gee_id=c(1,1,2,2),.statedu_gee_waves=c(1,2,1,2))
gamma_error<-capture(longitudinal_gee_adjusted(y~x,d,Gamma()))
correlation_error<-capture(longitudinal_gee_check_simple_correlation('exchangeable',2,c(1,1),c(1,2)))
stopifnot(inherits(gamma_error,'error'),inherits(correlation_error,'error'))
# Evaluate the exact source sprintf expressions for defensive convergence guards.
expressions<-list()
walk_expr<-function(node){
 if(missing(node))return()
 if(is.call(node)&&identical(node[[1]],as.name('sprintf'))&&is.character(node[[2]])&&grepl('^Adjusted GEE (did not converge|working correlation is)',node[[2]]))expressions[[length(expressions)+1L]]<<-node
 if(is.call(node)||is.expression(node))for(child in as.list(node))walk_expr(child)
}
walk_expr(body(longitudinal_gee_adjusted));stopifnot(length(expressions)==2L)
iter<-100;step<-c(1.23456789e-7,0);ev<-c(-.000123456789,1)
convergence<-simpleError(eval(expressions[[1]]));negative<-simpleError(eval(expressions[[2]]))
ev<-c(0,1);singular<-simpleError(eval(expressions[[2]]))
cases<-list(
 list(error=gamma_error,key='gamma_rows',args=list('2')),
 list(error=correlation_error,key='correlation',args=list('exchangeable',sprintf('%.6g',min(eigen(matrix(c(1,2,2,1),2),symmetric=TRUE,only.values=TRUE)$values)))),
 list(error=convergence,key='iterations',args=list('100','1.23457e-07')),
 list(error=negative,key='adjusted_correlation',args=list('not_positive','-0.000123457')),
 list(error=singular,key='adjusted_correlation',args=list('singular','0')))
for(lang in c('en','ko','ja','zh','es','fr','de','vi','ko')) {
 app_language_fn<-function()lang
 for(case in cases) {
  args<-case$args
  if(case$key=='adjusted_correlation')args[[1]]<-statedu_t(paste0('longitudinal.dynamic_error.',args[[1]]),lang)
  expected<-do.call(sprintf,c(list(statedu_t(paste0('longitudinal.dynamic_error.',case$key),lang)),args))
  stopifnot(longitudinal_input_error_text(case$error,lang)==expected)
  eval(callback)(case$error)
  stopifnot(notice$text==paste(statedu_t('analysis.status.longitudinal_failed',lang),expected),notice$type=='error',notice$duration==8)
  extended<-paste(conditionMessage(case$error),'User %s <&>')
  stopifnot(longitudinal_input_error_text(simpleError(extended),lang)==extended)
 }
 # Preserve scientific notation exactly, including its plus sign and trailing zeros.
 text<-sprintf(statedu_t('longitudinal.dynamic_error.correlation','en'),'ar1','-1.2300e+02')
 stopifnot(longitudinal_input_error_text(simpleError(text),lang)==sprintf(statedu_t('longitudinal.dynamic_error.correlation',lang),'ar1','-1.2300e+02'))
 cat('PASS dynamic errors:',lang,'counts, eigenvalues, convergence, exact numeric text and callbacks\n')
}
