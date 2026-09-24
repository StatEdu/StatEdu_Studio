Sys.setlocale('LC_CTYPE','English_United States.utf8')
source('scripts/validate_longitudinal_input_errors_i18n.R',encoding='UTF-8')
gee_data<-data.frame(y=1:4,x=1:4,.statedu_gee_id=c(1,1,2,2),.statedu_gee_waves=c(1,1,1,2))
errors<-list(
 gee_parameter=capture(longitudinal_gee_check_simple_correlation('ar1',NA_real_,c(1,1),c(1,2))),
 gee_matrix=capture(longitudinal_gee_check_simple_correlation('ar1',2,c(1,1),c(0,2000))),
 gee_weights=capture(longitudinal_gee_fit(y~x,gee_data,gaussian(),'unstructured_adjusted',weighted=TRUE)),
 gee_pairs=capture(longitudinal_gee_fit(y~x,gee_data,gaussian(),'unstructured')))
# Mock only the estimator return, leaving the actual post-fit guards unchanged.
for(case in c('convergence','variance')) {
 fit<-longitudinal_gee_fit
 fake<-list(geese=list(error=if(case=='convergence')1L else 0L,alpha=0,vbeta=matrix(-1,1,1)),coefficients=c(x=1))
 environment(fit)<-list2env(list(do.call=function(...)fake),parent=environment(longitudinal_gee_fit))
 errors[[paste0('gee_',case)]]<-capture(fit(y~x,gee_data,gaussian(),'independence'))
}
lmm<-longitudinal_repeated_lmm
# Guards precede the mmrm fit; isolate dependency availability if mmrm is absent.
environment(lmm)<-list2env(list(requireNamespace=function(...)TRUE),parent=environment(longitudinal_repeated_lmm))
base<-data.frame(y=1:6,id=1:6,time=rep(1:2,3),x=seq_len(6))
bad<-base;bad$x<-1
errors$lmm_design<-capture(lmm(bad,'y','id','time','x'))
bad<-base;bad$id[2]<-bad$id[1];bad$time[2]<-bad$time[1]
errors$lmm_pairs<-capture(lmm(bad,'y','id','time','x'))
bad<-base;bad$y<-1
errors$lmm_variation<-capture(lmm(bad,'y','id','time','x'))
bad<-base;bad$time<-1
errors$lmm_occasions<-capture(lmm(bad,'y','id','time','x'))
for(key in names(errors))stopifnot(inherits(errors[[key]],'error'),conditionMessage(errors[[key]])==statedu_t(paste0('longitudinal.input_error.',key),'en'))
for(lang in c('en','ko','ja','zh','es','fr','de','vi','ko')) {
 app_language_fn<-function()lang
 for(key in names(errors)) {
  expected<-statedu_t(paste0('longitudinal.input_error.',key),lang)
  stopifnot(longitudinal_input_error_text(errors[[key]],lang)==expected)
  cleared<-FALSE;eval(callback)(errors[[key]])
  stopifnot(cleared,notice$type=='error',notice$duration==8,notice$text==paste(statedu_t('analysis.status.longitudinal_failed',lang),expected))
 }
 raw<-paste(conditionMessage(errors$gee_matrix),'User <&> %s')
 stopifnot(longitudinal_input_error_text(simpleError(raw),lang)==raw)
 cat('PASS model errors:',lang,'10 guard branches, notification, unknown detail preservation\n')
}
stopifnot(is.null(longitudinal_gee_check_simple_correlation('exchangeable',.2,c(1,1,2,2),c(1,2,1,2))))
