Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
stopifnot(requireNamespace('mice',quietly=TRUE))
set.seed(917)
data<-data.frame(id=rep(1:20,each=3),time=rep(1:3,20),x=rnorm(60),y=rnorm(60),w=rep(1,60))
mi<-function(d,...)longitudinal_mi_sensitivity_results(d,'y','id','time',c('time','x'),'gee','gaussian','exchangeable',m=2L,maxit=1L,...)
fixtures<-list(mi_small=mi(data[1:2,]),mi_unneeded=mi(data))
missing<-data;missing$x[c(2,8,15)]<-NA;missing$w[1]<-0
fixtures$mi_weights<-mi(missing,weight='w',weight_type='sampling')
weighted<-function(key,raw,analyzed,model='gee',...)longitudinal_weighted_missing_sensitivity_results(key,raw,analyzed,'y','id','time',c('time','x'),model,'gaussian','exchangeable',...)
fixtures$wgee_model<-weighted('wgee',data,data,'lmm')
fixtures$ipw_weights<-weighted('ipw',missing,missing[complete.cases(missing),],weight='w',weight_type='combined')
pool_strategy<-'Multiple imputation (MI)'
fixtures$pool_tables<-longitudinal_pool_coef_tables(list(),pool_strategy)
fixtures$pool_terms<-longitudinal_pool_coef_tables(list(data.frame(Term='x',B=1,SE=.1),data.frame(Term='y',B=2,SE=.2)),pool_strategy)
pool_user_term<-'사용자 <&> %s; imputation 2: x.\n두 번째 줄'
fixtures$pool_finite<-longitudinal_pool_coef_tables(list(data.frame(Term=pool_user_term,B=NA_real_,SE=NA_real_)),pool_strategy)
# Exercise unavailable-package and empty-engine-result branches without modifying
# installed packages or the production analysis functions.
package_env<-new.env(parent=environment(longitudinal_mi_sensitivity_results))
package_env$requireNamespace<-function(package,quietly=FALSE) FALSE
package_fn<-longitudinal_mi_sensitivity_results;environment(package_fn)<-package_env
fixtures$mi_package<-package_fn(data,'y','id','time',c('time','x'),'gee','gaussian','exchangeable')
engine_env<-new.env(parent=environment(longitudinal_weighted_missing_sensitivity_results))
engine_env$longitudinal_fit_model<-function(...)list(coef_table=data.frame())
engine_fn<-longitudinal_weighted_missing_sensitivity_results;environment(engine_fn)<-engine_env
fixtures$ipw_coefficients<-engine_fn('ipw',data,data,'y','id','time',c('time','x'),'gee','gaussian','exchangeable')
stopifnot(all(vapply(fixtures,nrow,integer(1))==1L),fixtures$mi_unneeded$Status=='Not needed',all(vapply(fixtures[names(fixtures)!='mi_unneeded'],function(x)x$Status=='Failed',logical(1))))
stopifnot(grepl('imputation 1 weights:',fixtures$mi_weights$Note,fixed=TRUE),grepl('imputation 2 weights:',fixtures$mi_weights$Note,fixed=TRUE))
dir.create('tmp/longitudinal-error-exports',recursive=TRUE,showWarnings=FALSE)
saveRDS(fixtures,'tmp/longitudinal-error-exports/actual-mi-ipw.rds')
for(lang in c('ko','ja','zh','es','fr','de','vi'))for(key in names(fixtures)) {
 tab<-longitudinal_appendix_table(fixtures[[key]],lang)
 index<-match('Note',names(fixtures[[key]]))
 if(identical(tab[[index]],fixtures[[key]]$Note))stop('Untranslated: ',lang,' / ',key,' / ',fixtures[[key]]$Note)
 stopifnot(identical(tab$B,fixtures[[key]]$B))
 if(key=='pool_finite')stopifnot(grepl(pool_user_term,tab[[index]],fixed=TRUE))
 if(key %in% c('mi_package','ipw_coefficients')) {
  suffix<-if(key=='mi_package')'package' else 'coefficients'
  stopifnot(identical(tab[[index]],statedu_t(paste0('longitudinal.sensitivity_error.',suffix),lang)))
 }
 cat('PASS actual MI/IPW:',lang,key,'\n')
}
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 original<-data.frame(Variable=c(fixtures$pool_finite$Note,fixtures$mi_package$Note,fixtures$ipw_coefficients$Note),Details=c(fixtures$pool_finite$Note,fixtures$mi_package$Note,fixtures$ipw_coefficients$Note))
 localized<-longitudinal_appendix_table(original,lang)
 stopifnot(identical(original[[1]],localized[[1]]))
 if(lang=='en')stopifnot(identical(original[[2]],localized[[2]]),identical(names(original),names(localized)))
}
