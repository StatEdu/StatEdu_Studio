Sys.setlocale('LC_ALL','Korean_Korea.utf8')
source('scripts/validate_repeated_lmm.R',encoding='UTF-8')
# Use the same public result path as the analysis form.
d$time<-rep(1:2,80)
for(mode in c('reml_un','reml_ar1')){
 results<-prepare_longitudinal_analysis_result(d,'y','id','time',predictors=c('group','interaction'),model_type='lmm',family='gaussian',corstr=mode,assumption_checks=TRUE)
 stopifnot(length(results)==1L,identical(results[[1]]$corstr,mode))
 result<-results[[1]]
 stopifnot('df'%in%names(result$coef_table),all(is.finite(result$coef_table$p)),any(grepl('REML',result$notes)),any(result$software_versions$Software=='mmrm'))
 stopifnot(!any(grepl('Random intercept grouping',result$notes,fixed=TRUE)),nrow(result$sensitivity_results)==0L)
 stopifnot(any(result$assumption_checks$Check=='REML convergence'))
 state<-longitudinal_setup_state(names(d),data.frame(name=names(d),label=names(d)),model_type='lmm',corstr=mode,language='ko')
 rendered<-as.character(longitudinal_setup_panel(state,NULL))
 stopifnot(grepl(paste0('value="',mode,'" selected'),rendered,fixed=TRUE),!grepl('id="longitudinal_random_slope"',rendered,fixed=TRUE))
 restored<-unserialize(serialize(results,NULL));stopifnot(identical(restored[[1]]$corstr,mode),identical(restored[[1]]$coef_table,result$coef_table))
 fitted<-longitudinal_fit_model(d,'y','id','time',c('group','interaction'),'lmm','gaussian',mode)
 stopifnot(max(abs(predict(fitted$model)-fitted(fitted$model)))<1e-8)
 stopifnot(max(abs(diag(vcov(fitted$model))-fitted$coef_table$SE^2))<1e-8)
}
cat('Repeated LMM UI, analysis dispatch, inference, assumptions, report metadata, and serialization passed.\n')
