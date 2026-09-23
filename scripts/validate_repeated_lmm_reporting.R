source('scripts/validate_repeated_lmm_integration.R',encoding='UTF-8')
for(mode in c('reml_un','reml_ar1')){
 r<-prepare_longitudinal_analysis_result(d,'y','id','time',predictors=c('group','interaction'),model_type='lmm',corstr=mode)
 notes<-r[[1]]$publication_notes$Note
 stopifnot(any(grepl('Satterthwaite',notes)),!any(grepl('subject-specific',notes)))
 guide<-longitudinal_assumption_review_table(r)
 stopifnot(guide[[2]][1]=='Population-averaged',grepl('no random effects',guide[[2]][2]),grepl('mmrm',guide[[2]][3]))
 stopifnot('df'%in%names(longitudinal_display_coef_table(r[[1]])))
}
cat('REML publication notes, interpretation guide, package, and exported df passed.\n')
