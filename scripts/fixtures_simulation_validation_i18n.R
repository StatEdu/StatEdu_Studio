source('scripts/fixtures_lmm_correlation_errors_i18n.R',encoding='UTF-8')
lmm_input<-base;lmm_valid<-results[[1]]
source('scripts/fixtures_advanced_planning_i18n.R',encoding='UTF-8')
out<-'tmp/simulation-validation-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
results<-c(list(lmm_valid),results[c(1,9)])
designs<-c('LMM-20','stepped-wedge-20','SEM-100')
formula_keys<-paste0('sample_size.result.',c('planning_lmm_gls','planning_cluster_stepped','planning_sem_parameter'))
cluster_input<-modifyList(cinput,list(sample_size_cluster_design='stepped_wedge',sample_size_cluster_target='power',sample_size_cluster_n='12'))
sem_input<-modifyList(sinput,list(sample_size_sem_test='parameter',sample_size_sem_target='power'))
errors<-list();error_keys<-character()
for(value in c('19','0','NaN')){
 errors<-c(errors,list(sample_size_calculate('lmm',modifyList(lmm_input,list(sample_size_lmm_simulations=value))),sample_size_calculate('cluster',modifyList(cluster_input,list(sample_size_cluster_simulations=value)))))
 error_keys<-c(error_keys,rep('sample_size.result.error_simulations_min',2))
}
for(value in c('NaN','Inf','bad')){
 errors<-c(errors,list(suppressWarnings(sample_size_calculate('sem',modifyList(sem_input,list(sample_size_sem_simulations=value))))))
 error_keys<-c(error_keys,'sample_size.result.error_simulations_numeric')
}
before_errors<-serialize(errors,NULL)
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(i in seq_along(errors)){
 stopifnot(identical(errors[[i]]$error,statedu_t(error_keys[i],'en')))
 expected<-statedu_t(error_keys[i],lang,fallback='')
 actual<-xml2::xml_text(xml2::xml_find_first(xml2::read_html(as.character(sample_size_results_ui(errors[[i]],lang))),'//div[@class="analysis-warning"]'))
 stopifnot(nzchar(expected),identical(actual,expected));if(lang!='en')stopifnot(actual!=errors[[i]]$error)
 unknown<-paste0(errors[[i]]$error,' custom');stopifnot(identical(sample_size_result_text(unknown,lang),unknown))
}
stopifnot(length(errors)==9L,identical(before_errors,serialize(errors,NULL)))
# SEM has its own existing 100-draw floor rather than the LMM/cluster 20 minimum.
sem_floor<-sample_size_calculate('sem',modifyList(sem_input,list(sample_size_sem_simulations='20')))
stopifnot(is.null(sem_floor$error),identical(sem_floor$power,results[[3]]$power),identical(sem_floor$method_note,results[[3]]$method_note))
cat('PASS nine actual simulation-count failures x eight languages; LMM/cluster minimum20 and SEM floor100 preserved\n')
