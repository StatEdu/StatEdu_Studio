out<-'tmp/mediation-planning-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
base<-list(sample_size_regression_design='mediation',sample_size_regression_alpha='0.05',sample_size_regression_power='0.8',sample_size_regression_n='100',sample_size_regression_ratio='1',sample_size_regression_alternative='two.sided',sample_size_regression_dropout='0',sample_size_regression_a='0.3',sample_size_regression_b='0.3',sample_size_regression_a_effect='small',sample_size_regression_b_effect='small',sample_size_regression_covariates='0',sample_size_regression_fritz_test='bias_corrected_bootstrap',sample_size_regression_simulations='30',sample_size_regression_bootstraps='100')
designs<-c('fritz_mackinnon','monte_carlo','bootstrap','sobel','sobel','fritz_mackinnon')
targets<-c('sample_size','power','power','sample_size','power','power')
set.seed(1937);seed_before<-.Random.seed
results<-lapply(seq_along(designs),function(i)sample_size_calculate('regression',modifyList(base,list(sample_size_regression_mediation_method=designs[i],sample_size_regression_target=targets[i]))))
stopifnot(identical(seed_before,.Random.seed))
formula_keys<-paste0('sample_size.result.',c('planning_mediation_fritz','planning_mediation_mc','planning_mediation_bootstrap','planning_mediation_sobel','planning_mediation_sobel','planning_mediation_mc'))
for(i in seq_along(results))if(!is.null(results[[i]]$error)||!identical(results[[i]]$formula_note,statedu_t(formula_keys[i],'en')))stop('Unexpected result: ',designs[i],' ',targets[i],' ',results[[i]]$error)
stopifnot(results[[1]]$total==462,results[[6]]$mediation_method=='monte_carlo',isTRUE(all.equal(results[[2]]$indirect_effect,.09)),identical(results[[2]]$power,results[[6]]$power))
for(i in which(targets=='power'))stopifnot(is.finite(results[[i]]$power),results[[i]]$power>=0,results[[i]]$power<=1)
# Compatibility snapshot: an older result lacking a mediation-method identifier.
legacy<-results[[4]];legacy$mediation_method<-NULL
details<-sample_size_method_details('regression',legacy);legacy$formula_note<-details$formula;legacy$references<-details$references
results<-c(results,list(legacy));designs<-c(paste(designs,targets),'legacy-without-method');formula_keys<-c(formula_keys,'sample_size.result.planning_mediation_fallback')
stopifnot(identical(legacy$formula_note,statedu_t(tail(formula_keys,1),'en')))
for(lang in c('en','ko','ja','zh','es','fr','de','vi')){
 value<-statedu_t('sample_size.result.planning_mediation_fritz',lang,fallback='')
 stopifnot(nzchar(value),all(vapply(c('Fritz & MacKinnon','2007','3','.80'),grepl,logical(1),x=value,fixed=TRUE)))
}
cat('PASS six real mediation cases + one legacy snapshot; fixed .80 table, MC fallback, random-seed preservation\n')
