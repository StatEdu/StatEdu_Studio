# Six descriptions exercised by twelve real input branches.
out<-'tmp/survival-formula-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
eq<-list(effect_size_equivalence_margin='0.3',effect_size_equivalence_difference='-0.1',effect_size_equivalence_sd='2',effect_size_equivalence_p1='0.55',effect_size_equivalence_p2='0.65')
cases<-expand.grid(outcome=c('mean','proportion'),objective=c('equivalence','noninferiority'),stringsAsFactors=FALSE)
rates<-expand.grid(design=c('gamma_mean_ratio','poisson_irr','negative_binomial_irr'),scale=c('ratio','log_ratio'),stringsAsFactors=FALSE)
results<-c(list(effect_size_survival_calculate(list(effect_size_survival_hr='0.7'))),
 lapply(seq_len(nrow(cases)),function(i)effect_size_equivalence_calculate(modifyList(eq,list(effect_size_equivalence_outcome=cases$outcome[i],effect_size_equivalence_objective=cases$objective[i])))),
 list(effect_size_diagnostic_calculate(list(effect_size_diagnostic_auc='0.8',effect_size_diagnostic_null_auc='0.5'))),
 lapply(seq_len(nrow(rates)),function(i)effect_size_rates_calculate(list(effect_size_rates_design=rates$design[i],effect_size_rates_input_scale=rates$scale[i],effect_size_rates_ratio='1.5',effect_size_rates_log_ratio=as.character(log(1.5))))))
designs<-c('survival',paste(cases$outcome,cases$objective),'auc',paste(rates$design,rates$scale))
formula_keys<-paste0('sample_size.result.',c('survival_hr',ifelse(cases$objective=='equivalence','equivalence_distance','noninferiority_distance'),'diagnostic_auc',ifelse(rates$design=='gamma_mean_ratio','gamma_ratio','count_ratio')))
tokens<-list(survival_hr='log(HR)',equivalence_distance='margin - abs(observed effect)',noninferiority_distance=c('margin + observed effect','-margin'),diagnostic_auc=c('AUC - null AUC','sqrt(2) * qnorm(AUC)'),gamma_ratio=c('exp(beta)','beta'),count_ratio=c('exp(beta)','beta'))
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(key in names(tokens)) {
 value<-statedu_t(paste0('sample_size.result.',key),lang,fallback='')
 stopifnot(nzchar(value),all(vapply(tokens[[key]],grepl,logical(1),x=value,fixed=TRUE)))
}
for(i in seq_along(results))stopifnot(is.null(results[[i]]$error),identical(results[[i]]$formula_note,statedu_t(formula_keys[i],'en')))
stopifnot(isTRUE(all.equal(results[[1]]$log_hazard_ratio,log(0.7))),isTRUE(all.equal(results[[2]]$standardized_distance,0.1)),isTRUE(all.equal(results[[4]]$standardized_distance,0.1)),isTRUE(all.equal(results[[6]]$auc_cohen_d,sqrt(2)*qnorm(0.8))))
for(i in 7:12)stopifnot(isTRUE(all.equal(results[[i]]$ratio,1.5)),isTRUE(all.equal(results[[i]]$log_ratio,log(1.5))))
cat('PASS 12 real input branches; six descriptions x eight languages; numeric reference checks\n')
