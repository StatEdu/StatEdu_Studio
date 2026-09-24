out<-'tmp/clinical-planning-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
stopifnot(requireNamespace('TOSTER',quietly=TRUE))
base<-function(m){v<-list(target='sample_size',alpha='0.05',power='0.8',n='100',ratio='1',alternative='two.sided',dropout='0');setNames(v,paste0('sample_size_',m,'_',names(v)))}
surv<-modifyList(base('survival'),list(sample_size_survival_hr='0.7',sample_size_survival_event_probability='0.5',sample_size_survival_ratio='2'))
eq<-modifyList(base('equivalence'),list(sample_size_equivalence_difference='0',sample_size_equivalence_margin='0.2',sample_size_equivalence_sd='1',sample_size_equivalence_p1='0.5',sample_size_equivalence_p2='0.5'))
ec<-expand.grid(outcome=c('mean','proportion'),objective=c('equivalence','noninferiority'),stringsAsFactors=FALSE)
diag<-modifyList(base('diagnostic'),list(sample_size_diagnostic_sensitivity='0.85',sample_size_diagnostic_specificity='0.9',sample_size_diagnostic_prevalence='0.2',sample_size_diagnostic_precision='0.1',sample_size_diagnostic_auc='0.75',sample_size_diagnostic_null_auc='0.5'))
methods<-c('survival',rep('equivalence',4),rep('diagnostic',3))
inputs<-c(list(surv),lapply(1:4,function(i)modifyList(eq,list(sample_size_equivalence_outcome=ec$outcome[i],sample_size_equivalence_objective=ec$objective[i]))),lapply(c('sensitivity','specificity','auc'),function(d)modifyList(diag,list(sample_size_diagnostic_design=d))))
designs<-c('survival-unequal',paste(ec$outcome,ec$objective),'sensitivity','specificity','auc')
formula_keys<-paste0('sample_size.result.',c('planning_survival','planning_tost_exact',rep('planning_equivalence_normal',3),rep('planning_diagnostic_precision',2),'planning_auc'))
results<-lapply(1:8,function(i)sample_size_calculate(methods[i],inputs[[i]]))
other<-lapply(1:8,function(i){v<-inputs[[i]];v[[paste0('sample_size_',methods[i],'_target')]]<-'power';sample_size_calculate(methods[i],v)})
results<-c(results,other);designs<-c(paste(designs,'sample-size'),paste(designs,'power-or-precision'));formula_keys<-rep(formula_keys,2)
for(i in seq_along(results))if(!is.null(results[[i]]$error)||!identical(results[[i]]$formula_note,statedu_t(formula_keys[i],'en')))stop('Unexpected result ',designs[i],': ',results[[i]]$error,' / ',results[[i]]$formula_note)
stopifnot(results[[2]]$engine=='TOSTER',results[[10]]$engine=='TOSTER',results[[1]]$total==results[[1]]$group1+results[[1]]$group2,results[[1]]$total>=results[[1]]$required_events)
for(i in c(9:13,16)){r<-results[[i]];stopifnot(is.finite(r$power),r$power>=0,r$power<=1)}
# Sensitivity/specificity return precision in their note, not a power estimate.
for(i in 14:15){p<-if(i==14).85 else .9;fraction<-if(i==14).2 else .8;half<-qnorm(.975)*sqrt(p*(1-p)/(100*fraction));stopifnot(is.na(results[[i]]$power),grepl(sprintf('Achieved half-width is approximately %.3f.',half),results[[i]]$method_note,fixed=TRUE))}
stopifnot(results[[6]]$total>0,results[[7]]$total>0)
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))stopifnot(grepl('TOSTER::power_t_TOST',statedu_t('sample_size.result.planning_tost_exact',lang),fixed=TRUE),grepl('Schoenfeld',statedu_t('sample_size.result.planning_survival',lang),fixed=TRUE),grepl('Buderer',statedu_t('sample_size.result.planning_diagnostic_precision',lang),fixed=TRUE))
cat('PASS 16 actual survival/equivalence/diagnostic cases; exact TOSTER and approximate branches, eight-language engine tokens\n')
