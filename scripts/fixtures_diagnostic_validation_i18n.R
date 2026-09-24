out<-'tmp/diagnostic-validation-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
base<-list(sample_size_diagnostic_target='sample_size',sample_size_diagnostic_alpha='.05',sample_size_diagnostic_power='.8',sample_size_diagnostic_n='100',sample_size_diagnostic_ratio='1',sample_size_diagnostic_dropout='0',sample_size_diagnostic_sensitivity='.85',sample_size_diagnostic_specificity='.9',sample_size_diagnostic_prevalence='.2',sample_size_diagnostic_precision='.1',sample_size_diagnostic_auc='.75',sample_size_diagnostic_null_auc='.5')
errors<-list();error_keys<-character()
add_error<-function(value,key){errors[[length(errors)+1L]]<<-value;error_keys<<-c(error_keys,paste0('sample_size.result.',key))}
for(auc in c('.5','.4')){
 add_error(effect_size_diagnostic_calculate(list(effect_size_diagnostic_auc=auc,effect_size_diagnostic_null_auc='.5')),'error_auc_null')
 add_error(sample_size_calculate('diagnostic',modifyList(base,list(sample_size_diagnostic_design='auc',sample_size_diagnostic_auc=auc))),'error_expected_auc_null')
}
for(design in c('sensitivity','specificity'))for(precision in c('1','1.5'))add_error(sample_size_calculate('diagnostic',modifyList(base,list(sample_size_diagnostic_design=design,sample_size_diagnostic_precision=precision))),'error_diagnostic_precision')
before_errors<-serialize(errors,NULL)
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(i in seq_along(errors)){
 stopifnot(identical(errors[[i]]$error,statedu_t(error_keys[i],'en')))
 actual<-xml2::xml_text(xml2::xml_find_first(xml2::read_html(as.character(sample_size_results_ui(errors[[i]],lang))),'//div[@class="analysis-warning"]'))
 expected<-statedu_t(error_keys[i],lang,fallback='')
 stopifnot(nzchar(expected),identical(actual,expected))
 if(lang!='en')stopifnot(!identical(expected,errors[[i]]$error))
 unknown<-paste0(errors[[i]]$error,' custom');stopifnot(identical(sample_size_result_text(unknown,lang),unknown))
}
stopifnot(identical(before_errors,serialize(errors,NULL)),length(errors)==8L)
cases<-expand.grid(design=c('sensitivity','specificity','auc'),target=c('sample_size','power'),stringsAsFactors=FALSE)
results<-lapply(seq_len(nrow(cases)),function(i)sample_size_calculate('diagnostic',modifyList(base,list(sample_size_diagnostic_design=cases$design[i],sample_size_diagnostic_target=cases$target[i]))))
results<-c(results,list(effect_size_diagnostic_calculate(list(effect_size_diagnostic_auc='.75',effect_size_diagnostic_null_auc='.5'))))
designs<-c(paste(cases$design,cases$target),'auc-effect')
formula_keys<-paste0('sample_size.result.',c(ifelse(cases$design=='auc','planning_auc','planning_diagnostic_precision'),'diagnostic_auc'))
stopifnot(all(vapply(results,function(x)is.null(x$error),logical(1))),isTRUE(all.equal(results[[7]]$auc_cohen_d,sqrt(2)*qnorm(.75))))
for(i in 1:2){p<-c(.85,.9)[i];fraction<-c(.2,.8)[i];events<-ceiling(qnorm(.975)^2*p*(1-p)/.1^2);stopifnot(results[[i]]$required_events==events,results[[i]]$total==ceiling(events/fraction))}
for(design in c('sensitivity','specificity'))stopifnot(is.null(sample_size_calculate('diagnostic',modifyList(base,list(sample_size_diagnostic_design=design,sample_size_diagnostic_precision='.999')))$error))
cat('PASS eight actual diagnostic errors x eight languages; seven valid snapshots, reference effects/sample sizes and two precision boundary controls\n')
