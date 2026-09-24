out<-'tmp/logistic-validation-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
base<-list(sample_size_regression_design='logistic',sample_size_regression_target='sample_size',sample_size_regression_alpha='.05',sample_size_regression_power='.8',sample_size_regression_n='150',sample_size_regression_dropout='0',sample_size_regression_alternative='two.sided',sample_size_regression_or='2',sample_size_regression_p0='.3',sample_size_regression_predictor_prevalence='.5',sample_size_regression_covariate_r2='0')
errors<-list();keys<-character()
for(value in c('0','-1','1','NaN','Inf')){
 errors<-c(errors,list(effect_size_regression_calculate(list(effect_size_regression_design='logistic_or',effect_size_regression_or=value)),sample_size_calculate('regression',modifyList(base,list(sample_size_regression_or=value)))))
 keys<-c(keys,rep('sample_size.result.error_logistic_or',2))
}
for(value in c('-.1','1','NaN')){errors<-c(errors,list(sample_size_calculate('regression',modifyList(base,list(sample_size_regression_covariate_r2=value)))));keys<-c(keys,'sample_size.result.error_logistic_covariate_r2')}
before_errors<-serialize(errors,NULL)
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(i in seq_along(errors)){
 stopifnot(identical(errors[[i]]$error,statedu_t(keys[i],'en')))
 expected<-statedu_t(keys[i],lang,fallback='')
 actual<-xml2::xml_text(xml2::xml_find_first(xml2::read_html(as.character(sample_size_results_ui(errors[[i]],lang))),'//div[@class="analysis-warning"]'))
 stopifnot(nzchar(expected),identical(actual,expected));if(lang!='en')stopifnot(actual!=errors[[i]]$error)
 unknown<-paste0(errors[[i]]$error,' custom');stopifnot(identical(sample_size_result_text(unknown,lang),unknown))
}
stopifnot(length(errors)==13L,identical(before_errors,serialize(errors,NULL)))
cases<-expand.grid(or=c(.5,2),target=c('sample_size','power'),stringsAsFactors=FALSE)
results<-lapply(1:4,function(i)sample_size_calculate('regression',modifyList(base,list(sample_size_regression_or=as.character(cases$or[i]),sample_size_regression_target=cases$target[i]))))
reference_power<-function(n)pnorm(log(2)*sqrt(n*.3*.7*.5*.5)-qnorm(.975))
for(i in 1:2)stopifnot(is.null(results[[i]]$error),reference_power(results[[i]]$total)>=.8,reference_power(results[[i]]$total-1)<.8)
for(i in 3:4)stopifnot(isTRUE(all.equal(results[[i]]$power,reference_power(150))))
effect<-lapply(c(.5,2),function(or)effect_size_regression_calculate(list(effect_size_regression_design='logistic_or',effect_size_regression_or=as.character(or))))
for(i in 1:2)stopifnot(is.null(effect[[i]]$error),isTRUE(all.equal(effect[[i]]$effect_size_d,log(c(.5,2)[i])*sqrt(3)/pi)))
results<-c(results,effect);designs<-c(paste(cases$or,cases$target),'effect-.5','effect-2');formula_keys<-paste0('sample_size.result.',c(rep('planning_logistic',4),rep('note_logistic_d',2)))
cat('PASS thirteen actual logistic errors x eight languages, reciprocal OR and independent minimum-n/power/d references\n')
