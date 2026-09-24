out<-'tmp/survival-planning-errors-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
base<-list(sample_size_survival_target='sample_size',sample_size_survival_hr='.5',sample_size_survival_event_probability='.5',sample_size_survival_alpha='.05',sample_size_survival_power='.8',sample_size_survival_n='100',sample_size_survival_ratio='2',sample_size_survival_alternative='two.sided',sample_size_survival_dropout='0')
errors<-list();keys<-character()
for(value in c('0','-1','1','NaN','Inf')){
 errors<-c(errors,list(effect_size_survival_calculate(list(effect_size_survival_hr=value)),sample_size_calculate('survival',modifyList(base,list(sample_size_survival_hr=value)))))
 keys<-c(keys,rep('sample_size.result.error_survival_hr',2))
}
for(value in c('0','1','NaN','Inf')){
 errors<-c(errors,list(sample_size_calculate('survival',modifyList(base,list(sample_size_survival_event_probability=value)))))
 keys<-c(keys,'sample_size.result.error_survival_event_probability')
}
before_errors<-serialize(errors,NULL)
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(i in seq_along(errors)){
 stopifnot(identical(errors[[i]]$error,statedu_t(keys[i],'en')))
 expected<-statedu_t(keys[i],lang,fallback='')
 actual<-xml2::xml_text(xml2::xml_find_first(xml2::read_html(as.character(sample_size_results_ui(errors[[i]],lang))),'//div[@class="analysis-warning"]'))
 stopifnot(nzchar(expected),identical(actual,expected));if(lang!='en')stopifnot(actual!=errors[[i]]$error)
 unknown<-paste0(errors[[i]]$error,' custom');stopifnot(identical(sample_size_result_text(unknown,lang),unknown))
}
stopifnot(length(errors)==14L,identical(before_errors,serialize(errors,NULL)))
cases<-expand.grid(hr=c(.5,2),target=c('sample_size','power'),stringsAsFactors=FALSE)
results<-lapply(seq_len(nrow(cases)),function(i)sample_size_calculate('survival',modifyList(base,list(sample_size_survival_hr=as.character(cases$hr[i]),sample_size_survival_target=cases$target[i]))))
results<-c(results,lapply(c(.5,2),function(hr)effect_size_survival_calculate(list(effect_size_survival_hr=as.character(hr)))))
for(x in results)stopifnot(is.null(x$error))
events<-ceiling((qnorm(.975)+qnorm(.8))^2/((1/3)*(2/3)*log(.5)^2))
for(i in 1:2)stopifnot(results[[i]]$required_events==events,results[[i]]$total==ceiling(events/.5/3)+ceiling(events/.5*2/3))
stopifnot(identical(results[[3]]$power,results[[4]]$power),isTRUE(all.equal(results[[5]]$log_hazard_ratio,log(.5))),isTRUE(all.equal(results[[6]]$log_hazard_ratio,log(2))))
designs<-c(paste(cases$hr,cases$target),'effect-.5','effect-2');formula_keys<-paste0('sample_size.result.',c(rep('planning_survival',4),rep('survival_hr',2)))
cat('PASS fourteen actual survival errors x eight languages, reciprocal HR/event-count references and six valid outputs\n')
