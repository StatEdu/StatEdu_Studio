out<-'tmp/correlation-planning-errors-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
cbase<-list(sample_size_correlation_target='sample_size',sample_size_correlation_r='.3',sample_size_correlation_alpha='.05',sample_size_correlation_power='.8',sample_size_correlation_n='100',sample_size_correlation_alternative='two.sided',sample_size_correlation_dropout='0')
pbase<-list(sample_size_precision_target='sample_size',sample_size_precision_parameter='correlation',sample_size_precision_r='.3',sample_size_precision_confidence='.95',sample_size_precision_half_width='.1',sample_size_precision_n='100',sample_size_precision_dropout='0')
errors<-list();keys<-character()
for(value in c('-1','1','NaN','Inf')){
 errors<-c(errors,list(sample_size_calculate('correlation',modifyList(cbase,list(sample_size_correlation_r=value))),sample_size_calculate('precision',modifyList(pbase,list(sample_size_precision_r=value)))))
 keys<-c(keys,'sample_size.result.error_expected_r_nonzero','sample_size.result.error_expected_r_finite')
}
errors<-c(errors,list(sample_size_calculate('correlation',modifyList(cbase,list(sample_size_correlation_r='0'))),sample_size_calculate('precision',modifyList(pbase,list(sample_size_precision_r='.9999999')))))
keys<-c(keys,'sample_size.result.error_expected_r_nonzero','sample_size.result.error_correlation_half_width')
before_errors<-serialize(errors,NULL)
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(i in seq_along(errors)){
 stopifnot(identical(errors[[i]]$error,statedu_t(keys[i],'en')))
 expected<-statedu_t(keys[i],lang,fallback='')
 actual<-xml2::xml_text(xml2::xml_find_first(xml2::read_html(as.character(sample_size_results_ui(errors[[i]],lang))),'//div[@class="analysis-warning"]'))
 stopifnot(nzchar(expected),identical(actual,expected));if(lang!='en')stopifnot(actual!=errors[[i]]$error)
 unknown<-paste0(errors[[i]]$error,' custom');stopifnot(identical(sample_size_result_text(unknown,lang),unknown))
}
stopifnot(length(errors)==10L,identical(before_errors,serialize(errors,NULL)))
cases<-expand.grid(r=c(-.3,.3),target=c('sample_size','power'),stringsAsFactors=FALSE)
results<-lapply(1:4,function(i)sample_size_calculate('correlation',modifyList(cbase,list(sample_size_correlation_r=as.character(cases$r[i]),sample_size_correlation_target=cases$target[i]))))
for(i in 1:2)stopifnot(results[[i]]$total==ceiling(((qnorm(.975)+qnorm(.8))/atanh(.3))^2+3))
stopifnot(identical(results[[3]]$power,results[[4]]$power))
precision<-lapply(c(-.3,0,.3),function(r)sample_size_calculate('precision',modifyList(pbase,list(sample_size_precision_r=as.character(r)))))
for(i in 1:3){r<-c(-.3,0,.3)[i];stopifnot(precision[[i]]$total==ceiling((qnorm(.975)/(atanh(r+.1)-atanh(r)))^2+3))}
results<-c(results,precision);for(x in results)stopifnot(is.null(x$error))
designs<-c(paste(cases$r,cases$target),'precision-negative','precision-zero','precision-positive');formula_keys<-paste0('sample_size.result.',c(rep('planning_correlation',4),rep('planning_precision_r',3)))
cat('PASS ten actual correlation/precision errors x eight languages; signed-r planning and three precision references including zero\n')
