out<-'tmp/regression-count-errors-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
base<-list(sample_size_regression_target='power',sample_size_regression_alpha='.05',sample_size_regression_power='.8',sample_size_regression_n='150',sample_size_regression_dropout='0',sample_size_regression_effect='.15',sample_size_regression_predictors='1',sample_size_regression_tested='1',sample_size_regression_interactions='1',sample_size_regression_total_predictors='1')
calc<-function(design,extra=list())suppressWarnings(sample_size_calculate('regression',modifyList(modifyList(base,list(sample_size_regression_design=design)),extra)))
errors<-list();keys<-character()
for(v in c('0','-1','NaN','Inf')){errors<-c(errors,list(calc('multiple',list(sample_size_regression_predictors=v))));keys<-c(keys,'sample_size.result.error_regression_predictors')}
for(d in c('hierarchical','moderation')){
 field<-if(d=='hierarchical')'tested'else'interactions'
 for(v in c('0','-1','NaN','Inf')){errors<-c(errors,list(calc(d,setNames(list(v),paste0('sample_size_regression_',field)))));keys<-c(keys,'sample_size.result.error_regression_tested')}
 for(v in c('0','-1','NaN','Inf')){errors<-c(errors,list(calc(d,list(sample_size_regression_total_predictors=v))));keys<-c(keys,'sample_size.result.error_regression_total_predictors')}
}
before_errors<-serialize(errors,NULL)
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(i in seq_along(errors)){
 stopifnot(identical(errors[[i]]$error,statedu_t(keys[i],'en')))
 expected<-statedu_t(keys[i],lang,fallback='')
 actual<-xml2::xml_text(xml2::xml_find_first(xml2::read_html(as.character(sample_size_results_ui(errors[[i]],lang))),'//div[@class="analysis-warning"]'))
 stopifnot(nzchar(expected),identical(actual,expected));if(lang!='en')stopifnot(actual!=errors[[i]]$error)
 unknown<-paste0(errors[[i]]$error,' custom');stopifnot(identical(sample_size_result_text(unknown,lang),unknown))
}
stopifnot(length(errors)==20L,identical(before_errors,serialize(errors,NULL)))
cases<-expand.grid(design=c('multiple','hierarchical','moderation'),target=c('power','sample_size'),stringsAsFactors=FALSE)
results<-lapply(1:6,function(i)calc(cases$design[i],list(sample_size_regression_target=cases$target[i])))
reference<-function(n)pf(qf(.95,1,n-2),1,n-2,ncp=n*.15,lower.tail=FALSE)
for(i in 1:6){r<-results[[i]];stopifnot(is.null(r$error));if(i<=3)stopifnot(isTRUE(all.equal(r$power,reference(150))))else stopifnot(reference(r$total)>=.8,reference(r$total-1)<.8)}
designs<-paste(cases$design,cases$target,sep='-');formula_keys<-rep('sample_size.result.planning_regression_f2',6)
cat('PASS twenty actual predictor-count errors x eight languages; six independent minimum-count power/sample-size references\n')
