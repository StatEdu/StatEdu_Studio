out<-'tmp/sem-parameter-errors-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
base<-list(sample_size_sem_target='power',sample_size_sem_alpha='.05',sample_size_sem_power='.8',sample_size_sem_n='120',sample_size_sem_dropout='0',sample_size_sem_test='parameter',sample_size_sem_simulations='100',sample_size_sem_complexity='moderate')
types<-c('path','loading','correlation');errors<-list();keys<-character()
for(type in types)for(value in c('bad','NaN','Inf','-1','1','1.1')){
 errors<-c(errors,list(suppressWarnings(sample_size_calculate('sem',modifyList(base,list(sample_size_sem_parameter_type=type,sample_size_sem_parameter=value))))))
 keys<-c(keys,paste0('sample_size.result.',if(value%in%c('bad','NaN','Inf'))'error_sem_parameter_numeric'else'error_sem_parameter_range'))
}
before_errors<-serialize(errors,NULL)
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(i in seq_along(errors)){
 stopifnot(identical(errors[[i]]$error,statedu_t(keys[i],'en')))
 expected<-statedu_t(keys[i],lang,fallback='')
 actual<-xml2::xml_text(xml2::xml_find_first(xml2::read_html(as.character(sample_size_results_ui(errors[[i]],lang))),'//div[@class="analysis-warning"]'))
 stopifnot(nzchar(expected),identical(actual,expected));if(lang!='en')stopifnot(actual!=errors[[i]]$error)
 unknown<-paste0(errors[[i]]$error,' custom');stopifnot(identical(sample_size_result_text(unknown,lang),unknown))
}
stopifnot(length(errors)==18L,identical(before_errors,serialize(errors,NULL)))
cases<-expand.grid(type=types,value=c(-.3,.3),stringsAsFactors=FALSE)
set.seed(1809);seed_before<-.Random.seed
results<-lapply(seq_len(nrow(cases)),function(i)sample_size_calculate('sem',modifyList(base,list(sample_size_sem_parameter_type=cases$type[i],sample_size_sem_parameter=as.character(cases$value[i])))))
stopifnot(identical(seed_before,.Random.seed))
for(x in results)stopifnot(is.null(x$error),is.finite(x$power),x$power>=0,x$power<=1)
for(type in types){zero<-sample_size_calculate('sem',modifyList(base,list(sample_size_sem_parameter_type=type,sample_size_sem_parameter='0')));stopifnot(!is.null(zero$error))}
designs<-paste(cases$type,cases$value);formula_keys<-rep('sample_size.result.planning_sem_parameter',6)
cat('PASS 18 actual SEM parameter errors x eight languages; six valid signed parameters, RNG preservation and unchanged zero rejection\n')
