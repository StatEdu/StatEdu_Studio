source('scripts/fixtures_reliability_planning_notes_i18n.R',encoding='UTF-8')
out<-'tmp/reliability-counts-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
methods<-c('alpha','icc','kappa');error_keys<-paste0('sample_size.result.',c('error_reliability_items','error_reliability_raters','error_reliability_categories'))
errors<-list();keys<-character()
for(i in seq_along(methods))for(value in c('1','0','NaN','Inf')){
 input<-modifyList(base,list(sample_size_reliability_design=methods[i]))
 input[[if(i==3)'sample_size_reliability_categories'else'sample_size_reliability_items']]<-value
 errors<-c(errors,list(suppressWarnings(sample_size_calculate('reliability',input))));keys<-c(keys,error_keys[i])
}
before_errors<-serialize(errors,NULL)
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(i in seq_along(errors)){
 stopifnot(identical(errors[[i]]$error,statedu_t(keys[i],'en')))
 expected<-statedu_t(keys[i],lang,fallback='')
 actual<-xml2::xml_text(xml2::xml_find_first(xml2::read_html(as.character(sample_size_results_ui(errors[[i]],lang))),'//div[@class="analysis-warning"]'))
 stopifnot(nzchar(expected),identical(actual,expected));if(lang!='en')stopifnot(actual!=errors[[i]]$error)
 unknown<-paste0(errors[[i]]$error,' custom');stopifnot(identical(sample_size_result_text(unknown,lang),unknown))
}
stopifnot(length(errors)==12L,identical(before_errors,serialize(errors,NULL)))
z<-qnorm(.975);reference<-c(max(3,ceiling(4*(z/(.1/.2))^2+2)),ceiling((z/(.1/(1-.8^2)))^2/2+2),ceiling(z^2*.9*.1/.5^2/.1^2))
controls<-lapply(methods,function(d)sample_size_calculate('reliability',modifyList(base,list(sample_size_reliability_design=d,sample_size_reliability_items='2',sample_size_reliability_categories='2'))))
for(i in 1:3)stopifnot(is.null(controls[[i]]$error),controls[[i]]$total==reference[i],controls[[i]]$adjusted_total==ceiling(reference[i]/.9))
results<-c(results,controls);designs<-c(designs,paste0(methods,'-minimum2'));formula_keys<-c(formula_keys,paste0('sample_size.result.planning_reliability_',methods))
cat('PASS twelve real reliability-count errors x eight languages and three minimum-two sample-size references\n')
