# Reuse actual LMM results and its previously validated correlation error paths.
source('scripts/fixtures_lmm_correlation_errors_i18n.R',encoding='UTF-8')
out<-'tmp/numeric-vector-errors-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
subjects<-c('Group 1 means','Group 2 means','Unstructured correlations','Unstructured working correlations')
keys<-paste0('sample_size.result.',c('error_vector_group1','error_vector_group2','error_vector_unstructured','error_vector_working'))
errors<-list();error_keys<-character()
capture_error<-function(expr)tryCatch(expr,error=function(e)list(error=conditionMessage(e)))
for(i in seq_along(subjects))for(value in c('','0, bad, .5','0, Inf, .5','0, NaN, .5')){
 errors[[length(errors)+1L]]<-capture_error(sample_size_parse_numeric_vector(value,subjects[i]));error_keys<-c(error_keys,keys[i])
}
# Check that four real calculation paths supply the expected field names.
real<-list(sample_size_calculate('lmm',modifyList(base,list(sample_size_lmm_group1_means='bad'))),sample_size_calculate('lmm',modifyList(base,list(sample_size_lmm_design='two_group_repeated',sample_size_lmm_group2_means='bad'))),capture_error(sample_size_lmm_correlation_matrix(3,.3,'unstructured','bad')),capture_error(sample_size_gee_design_effect(3,.3,'unstructured','bad')))
errors<-c(errors,real);error_keys<-c(error_keys,keys)
before_errors<-serialize(errors,NULL)
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(i in seq_along(errors)){
 stopifnot(identical(errors[[i]]$error,statedu_t(error_keys[i],'en')))
 expected<-statedu_t(error_keys[i],lang,fallback='')
 actual<-xml2::xml_text(xml2::xml_find_first(xml2::read_html(as.character(sample_size_results_ui(errors[[i]],lang))),'//div[@class="analysis-warning"]'))
 stopifnot(nzchar(expected),identical(actual,expected),grepl('0, 0.2, 0.5',actual,fixed=TRUE))
 if(lang!='en')stopifnot(actual!=errors[[i]]$error)
 unknown<-'Custom subject must be a numeric vector, for example: 0, 0.2, 0.5.'
 stopifnot(identical(sample_size_result_text(unknown,lang),unknown))
}
stopifnot(length(errors)==20L,identical(before_errors,serialize(errors,NULL)))
for(value in c('0, 0.2, 0.5','0;0.2;0.5','0 0.2 0.5','0\t0.2\n0.5'))stopifnot(identical(sample_size_parse_numeric_vector(value,'Group 1 means'),c(0,.2,.5)))
stopifnot(identical(sample_size_parse_numeric_vector(c(0,.2,.5),'Group 1 means'),c(0,.2,.5)))
cat('PASS 20 numeric-vector errors x eight languages, four calculation paths, five valid numeric-list forms and unchanged errors\n')
