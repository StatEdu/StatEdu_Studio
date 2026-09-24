source('scripts/fixtures_gee_method_note_i18n.R',encoding='UTF-8')
out<-'tmp/gee-correlation-errors-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
errors<-list();keys<-character();slots<-list()
add_error<-function(input,key,args=NULL){errors[[length(errors)+1L]]<<-sample_size_calculate('gee',modifyList(base,input));keys<<-c(keys,paste0('sample_size.result.',key));slots[length(errors)]<<-list(args)}
for(t in c(3,4))add_error(list(sample_size_gee_correlation_structure='unstructured',sample_size_gee_time_points=as.character(t),sample_size_gee_correlations='.2'),'error_gee_pair_count',c(as.character(t*(t-1)/2),as.character(t)))
for(v in c('-.1,0,0','1,0,0'))add_error(list(sample_size_gee_correlation_structure='unstructured',sample_size_gee_correlations=v),'error_gee_unstructured_range')
for(s in c('exchangeable','ar1'))for(v in c('-.1','1','Inf','NaN'))add_error(list(sample_size_gee_correlation_structure=s,sample_size_gee_rho=v),'error_gee_rho_range')
before_errors<-serialize(errors,NULL)
for(lang in c('en','ko','ja','zh','es','fr','de','vi')){
 for(i in seq_along(errors)){
  expected<-statedu_t(keys[i],lang,fallback='');en<-statedu_t(keys[i],'en')
  if(length(slots[[i]])){expected<-do.call(sprintf,c(list(expected),as.list(slots[[i]])));en<-do.call(sprintf,c(list(en),as.list(slots[[i]])))}
  stopifnot(identical(errors[[i]]$error,en),nzchar(expected))
  actual<-xml2::xml_text(xml2::xml_find_first(xml2::read_html(as.character(sample_size_results_ui(errors[[i]],lang))),'//div[@class="analysis-warning"]'))
  stopifnot(identical(actual,expected));if(lang!='en')stopifnot(actual!=en)
 }
 raw<-sprintf(statedu_t('sample_size.result.error_gee_pair_count','en'),'006','004')
 stopifnot(identical(sample_size_result_text(raw,lang),sprintf(statedu_t('sample_size.result.error_gee_pair_count',lang),'006','004')))
 for(unknown in c(paste0(raw,' custom'),sub('006','6.0',raw,fixed=TRUE)))stopifnot(identical(sample_size_result_text(unknown,lang),unknown))
}
stopifnot(length(errors)==12L,identical(before_errors,serialize(errors,NULL)))
for(s in c('exchangeable','ar1','unstructured'))for(v in c(0,.999)){
 actual<-sample_size_gee_design_effect(3,v,s,rep(v,3))
 expected<-if(s=='ar1')1+2*(2*v+v^2)/3 else 1+2*v
 stopifnot(isTRUE(all.equal(actual,expected)))
}
cat('PASS twelve actual GEE input errors x eight languages, exact count slots and six valid boundary design-effect references\n')
