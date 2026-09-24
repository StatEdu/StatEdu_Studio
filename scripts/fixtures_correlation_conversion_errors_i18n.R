out<-'tmp/correlation-conversion-errors-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
calc<-function(design,value)suppressWarnings(effect_size_correlation_calculate(list(effect_size_correlation_design=design,effect_size_correlation_r=value,effect_size_correlation_t=value,effect_size_correlation_df='98')))
errors<-list();keys<-character()
for(v in c('0','bad','NaN','Inf','-Inf')){errors<-c(errors,list(calc('r_from_t',v)));keys<-c(keys,'sample_size.result.error_correlation_t_finite')}
for(v in c('0','-1','1','bad','NaN','Inf','-Inf')){errors<-c(errors,list(calc('point_biserial',v)));keys<-c(keys,'sample_size.result.error_point_biserial_range')}
before_errors<-serialize(errors,NULL)
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(i in seq_along(errors)){
 stopifnot(identical(errors[[i]]$error,statedu_t(keys[i],'en')))
 expected<-statedu_t(keys[i],lang,fallback='')
 actual<-xml2::xml_text(xml2::xml_find_first(xml2::read_html(as.character(sample_size_results_ui(errors[[i]],lang))),'//div[@class="analysis-warning"]'))
 stopifnot(nzchar(expected),identical(actual,expected));if(lang!='en')stopifnot(actual!=errors[[i]]$error)
 unknown<-paste0(errors[[i]]$error,' custom');stopifnot(identical(sample_size_result_text(unknown,lang),unknown))
}
stopifnot(length(errors)==12L,identical(before_errors,serialize(errors,NULL)))
tresults<-lapply(c(-2.5,2.5),function(v)calc('r_from_t',as.character(v)))
for(i in 1:2){
 r<-tresults[[i]];v<-c(-2.5,2.5)[i]
 stopifnot(is.null(r$error),isTRUE(all.equal(r$correlation_r,v/sqrt(v*v+98))))
 for(lang in c('en','ko','ja','zh','es','fr','de','vi')){
  rendered<-xml2::xml_text(xml2::read_html(as.character(sample_size_results_ui(r,lang))))
  stopifnot(grepl('r = sign(t) * sqrt(t^2 / (t^2 + df))',rendered,fixed=TRUE))
 }
}
results<-lapply(c(-.3,.3),function(v)calc('point_biserial',as.character(v)))
for(i in 1:2){r<-results[[i]];v<-c(-.3,.3)[i];stopifnot(is.null(r$error),r$correlation_r==v,isTRUE(all.equal(r$effect_size_d,2*v/sqrt(1-v*v))))}
designs<-c('point-biserial-negative','point-biserial-positive')
formula_keys<-rep('sample_size.result.note_point_biserial',2)
cat('PASS twelve conversion errors x eight languages; four signed references; t formulas rendered in eight languages\n')
