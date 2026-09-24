out<-'tmp/expected-mean-errors-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
margin<-function(objective,value)suppressWarnings(effect_size_equivalence_calculate(list(effect_size_equivalence_outcome='mean',effect_size_equivalence_objective=objective,effect_size_equivalence_margin='.25',effect_size_equivalence_sd='2',effect_size_equivalence_difference=value)))
precision<-function(value)suppressWarnings(effect_size_precision_calculate(list(effect_size_precision_parameter='mean',effect_size_precision_estimate=value,effect_size_precision_half_width='.5',effect_size_precision_sd='2')))
errors<-list();keys<-character()
for(value in c('bad','NaN','Inf','-Inf')){
 errors<-c(errors,list(margin('equivalence',value),margin('noninferiority',value),precision(value)))
 keys<-c(keys,paste0('sample_size.result.',c(rep('error_expected_difference_numeric',2),'error_expected_mean_numeric')))
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
results<-list();designs<-character();formula_keys<-character()
for(v in c(-.125,0,.125)){
 eq<-margin('equivalence',as.character(v));ni<-margin('noninferiority',as.character(v));pr<-precision(as.character(v))
 stopifnot(is.null(eq$error),is.null(ni$error),is.null(pr$error),isTRUE(all.equal(eq$standardized_distance,(.25-abs(v))/2)),isTRUE(all.equal(ni$standardized_distance,(.25+v)/2)),eq$inside_margin=='Yes',ni$inside_margin=='Yes',pr$standardized_half_width==.25)
 if(v==0)stopifnot(is.na(pr$relative_half_width))else stopifnot(pr$relative_half_width==abs(.5/v))
 results<-c(results,list(eq,ni,pr));designs<-c(designs,paste(c('equivalence','noninferiority','precision'),v,sep='-'))
 formula_keys<-c(formula_keys,paste0('sample_size.result.',c('note_equivalence_inside','note_noninferiority_above','precision_mean')))
}
cat('PASS twelve actual expected-mean/difference errors x eight languages; nine signed/zero references including undefined relative precision at zero\n')
