source('scripts/fixtures_clinical_planning_i18n.R',encoding='UTF-8')
selected<-c(2:5,10:13);results<-results[selected];designs<-designs[selected];formula_keys<-formula_keys[selected]
out<-'tmp/margin-validation-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
errors<-list();keys<-character()
for(outcome in c('mean','proportion'))for(objective in c('equivalence','noninferiority'))for(difference in if(objective=='equivalence')c(-.5,-.25,.25,.5)else c(-.5,-.25)){
 input<-modifyList(eq,list(sample_size_equivalence_outcome=outcome,sample_size_equivalence_objective=objective,sample_size_equivalence_margin='.25',sample_size_equivalence_difference=as.character(difference),sample_size_equivalence_p1=as.character(.5+difference/2),sample_size_equivalence_p2=as.character(.5-difference/2)))
 errors<-c(errors,list(sample_size_calculate('equivalence',input)));keys<-c(keys,paste0('sample_size.result.',if(objective=='equivalence')'error_equivalence_boundary'else'error_noninferiority_boundary'))
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
# Effect-size calculator intentionally reports boundary distance, rather than rejecting it.
for(objective in c('equivalence','noninferiority')){
 x<-effect_size_equivalence_calculate(list(effect_size_equivalence_outcome='mean',effect_size_equivalence_objective=objective,effect_size_equivalence_margin='.25',effect_size_equivalence_difference='-.25',effect_size_equivalence_sd='1'))
 stopifnot(is.null(x$error),x$distance_to_margin==0,x$inside_margin=='No')
}
cat('PASS twelve actual margin-boundary errors x eight languages, exact positive/negative boundaries and effect-calculator reporting preserved\n')
