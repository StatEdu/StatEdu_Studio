source('scripts/fixtures_rank_matched_notes_i18n.R',encoding='UTF-8')
selected<-c(3:5,7:8);results<-results[selected];designs<-designs[selected];formula_keys<-formula_keys[selected]
out<-'tmp/mcnemar-effect-errors-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
errors<-list();keys<-character()
for(field in c('b','c'))for(value in c('-1','NaN','Inf')){
 input<-list(effect_size_mcnemar_design='matched_or_counts',effect_size_mcnemar_b='5',effect_size_mcnemar_c='10');input[[paste0('effect_size_mcnemar_',field)]]<-value
 errors<-c(errors,list(effect_size_mcnemar_calculate(input)));keys<-c(keys,paste0('sample_size.result.error_mcnemar_',field))
}
errors<-c(errors,list(effect_size_mcnemar_calculate(list(effect_size_mcnemar_design='matched_or_counts',effect_size_mcnemar_b='0',effect_size_mcnemar_c='0'))));keys<-c(keys,'sample_size.result.error_mcnemar_no_pairs')
for(design in c('matched_or_probs','cohen_g')){
 errors<-c(errors,list(effect_size_mcnemar_calculate(list(effect_size_mcnemar_design=design,effect_size_mcnemar_p01='.75',effect_size_mcnemar_p10='.5'))));keys<-c(keys,'sample_size.result.error_mcnemar_probability_sum')
}
before_errors<-serialize(errors,NULL)
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(i in seq_along(errors)){
 stopifnot(identical(errors[[i]]$error,statedu_t(keys[i],'en')))
 expected<-statedu_t(keys[i],lang,fallback='')
 actual<-xml2::xml_text(xml2::xml_find_first(xml2::read_html(as.character(sample_size_results_ui(errors[[i]],lang))),'//div[@class="analysis-warning"]'))
 stopifnot(nzchar(expected),identical(actual,expected));if(lang!='en')stopifnot(actual!=errors[[i]]$error)
 unknown<-paste0(errors[[i]]$error,' custom');stopifnot(identical(sample_size_result_text(unknown,lang),unknown))
}
stopifnot(length(errors)==9L,identical(before_errors,serialize(errors,NULL)))
for(design in c('matched_or_probs','cohen_g')){
 x<-effect_size_mcnemar_calculate(list(effect_size_mcnemar_design=design,effect_size_mcnemar_p01='.75',effect_size_mcnemar_p10='.25'))
 stopifnot(is.null(x$error),isTRUE(all.equal(x$primary_effect_size,if(design=='cohen_g').25 else 3)))
}
cat('PASS nine actual matched-pair errors x eight languages, sum=1 accepted and existing zero-cell correction references\n')
