out<-'tmp/regression-r2-errors-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
base<-list(effect_size_regression_r2='.2',effect_size_regression_full_r2='.4',effect_size_regression_reduced_r2='.2',effect_size_regression_delta_r2='.1')
methods<-c('f2_from_r2','hierarchical_f2','moderation_f2','hierarchical_f2');fields<-c('r2','full_r2','delta_r2','reduced_r2')
key_names<-c('error_r2_range','error_full_r2_range','error_delta_r2_range','error_reduced_r2_order');errors<-list();keys<-character()
for(i in 1:4)for(value in if(i==4)c('-.1','.4','.5','NaN')else c('0','1','NaN')){
 input<-modifyList(base,list(effect_size_regression_design=methods[i]));input[[paste0('effect_size_regression_',fields[i])]]<-value
 errors<-c(errors,list(effect_size_regression_calculate(input)));keys<-c(keys,paste0('sample_size.result.',key_names[i]))
}
before_errors<-serialize(errors,NULL)
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(i in seq_along(errors)){
 stopifnot(identical(errors[[i]]$error,statedu_t(keys[i],'en')))
 expected<-statedu_t(keys[i],lang,fallback='')
 actual<-xml2::xml_text(xml2::xml_find_first(xml2::read_html(as.character(sample_size_results_ui(errors[[i]],lang))),'//div[@class="analysis-warning"]'))
 stopifnot(nzchar(expected),identical(actual,expected));if(lang!='en')stopifnot(actual!=errors[[i]]$error)
 unknown<-paste0(errors[[i]]$error,' custom');stopifnot(identical(sample_size_result_text(unknown,lang),unknown))
}
stopifnot(length(errors)==13L,identical(before_errors,serialize(errors,NULL)))
results<-lapply(methods[1:3],function(d)effect_size_regression_calculate(modifyList(base,list(effect_size_regression_design=d))))
results<-c(results,list(effect_size_regression_calculate(modifyList(base,list(effect_size_regression_design='hierarchical_f2',effect_size_regression_reduced_r2='0')))))
for(i in 1:4)stopifnot(is.null(results[[i]]$error),isTRUE(all.equal(results[[i]]$f_squared,c(.2/.8,.2/.6,.1/.9,.4/.6)[i])))
designs<-c(methods[1:3],'reduced-zero')
for(lang in c('en','ko','ja','zh','es','fr','de','vi')){
 actual<-xml2::xml_text(xml2::read_html(as.character(sample_size_results_ui(results[[1]],lang))))
 stopifnot(grepl('R-squared / (1 - R-squared)',actual,fixed=TRUE))
}
# Pure-symbol f2 conversion is unchanged; export the three localized descriptions.
results<-results[2:4];designs<-designs[2:4]
formula_keys<-paste0('sample_size.result.',c('incremental_f2','interaction_f2','incremental_f2'))
cat('PASS thirteen actual R-squared errors x eight languages; four independent f-squared references including reduced R2=0\n')
