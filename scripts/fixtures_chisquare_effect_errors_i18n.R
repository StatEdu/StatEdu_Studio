source('scripts/fixtures_association_method_notes_i18n.R',encoding='UTF-8')
results<-results[2:4];designs<-designs[2:4];formula_keys<-formula_keys[2:4]
out<-'tmp/chisquare-effect-errors-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
base<-list(effect_size_chisquare_design='cohens_w_from_probs',effect_size_chisquare_observed='2,5,3',effect_size_chisquare_expected='1,1,1',effect_size_chisquare_statistic='12',effect_size_chisquare_n='120',effect_size_chisquare_rows='3',effect_size_chisquare_columns='3')
errors<-list();keys<-character()
add_error<-function(changes,key){errors[[length(errors)+1L]]<<-suppressWarnings(effect_size_chisquare_calculate(modifyList(base,changes)));keys<<-c(keys,paste0('sample_size.result.',key))}
add_error(list(effect_size_chisquare_observed='1',effect_size_chisquare_expected='1'),'error_chisquare_lengths')
add_error(list(effect_size_chisquare_observed='1,2'),'error_chisquare_lengths')
for(field in c('effect_size_chisquare_observed','effect_size_chisquare_expected'))for(value in c('-1,1,1','NaN,1,1')){changes<-list();changes[[field]]<-value;add_error(changes,'error_chisquare_proportions')}
add_error(list(effect_size_chisquare_expected='0,1,1'),'error_chisquare_proportions')
for(field in c('effect_size_chisquare_rows','effect_size_chisquare_columns'))for(value in c('1','NaN')){changes<-list(effect_size_chisquare_design='cramers_v');changes[[field]]<-value;add_error(changes,'error_chisquare_dimensions')}
before_errors<-serialize(errors,NULL)
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(i in seq_along(errors)){
 stopifnot(identical(errors[[i]]$error,statedu_t(keys[i],'en')))
 expected<-statedu_t(keys[i],lang,fallback='')
 actual<-xml2::xml_text(xml2::xml_find_first(xml2::read_html(as.character(sample_size_results_ui(errors[[i]],lang))),'//div[@class="analysis-warning"]'))
 stopifnot(nzchar(expected),identical(actual,expected));if(lang!='en')stopifnot(actual!=errors[[i]]$error)
 unknown<-paste0(errors[[i]]$error,' custom');stopifnot(identical(sample_size_result_text(unknown,lang),unknown))
}
stopifnot(length(errors)==11L,identical(before_errors,serialize(errors,NULL)))
zero_observed<-effect_size_chisquare_calculate(modifyList(base,list(effect_size_chisquare_observed='0,1',effect_size_chisquare_expected='1,1')))
stopifnot(is.null(zero_observed$error),zero_observed$cohens_w==1)
v<-effect_size_chisquare_calculate(modifyList(base,list(effect_size_chisquare_design='cramers_v',effect_size_chisquare_rows='2',effect_size_chisquare_columns='2')))
stopifnot(is.null(v$error),isTRUE(all.equal(v$primary_effect_size,sqrt(.1))))
results<-c(results,list(zero_observed));designs<-c(designs,'zero-observed');formula_keys<-c(formula_keys,'sample_size.result.note_w_categories')
cat('PASS eleven actual chi-square errors x eight languages, zero observed category and 2x2 dimension controls\n')
