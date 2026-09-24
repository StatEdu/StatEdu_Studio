source('scripts/fixtures_sem_planning_notes_i18n.R',encoding='UTF-8')
results<-results[7:14];designs<-designs[7:14];formula_keys<-formula_keys[7:14]
out<-'tmp/sem-rmsea-errors-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
input<-modifyList(base,list(sample_size_sem_test='close_fit',sample_size_sem_df_source='manual',sample_size_sem_target='power',sample_size_sem_null_rmsea='.05',sample_size_sem_alternative_rmsea='.08'))
errors<-list();keys<-character()
add_error<-function(changes,key){errors[[length(errors)+1L]]<<-suppressWarnings(sample_size_calculate('sem',modifyList(input,changes)));keys<<-c(keys,paste0('sample_size.result.',key))}
for(df in c('0','-1','NaN'))add_error(list(sample_size_sem_df=df),'error_sem_df')
add_error(list(sample_size_sem_df_source='structure',sample_size_sem_measured_variables='3'),'error_sem_estimated_df')
for(test in c('close_fit','not_close_fit'))for(value in if(test=='close_fit')c('.05','.04')else c('.05','.08'))add_error(list(sample_size_sem_test=test,sample_size_sem_alternative_rmsea=value),if(test=='close_fit')'error_rmsea_close'else'error_rmsea_not_close')
before_errors<-serialize(errors,NULL)
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(i in seq_along(errors)){
 stopifnot(identical(errors[[i]]$error,statedu_t(keys[i],'en')))
 expected<-statedu_t(keys[i],lang,fallback='')
 actual<-xml2::xml_text(xml2::xml_find_first(xml2::read_html(as.character(sample_size_results_ui(errors[[i]],lang))),'//div[@class="analysis-warning"]'))
 stopifnot(nzchar(expected),identical(actual,expected));if(lang!='en')stopifnot(actual!=errors[[i]]$error)
 unknown<-paste0(errors[[i]]$error,' custom');stopifnot(identical(sample_size_result_text(unknown,lang),unknown))
}
stopifnot(length(errors)==8L,identical(before_errors,serialize(errors,NULL)))
for(test in c('close_fit','not_close_fit')){
 alt<-if(test=='close_fit').08 else .03
 valid<-sample_size_calculate('sem',modifyList(input,list(sample_size_sem_test=test,sample_size_sem_df='1',sample_size_sem_alternative_rmsea=as.character(alt))))
 crit<-qchisq(if(test=='close_fit').95 else .05,1,ncp=119*.05^2)
 reference<-pchisq(crit,1,ncp=119*alt^2,lower.tail=test=='not_close_fit')
 stopifnot(is.null(valid$error),isTRUE(all.equal(valid$power,reference)))
}
cat('PASS eight real RMSEA/df errors x eight languages and two df=1 tail-probability references\n')
