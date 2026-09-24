source('scripts/fixtures_sem_planning_notes_i18n.R',encoding='UTF-8')
out<-'tmp/sem-structure-counts-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
errors<-list();keys<-character()
fields<-c('latent_variables','measured_variables','structural_paths','free_parameters')
key_names<-c('error_sem_latent_count','error_sem_measured_count','error_sem_path_count','error_sem_free_count')
for(test in c('complexity','close_fit'))for(i in seq_along(fields)){
 if(test=='close_fit' && i==4)next
 for(value in c(c('0','2','-1','0')[i],'NaN')){
  input<-modifyList(base,list(sample_size_sem_test=test,sample_size_sem_target='power',sample_size_sem_df_source='structure',sample_size_sem_null_rmsea='.05',sample_size_sem_alternative_rmsea='.08'))
  input[[paste0('sample_size_sem_',fields[i])]]<-value
  errors<-c(errors,list(sample_size_calculate('sem',input)));keys<-c(keys,paste0('sample_size.result.',key_names[i]))
 }
}
before_errors<-serialize(errors,NULL)
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(i in seq_along(errors)){
 stopifnot(identical(errors[[i]]$error,statedu_t(keys[i],'en')))
 expected<-statedu_t(keys[i],lang,fallback='')
 actual<-xml2::xml_text(xml2::xml_find_first(xml2::read_html(as.character(sample_size_results_ui(errors[[i]],lang))),'//div[@class="analysis-warning"]'))
 stopifnot(nzchar(expected),identical(actual,expected));if(lang!='en')stopifnot(actual!=errors[[i]]$error)
 unknown<-paste0(errors[[i]]$error,' custom');stopifnot(identical(sample_size_result_text(unknown,lang),unknown))
}
stopifnot(length(errors)==14L,identical(before_errors,serialize(errors,NULL)))
valid<-sample_size_calculate('sem',modifyList(base,list(sample_size_sem_test='complexity',sample_size_sem_target='sample_size',sample_size_sem_latent_variables='1',sample_size_sem_measured_variables='1',sample_size_sem_structural_paths='0',sample_size_sem_free_parameters='1')))
stopifnot(is.null(valid$error),is.finite(valid$total),valid$total>0)
df<-sample_size_sem_estimated_df(1,4,0)
stopifnot(df$df==2L,df$free_parameters==8L,df$observed_moments==10L)
cat('PASS fourteen actual SEM count errors x eight languages, minimum counts and independent df reference\n')
