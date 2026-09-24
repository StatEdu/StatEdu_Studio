out <- 'tmp/sem-planning-notes-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
base <- list(sample_size_sem_alpha='.05',sample_size_sem_power='.8',sample_size_sem_n='120',sample_size_sem_dropout='0',sample_size_sem_df='20',sample_size_sem_latent_variables='3',sample_size_sem_measured_variables='9',sample_size_sem_structural_paths='3',sample_size_sem_free_parameters='20',sample_size_sem_expected_loading='.7',sample_size_sem_expected_path='.3')
cc<-expand.grid(complexity=c('simple','moderate','complex'),target=c('sample_size','power'),stringsAsFactors=FALSE)
rc<-expand.grid(df_source=c('manual','structure'),test=c('close_fit','not_close_fit'),target=c('sample_size','power'),stringsAsFactors=FALSE)
results<-c(lapply(1:6,function(i)sample_size_calculate('sem',modifyList(base,list(sample_size_sem_test='complexity',sample_size_sem_complexity=cc$complexity[i],sample_size_sem_target=cc$target[i])))),lapply(1:8,function(i)sample_size_calculate('sem',modifyList(base,list(sample_size_sem_df_source=rc$df_source[i],sample_size_sem_test=rc$test[i],sample_size_sem_target=rc$target[i],sample_size_sem_null_rmsea=if(rc$test[i]=='close_fit')'.05' else '.08',sample_size_sem_alternative_rmsea=if(rc$test[i]=='close_fit')'.08' else '.05')))))
designs<-c(paste(cc$complexity,cc$target),paste(rc$df_source,rc$test,rc$target))
formula_keys<-paste0('sample_size.result.',c(rep('planning_sem_complexity',6),rep('planning_sem_rmsea',8)))
for(i in seq_along(results))stopifnot(is.null(results[[i]]$error))
for(i in 1:6)stopifnot(results[[i]]$parameter_rule_n==c(200,300,400)[(i-1)%%3+1])
for(i in 11:14){j<-i-6;df<-if(rc$df_source[j]=='manual')20 else results[[i]]$df;null<-if(rc$test[j]=='close_fit').05 else .08;alt<-if(rc$test[j]=='close_fit').08 else .05;lower<-rc$test[j]=='not_close_fit';crit<-qchisq(if(lower).05 else .95,df,ncp=119*df*null^2);stopifnot(isTRUE(all.equal(results[[i]]$power,pchisq(crit,df,ncp=119*df*alt^2,lower.tail=lower))))}
clean<-function(x)gsub('[[:space:]\u00a0]+','',x,perl=TRUE)
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(i in seq_along(results)) {
 expected<-if(i<=6)sprintf(statedu_t('sample_size.result.note_sem_complexity_count',lang),as.character(c(10,15,20)[(i-1)%%3+1])) else statedu_t(paste0('sample_size.result.note_sem_rmsea_',rc$df_source[i-6]),lang)
 stopifnot(identical(sample_size_result_text(results[[i]]$method_note,lang),expected))
 actual<-xml2::xml_text(xml2::read_html(as.character(sample_size_results_ui(results[[i]],lang))))
 for(part in trimws(strsplit(result_sci_note_text(estimation=expected),';',fixed=TRUE)[[1]]))stopifnot(grepl(clean(sub('[.。]$','',part)),clean(actual),fixed=TRUE))
}
cat('PASS fourteen SEM complexity/RMSEA cases, parameter-rule counts and both noncentral chi-square tails across eight languages\n')
