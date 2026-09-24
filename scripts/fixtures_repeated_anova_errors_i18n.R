out<-'tmp/repeated-anova-errors-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
base<-list(sample_size_anova_target='power',sample_size_anova_alpha='.05',sample_size_anova_power='.8',sample_size_anova_n='150',sample_size_anova_dropout='0',sample_size_anova_effect='.15',sample_size_anova_groups='3',sample_size_anova_measurements='2',sample_size_anova_correlation='.5',sample_size_anova_epsilon='1',sample_size_anova_effect_test='interaction')
calc<-function(design,extra=list())suppressWarnings(sample_size_calculate('anova',modifyList(modifyList(base,list(sample_size_anova_design=design)),extra)))
errors<-list();keys<-character()
for(d in c('repeated_one_group','mixed_repeated'))for(f in c('epsilon','correlation','measurements')){
 values<-switch(f,epsilon=c('0','1.1','NaN','Inf'),correlation=c('-1','1','NaN','Inf'),measurements=c('0','1','NaN','Inf'))
 key<-switch(f,epsilon='error_anova_epsilon',correlation='error_anova_repeated_correlation',measurements='error_anova_measurements')
 for(v in values){errors<-c(errors,list(calc(d,setNames(list(v),paste0('sample_size_anova_',f)))));keys<-c(keys,paste0('sample_size.result.',key))}
}
before_errors<-serialize(errors,NULL)
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(i in seq_along(errors)){
 stopifnot(identical(errors[[i]]$error,statedu_t(keys[i],'en')))
 expected<-statedu_t(keys[i],lang,fallback='')
 actual<-xml2::xml_text(xml2::xml_find_first(xml2::read_html(as.character(sample_size_results_ui(errors[[i]],lang))),'//div[@class="analysis-warning"]'))
 stopifnot(nzchar(expected),identical(actual,expected));if(lang!='en')stopifnot(actual!=errors[[i]]$error)
 unknown<-paste0(errors[[i]]$error,' custom');stopifnot(identical(sample_size_result_text(unknown,lang),unknown))
}
stopifnot(length(errors)==24L,identical(before_errors,serialize(errors,NULL)))
cases<-expand.grid(design=c('repeated_one_group','mixed_repeated'),rho=c(-.5,0,.5),stringsAsFactors=FALSE)
results<-lapply(1:6,function(i)calc(cases$design[i],list(sample_size_anova_correlation=as.character(cases$rho[i]))))
for(i in 1:6){
 r<-results[[i]];mixed<-cases$design[i]=='mixed_repeated';df1<-if(mixed)2 else 1;df2<-if(mixed)147 else 149
 expected<-pf(qf(.95,df1,df2),df1,df2,ncp=150*2*.15^2/(1-cases$rho[i]),lower.tail=FALSE)
 stopifnot(is.null(r$error),isTRUE(all.equal(r$power,expected)))
}
designs<-paste(cases$design,cases$rho,sep='-');formula_keys<-rep(paste0('sample_size.result.',c('note_plan_repeated','note_plan_mixed')),3)
cat('PASS twenty-four actual repeated ANOVA errors x eight languages; six independent noncentral-F references with two measurements and epsilon=1\n')
