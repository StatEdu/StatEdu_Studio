Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(918)
name_data<-data.frame(id=1:60,time=1,Warning=rnorm(60),y=rnorm(60))
name_data$y[c(3,9,22,41,55)]<-NA
name_result<-longitudinal_prepare_analysis_weights(name_data,name_data[complete.cases(name_data),],'y','id','time','Warning',weight_type='ipw')
ipw_name_tables<-list(actual=longitudinal_display_weight_summary_table(list(weight_summary=name_result$summary)),
 reserved=longitudinal_ipw_diagnostics(rep(.8,5),rep(1,5),model_terms='Intercept only'),
 fallback=longitudinal_ipw_diagnostics(rep(.8,5),rep(1,5)))
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(key in names(ipw_name_tables)) {
 tab<-ipw_name_tables[[key]];localized<-longitudinal_appendix_table(tab,lang)
 row<-which(tab$Item=='IPW observation model variables')
 if(key!='fallback')stopifnot(identical(localized[[2]][row],tab$Value[row]))
 else if(lang!='en')stopifnot(localized[[2]][row]!=tab$Value[row])
 cat('PASS IPW variable names:',lang,key,'\n')
}
observation_name<-'Warning. 사용자 <&> %s;\n두 번째 줄'
observation_note<-sprintf('Observation model: %s; weights clipped to [%s, %s] and normalized to mean 1. Report these variables and review positivity/weight stability.',observation_name,'0.100','2.300')
ipw_name_tables$description<-data.frame(Variable=observation_name,Details=observation_note)
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 translated<-longitudinal_appendix_table(ipw_name_tables$description,lang)
 stopifnot(identical(translated[[1]],observation_name),grepl(observation_name,translated[[2]],fixed=TRUE),grepl('[0.100, 2.300]',translated[[2]],fixed=TRUE))
 if(lang!='en')stopifnot(!grepl('Observation model:',translated[[2]],fixed=TRUE),!grepl('weights clipped to',translated[[2]],fixed=TRUE))
 else stopifnot(identical(translated[[2]],observation_note))
 cat('PASS IPW observation description:',lang,'\n')
}
