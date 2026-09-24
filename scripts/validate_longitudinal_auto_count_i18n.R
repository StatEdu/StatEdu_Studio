Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(922)
auto_data<-data.frame(id=rep(1:60,each=5),time=rep(0:4,60),x=rnorm(300))
auto_count_tables<-list()
for(distribution in c('poisson','negative_binomial')) {
 auto_data$y<-if(distribution=='poisson')rpois(300,exp(.6+.2*auto_data$x)) else rnbinom(300,mu=exp(1+.2*auto_data$x),size=.5)
 stopifnot(longitudinal_auto_family(auto_data,'y')=='count')
 fit<-prepare_longitudinal_analysis_result(auto_data,'y','id','time',predictors='x',model_type='gee',family='auto')
 stopifnot(length(fit)==1L,nrow(fit[[1]]$coef_table)>0,fit[[1]]$family==distribution)
 auto_count_tables[[distribution]]<-fit[[1]]$fit_details
}
select_count<-function(d)longitudinal_count_family_selection(d,y~time+x,'gee','id','time')
bad_data<-auto_data;bad_data$y[1]<--1
failed_poisson<-select_count(bad_data)
stopifnot(failed_poisson$family=='poisson',startsWith(failed_poisson$note,'Poisson screening failed'))
auto_count_tables$poisson_failure<-failed_poisson$details
no_dispersion_env<-new.env(parent=environment(longitudinal_count_family_selection))
no_dispersion_env$longitudinal_count_dispersion_ratio<-function(...)NA_real_
no_dispersion_fn<-longitudinal_count_family_selection;environment(no_dispersion_fn)<-no_dispersion_env
no_dispersion<-no_dispersion_fn(auto_data,y~time+x,'gee','id','time')
stopifnot(no_dispersion$family=='poisson',startsWith(no_dispersion$note,'Poisson dispersion could not'))
auto_count_tables$dispersion_failure<-no_dispersion$details
nb_failure_env<-new.env(parent=environment(longitudinal_count_family_selection))
nb_failure_env$longitudinal_fit_count_screen_model<-function(data,formula,model_type,family,...) {
 if(family=='negative_binomial')return(simpleError('Injected negative binomial failure'))
 longitudinal_fit_count_screen_model(data,formula,model_type,family,...)
}
nb_failure_fn<-longitudinal_count_family_selection;environment(nb_failure_fn)<-nb_failure_env
nb_failure<-nb_failure_fn(auto_data,y~time+x,'gee','id','time')
stopifnot(nb_failure$family=='poisson',grepl('negative binomial screening did not fit',nb_failure$note,fixed=TRUE))
auto_count_tables$nb_failure<-nb_failure$details
set.seed(923)
zero_data<-auto_data;zero_data$y<-rpois(300,10)*rbinom(300,1,.3)
zero_result<-select_count(zero_data)
stopifnot(startsWith(zero_result$details$Value[zero_result$details$Item=='Zero-inflation screen'],'Possible excess zeros'))
auto_count_tables$excess_zeros<-zero_result$details
untranslated_count<-character(0)
before_auto<-serialize(auto_count_tables,NULL)
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(key in names(auto_count_tables)) {
 original<-auto_count_tables[[key]];translated<-longitudinal_appendix_table(original,lang)
 for(i in seq_len(nrow(original))) {
  if(lang!='en' && identical(original$Item[i],translated[[1]][i]) && !original$Item[i] %in% c('AIC','BIC','N','logLik'))untranslated_count<-unique(c(untranslated_count,original$Item[i]))
  value<-original$Value[i]
  numeric_value<-!nzchar(value)||grepl('^[+−-]?[0-9.]+([eE][+-]?[0-9]+)?$',value)
  if(numeric_value)stopifnot(identical(value,translated[[2]][i]))
  else if(lang!='en' && identical(value,translated[[2]][i]))untranslated_count<-unique(c(untranslated_count,value))
 }
 cat('CHECK automatic count:',lang,key,'\n')
}
stopifnot(identical(before_auto,serialize(auto_count_tables,NULL)))
if(length(untranslated_count))stop(paste(untranslated_count,collapse='\n'))
