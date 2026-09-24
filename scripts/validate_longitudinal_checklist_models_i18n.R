Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(919)
check_data<-data.frame(id=rep(1:30,each=3),time=rep(1:3,30),x=rnorm(90))
check_data$y<-1+.5*check_data$x+rnorm(90)
check_data$y[c(3,8,16,29)]<-NA
checklist_models<-lapply(c(FALSE,TRUE),function(checks) {
 fit<-prepare_longitudinal_analysis_result(check_data,'y','id','time',predictors='x',model_type='gee',family='gaussian',assumption_checks=checks,missing_strategies=if(checks)'ipw' else character(0))
 stopifnot(length(fit)==1L)
 fit[[1]]$reporting_checklist
})
checklist_models[[3]]<-longitudinal_reporting_checklist(list(software_versions=data.frame(Software='R',Version='4.5.3')))
for(model in c('lmm','panel_fe','panel_re')) {
 fit<-prepare_longitudinal_analysis_result(check_data,'y','id','time',predictors='x',model_type=model,family='gaussian')
 stopifnot(length(fit)==1L)
 checklist_models[[model]]<-fit[[1]]$reporting_checklist
}
set.seed(920)
glmm_data<-data.frame(id=rep(1:40,each=5),time=rep(0:4,40),x=rnorm(200))
glmm_eta<-.2+.3*glmm_data$x+rep(rnorm(40,sd=.7),each=5)
for(distribution in c('binomial','poisson')) {
 glmm_data$y<-if(distribution=='binomial')rbinom(200,1,plogis(glmm_eta)) else rpois(200,exp(glmm_eta))
 fit<-prepare_longitudinal_analysis_result(glmm_data,'y','id','time',predictors='x',model_type='glmm',family=distribution)
 stopifnot(length(fit)==1L,nrow(fit[[1]]$coef_table)>0)
 checklist_models[[paste0('glmm_',distribution)]]<-fit[[1]]$reporting_checklist
}
set.seed(921)
for(distribution in c('gamma','negative_binomial','binomial')) {
 slope<-distribution=='binomial'
 eta<-glmm_eta+if(slope)rep(rnorm(40,sd=.45),each=5)*(glmm_data$time-2) else 0
 glmm_data$y<-switch(distribution,gamma=rgamma(200,shape=3,scale=exp(eta)/3),negative_binomial=rnbinom(200,mu=exp(eta),size=2),binomial=rbinom(200,1,plogis(eta)))
 fit<-prepare_longitudinal_analysis_result(glmm_data,'y','id','time',predictors='x',model_type='glmm',family=distribution,random_slope=slope)
 stopifnot(length(fit)==1L,nrow(fit[[1]]$coef_table)>0)
 checklist_models[[paste0('glmm_extended_',distribution)]]<-fit[[1]]$reporting_checklist
}
before_checklists<-serialize(checklist_models,NULL)
missing_checklist_text<-character(0)
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(k in seq_along(checklist_models)) {
 tab<-checklist_models[[k]];translated<-longitudinal_appendix_table(tab,lang)
 version_row<-which(tab$Item=='Software/package version reported')
 stopifnot(identical(tab$Details[version_row],translated[[3]][version_row]))
 if(lang!='en')for(i in seq_len(nrow(tab)))for(j in 1:3) {
  if(j==3 && i==version_row)next
  if(nzchar(tab[[j]][i]) && identical(tab[[j]][i],translated[[j]][i]))missing_checklist_text<-unique(c(missing_checklist_text,tab[[j]][i]))
 }
 for(entry in attr(tab,'longitudinal_checklist_messages')) {
  row<-which(tab$Item==entry$item)
  parts<-vapply(entry$messages,longitudinal_appendix_text,character(1),language=lang)
  stopifnot(identical(translated[[3]][row],paste(parts,collapse=' ')))
  if(lang!='en' && length(parts))missing_checklist_text<-unique(c(missing_checklist_text,entry$messages[parts==entry$messages]))
 }
 cat('CHECK model checklist:',lang,k,'\n')
}
stopifnot(identical(before_checklists,serialize(checklist_models,NULL)))
if(length(missing_checklist_text))stop(paste(missing_checklist_text,collapse='\n'))
