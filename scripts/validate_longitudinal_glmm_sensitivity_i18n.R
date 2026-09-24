Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(926)
d<-data.frame(id=rep(1:50,each=5),time=rep(0:4,50),x=rnorm(250))
eta<-.3+.25*d$x+rep(rnorm(50,sd=.7),each=5)+rep(rnorm(50,sd=.2),each=5)*(d$time-2)
outcomes<-list(binomial=rbinom(250,1,plogis(eta)),poisson=rpois(250,exp(eta)),
 negative_binomial=rnbinom(250,mu=exp(eta),size=2),gamma=rgamma(250,shape=3,scale=exp(eta)/3))
tables<-list();missing<-character()
for(family in names(outcomes))for(slope in c(FALSE,TRUE)) {
 d$y<-outcomes[[family]]
 tab<-longitudinal_sensitivity_analysis_results(d,'y','id','time',c('time','x'),'glmm',family,'exchangeable',random_slope=slope)
 stopifnot(nrow(tab)==2L,all(tab$Status %in% c('Fitted','Fitted (selected)')),
  sum(tab$Status=='Fitted (selected)')==1L,
  tab$Comparison[tab$Status=='Fitted (selected)']==if(slope)'Random intercept + random slope for time' else 'Random intercept only')
 before<-serialize(tab,NULL)
 for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
  translated<-longitudinal_appendix_table(tab,lang)
  for(j in match(c('Analysis','Comparison','Status','Note'),names(tab)))for(i in 1:2) {
   if(lang!='en' && identical(tab[[j]][i],translated[[j]][i]))missing<-unique(c(missing,paste(lang,tab[[j]][i])))
  }
  stopifnot(identical(tab$Metric,translated[[match('Metric',names(tab))]]),
   identical(tab$Value,translated[[match('Value',names(tab))]]),identical(before,serialize(tab,NULL)))
 }
 tables[[paste(family,slope)]]<-tab
 cat('CHECK GLMM sensitivity:',family,'selected slope',slope,'\n');flush.console()
}
if(length(missing))stop(paste(missing,collapse='\n'))
dir.create('tmp/longitudinal-glmm-sensitivity',recursive=TRUE,showWarnings=FALSE)
saveRDS(tables,'tmp/longitudinal-glmm-sensitivity/tables.rds')
cat('PASS 64 GLMM sensitivity-table checks\n')
