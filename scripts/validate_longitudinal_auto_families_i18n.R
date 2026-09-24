Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(924)
d<-data.frame(id=rep(1:50,each=5),time=rep(0:4,50),x=rnorm(250))
eta<-.2+.3*d$x+rep(rnorm(50,sd=.6),each=5)
outcomes<-list(gaussian=eta+rnorm(250),binomial=rbinom(250,1,plogis(eta)),gamma=rgamma(250,shape=.8,scale=exp(eta)/.8))
auto_family_results<-list();auto_family_panels<-list();missing_text<-character(0)
for(family in names(outcomes))for(model in if(family=='gaussian')'gee' else c('gee','glmm')) {
 d$y<-outcomes[[family]]
 info<-data.frame(name=names(d),measurement=c('category','continuous','continuous',if(family=='binomial')'binary' else 'continuous'))
 stopifnot(longitudinal_auto_family(d,'y',info)==family)
 fit<-prepare_longitudinal_analysis_result(d,'y','id','time',predictors='x',model_type=model,family='auto',variable_info=info)
 stopifnot(length(fit)==1L,fit[[1]]$family==family,nrow(fit[[1]]$coef_table)>0)
 key<-paste(model,family,sep='_');auto_family_results[[key]]<-fit
 snapshot<-serialize(fit,NULL)
 for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
  options(statedu.app_language=lang)
  html<-as.character(longitudinal_results_panel(fit))
  doc<-xml2::read_html(html,encoding='UTF-8')
  main<-xml2::xml_text(xml2::xml_find_all(doc,'//*[contains(@class,"longitudinal-result-panel--main")]'))
  stopifnot(length(main)>0)
  if(lang=='en')baseline<-main else stopifnot(identical(main,baseline))
  for(field in c('reporting_checklist','fit_details')) {
   tab<-fit[[1]][[field]];translated<-longitudinal_appendix_table(tab,lang)
   if(!is.data.frame(tab)||!nrow(tab))next
   for(i in seq_len(nrow(tab)))for(j in seq_len(ncol(tab))) {
    value<-as.character(tab[[j]][i])
    if(field=='reporting_checklist' && j==3 && tab$Item[i]=='Software/package version reported')next
    if(!nzchar(value)||grepl('^[+−-]?[0-9.]+([eE][+-]?[0-9]+)?$',value)) {
     stopifnot(identical(tab[[j]][i],translated[[j]][i]));next
    }
    if(lang=='es' && value=='No') {
     stopifnot(identical(as.character(translated[[j]][i]),statedu_t('analysis.ui.no',lang)));next
    }
    if(lang!='en' && identical(value,as.character(translated[[j]][i])))missing_text<-unique(c(missing_text,paste(lang,value)))
   }
  }
  stopifnot(identical(snapshot,serialize(fit,NULL)))
  auto_family_panels[[paste(key,lang,sep='_')]]<-html
  cat('CHECK automatic family:',key,lang,'\n');flush.console()
 }
}
if(length(missing_text))stop(paste(missing_text,collapse='\n'))
dir.create('tmp/longitudinal-auto-families',recursive=TRUE,showWarnings=FALSE)
saveRDS(auto_family_panels,'tmp/longitudinal-auto-families/panels.rds')
saveRDS(lapply(auto_family_results,function(fit)fit[[1]]$fit_details),'tmp/longitudinal-auto-families/details.rds')
cat('PASS all 40 automatic-family checks\n')
