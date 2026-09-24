Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/ancova-method-reasons-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
set.seed(951);d<-data.frame(y=rnorm(60),group=rep(c('Review','사용자 <&> %s'),30),x=rnorm(60))
info<-data.frame(name=names(d),measurement=c('continuous','category','continuous'),var_label=c('Normality','집단','공변량'))
base<-prepare_ancova_results(d,'y','group','x',info,options=list(auto_method='warn',normality_enabled=FALSE))
conditions<-list(standard=c(.5,.5,.5),warning=c(.01,.01,.01),interaction=c(.01,.5,.5),ranked=c(.5,.01,.5),robust=c(.5,.5,.01))
for(kind in names(conditions)){
 p<-conditions[[kind]];assumptions<-list(slope_p=p[1],normality_p=p[2],homogeneity_p=p[3]);opts<-list(auto_method=if(kind=='warning')'warn'else'auto')
 method<-ancova_choose_method(assumptions,opts);reason<-ancova_method_reason(method,assumptions,opts)
 result<-base;result$results[[1]]$method<-method;result$results[[1]]$reason<-reason
 raw<-ancova_model_overview_table(result,info)
 for(language in c('en','ko','ja','zh','es','fr','de','vi')){
  options(statedu.app_language=language)
  localized<-ancova_appendix_table(raw,language)
  index<-match('Reason',names(raw));method_index<-match('Analysis',names(raw))
  if(language!='en'){
   stopifnot(localized[[index]]!=reason)
   if(method!='ANCOVA')stopifnot(localized[[method_index]]!=method)
  }
  for(name in c('DV','Group','Covariates','Raw N','Complete N','Excluded N')){
   j<-match(name,names(raw));stopifnot(identical(as.character(localized[[j]]),as.character(raw[[j]])))
  }
  collision<-data.frame(DV=reason,Group=method,Reason='사용자 <&> %s',check.names=FALSE)
  protected<-ancova_appendix_table(collision,language);stopifnot(identical(unname(protected[[1]]),reason),identical(unname(protected[[2]]),method),identical(unname(protected[[3]]),collision$Reason))
  html<-as.character(ancova_model_overview_html_table(raw,table_role='appendix',table_language=language))
  stopifnot(grepl(localized[[index]],xml2::xml_text(xml2::read_html(html,encoding='UTF-8')),fixed=TRUE))
  # Use the unchanged real fitted model to verify that shared catalog additions do not localize main tables.
  doc<-xml2::read_html(as.character(ancova_results_ui(base,info)),encoding='UTF-8')
  main<-lapply(xml2::xml_find_all(doc,'//table[@data-result-table-role="main"]'),function(t)xml2::xml_text(xml2::xml_find_all(t,'.//th|.//td')))
  if(language=='en')baseline<-main else stopifnot(identical(main,baseline))
  if(language=='ja')entries[[kind]]<-list(id=kind,title=kind,html=html)
  cat('PASS:',kind,language,'actual method selector/reason, overview rendering, raw names, English main cells\n')
 }
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
