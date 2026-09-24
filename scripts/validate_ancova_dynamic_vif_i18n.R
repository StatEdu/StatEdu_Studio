Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
options(statedu.output_decimal_digits=3L)
out<-'tmp/ancova-dynamic-vif-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
set.seed(936);group<-rep(c('Review','사용자 <&> %s'),each=60)
x<-as.numeric(scale(residuals(lm(rnorm(120)~group))))
e<-as.numeric(scale(residuals(lm(rnorm(120)~x+group))))
for(kind in c('acceptable','moderate','high')){
 rho<-switch(kind,acceptable=.2,moderate=.92,high=.98)
 d<-data.frame(group,x,z=rho*x+sqrt(1-rho^2)*e);d$y<-2*x+rnorm(120);d$y[1]<-40
 info<-data.frame(name=names(d),measurement=c('category','continuous','continuous','continuous'),var_label=c('집단','공변량','보조','Normality'))
 result<-prepare_ancova_results(d,'y','group',c('x','z'),info,options=list(auto_method='warn',normality_enabled=FALSE))
 raw<-ancova_assumption_review_table(result,info)
 prefix<-switch(kind,acceptable='Acceptable',moderate='Moderate collinearity',high='High collinearity')
 stopifnot(startsWith(raw$Collinearity,prefix),startsWith(raw$Influence,'Flagged cases='))
 number<-sub('.*VIF=([^)]*)\\)$','\\1',raw$Collinearity)
 for(language in c('en','ko','ja','zh','es','fr','de','vi')){
  options(statedu.app_language=language)
  localized<-ancova_appendix_table(raw,language)
  j<-match('Collinearity',names(raw));k<-match('Influence',names(raw))
  stopifnot(grepl(number,localized[[j]],fixed=TRUE))
  nums<-function(s)regmatches(s,gregexpr('[0-9]+(?:[.][0-9]+)?|[.][0-9]+',s,perl=TRUE))[[1]]
  stopifnot(identical(nums(raw$Influence),nums(localized[[k]])))
  if(language!='en')stopifnot(localized[[j]]!=raw$Collinearity,localized[[k]]!=raw$Influence)
  # Dictionary-like user identifiers are restored even when they resemble complete diagnostics.
  custom<-data.frame(DV=raw$Collinearity,Group='사용자 <&> %s',check.names=FALSE)
  protected<-ancova_appendix_table(custom,language);stopifnot(identical(unname(protected[[1]]),custom$DV),identical(unname(protected[[2]]),custom$Group))
  html<-as.character(ancova_results_ui(result,info));doc<-xml2::read_html(html,encoding='UTF-8')
  main<-lapply(xml2::xml_find_all(doc,'//table[@data-result-table-role="main"]'),function(t)xml2::xml_text(xml2::xml_find_all(t,'.//th|.//td')))
  if(language=='en')baseline<-main else stopifnot(identical(main,baseline))
  panel<-xml2::xml_find_all(doc,'//div[contains(@class,"ancova-assumption-panel")]');stopifnot(length(panel)==1L,grepl(localized[[j]],xml2::xml_text(panel),fixed=TRUE))
  if(language=='ja')entries[[kind]]<-list(id=kind,title=kind,html=as.character(panel))
  cat('PASS:',kind,language,'actual VIF and Cook diagnostics, exact numbers, protected identifiers, English main tables\n')
 }
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
