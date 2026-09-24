Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
options(statedu.output_decimal_digits=3L)
out<-'tmp/ancova-diagnostic-headers-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
set.seed(927);d<-data.frame(group=rep(c('Review','사용자 <&> %s'),each=40),x=rnorm(80));d$y<-2*d$x+rnorm(80);d$y[1]<-30;d$x[2]<-NA
info<-data.frame(name=c('group','x','y'),measurement=c('category','continuous','continuous'),var_label=c('집단','공변량','Normality'))
keys<-c('DV','Raw N','Excluded N','Slope p','Slope check','Case','Excluded flagged cases','partial eta2')
for(kind in c('type1','type2','type3')){
 result<-prepare_ancova_results(d,'y','group','x',info,options=list(auto_method='warn',normality_enabled=FALSE,sum_of_squares=kind,influence_sensitivity=TRUE))
 stopifnot(length(result$results)==1L)
 sources<-list(ancova_model_overview_table(result,info),ancova_assumption_review_table(result,info),ancova_influence_review_table(result,info),ancova_influence_sensitivity_review_table(result,info))
 stopifnot(all(vapply(sources,nrow,integer(1))>0L))
 for(language in c('en','ko','ja','zh','es','fr','de','vi')){
  options(statedu.app_language=language)
  for(source in sources){
   localized<-ancova_appendix_table(source,language)
   for(j in seq_along(source)){
    name<-names(source)[j]
    if(language!='en'&&name%in%keys)stopifnot(names(localized)[j]!=name)
    if(name%in%c('DV','Group','Covariates'))stopifnot(identical(as.character(localized[[j]]),as.character(source[[j]])))
    if(is.numeric(source[[j]]))stopifnot(identical(as.character(localized[[j]]),as.character(source[[j]])))
    if(name=='Sum of squares'&&language!='en')stopifnot(!any(as.character(localized[[j]])%in%c('Type I SS','Type II SS','Type III SS')))
   }
  }
  html<-as.character(ancova_results_ui(result,info));doc<-xml2::read_html(html,encoding='UTF-8')
  main<-lapply(xml2::xml_find_all(doc,'//table[@data-result-table-role="main"]'),function(t)xml2::xml_text(xml2::xml_find_all(t,'.//th|.//td')))
  if(language=='en')baseline<-main else stopifnot(identical(main,baseline))
  if(language=='ja'){
   panels<-xml2::xml_find_all(doc,'//div[contains(@class,"ancova-model-overview-panel") or contains(@class,"ancova-assumption-panel") or contains(@class,"ancova-influence-panel") or contains(@class,"ancova-influence-sensitivity-panel")]')
   # Capture the actual affected diagnostics, preserving their layout and notes.
   stopifnot(length(panels)>=3L)
   entries[[kind]]<-list(id=kind,title=kind,html=paste(as.character(panels),collapse='\n'))
  }
  cat('PASS:',kind,language,'actual diagnostic headers, sum-of-squares labels, user values, English main tables\n')
 }
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
