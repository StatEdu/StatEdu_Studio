Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/ancova-errors-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
d<-data.frame(y=1:8,group=rep(c('A','B'),4),x=c(2,5,1,4,3,7,6,8))
info<-data.frame(name=names(d),measurement=c('continuous','category','continuous'),var_label=c('Review','사용자 <&> %s','Normality'))
cases<-list(complete=prepare_ancova_results(d[1:3,],'y','group','x',info),
 group=prepare_ancova_results(transform(d,group='A'),'y','group','x',info),
 covariate=prepare_ancova_results(d,'y','group',character(0),info))
expected<-c('ANCOVA requires at least four complete cases.','Grouping variable must have at least two observed levels.','Select at least one covariate.')
for(i in seq_along(cases))stopifnot(nrow(cases[[i]]$skipped)==1L,cases[[i]]$skipped$Message==expected[i])
cases$custom<-cases$complete;cases$custom$skipped$Message<-'Review: 사용자 <&> %s'
no_data<-tryCatch(prepare_ancova_results(NULL,'y','group','x'),error=function(e)list(error=conditionMessage(e)))
stopifnot(no_data$error=='No data frame is available for ANCOVA.')
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 options(statedu.app_language=language)
 for(kind in names(cases)){
  result<-cases[[kind]];doc<-xml2::read_html(as.character(ancova_results_ui(result,info)),encoding='UTF-8')
  cells<-xml2::xml_text(xml2::xml_find_all(doc,'//td'))
  stopifnot('Review'%in%cells,'사용자 <&> %s'%in%cells)
  translated<-ancova_error_ui_text(result$skipped$Message,language)
  stopifnot(translated%in%cells)
  if(kind!='custom'&&language!='en')stopifnot(translated!=result$skipped$Message)
  if(kind=='custom')stopifnot(translated==result$skipped$Message)
  panel<-xml2::xml_find_all(doc,'//div[contains(@class,"result-section")][.//table//td]')
  # Keep only the emitted diagnostic table section; empty overview panels are not exported.
  if(language=='ja')entries[[kind]]<-list(id=kind,title=kind,html=as.character(panel[[length(panel)]]))
 }
 error_doc<-xml2::read_html(as.character(ancova_results_ui(no_data)),encoding='UTF-8')
 stopifnot(xml2::xml_text(xml2::xml_find_first(error_doc,'//div[@class="analysis-error"]'))==ancova_error_ui_text(no_data$error,language))
 if(language!='en')stopifnot(ancova_error_ui_text(no_data$error,language)!=no_data$error)
 stopifnot(ancova_error_ui_text('Normality <&> %s',language)=='Normality <&> %s')
 cat('PASS:',language,'three actual skipped analyses, actual no-data error, raw labels/custom details\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
