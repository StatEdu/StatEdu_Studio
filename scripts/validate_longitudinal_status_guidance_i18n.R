Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(927)
d<-data.frame(id=rep(1:40,each=5),time=rep(0:4,40),x=rnorm(200))
d$y<-1+.3*d$x+.1*d$time+rep(rnorm(40),each=5)+rnorm(200)
mi<-longitudinal_mi_sensitivity_results(d,'y','id','time',c('time','x'),'lmm','gaussian','exchangeable')
stopifnot(nrow(mi)==1L,mi$Status=='Not needed')
fit<-longitudinal_fit_model(d,'y','id','time',c('time','x'),'gee','gaussian','unstructured_adjusted')
stopifnot(length(fit$fit_note)==2L,nrow(fit$coef_table)>0)
tables<-list(mi=mi,compatibility=data.frame(Interpretation=fit$fit_note))
out<-'tmp/longitudinal-status-guidance-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
before<-serialize(tables,NULL);missing<-list();captured<-list()
warning<-'optimizer.x: external warning (1.2300e-09); 사용자.y'
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 options(statedu.app_language=lang)
 for(name in names(tables)) {
  original<-tables[[name]];translated<-longitudinal_appendix_table(original,lang)
  for(column in intersect(c('Strategy','Status','Note','Interpretation'),names(original))) {
   values<-original[[column]];actual<-translated[[match(column,names(original))]]
   absent<-which(nzchar(values)&values==actual)
   if(lang!='en' && length(absent))for(i in absent)missing[[length(missing)+1L]]<-data.frame(language=lang,english=values[i])
  }
  if(lang!='en' && name=='mi' && names(translated)[1]=='Strategy')missing[[length(missing)+1L]]<-data.frame(language=lang,english='Strategy')
  for(column in intersect(c('Term','B','SE','Statistic','df','p','LLCI','ULCI'),names(original)))stopifnot(identical(original[[column]],translated[[match(column,names(original))]]))
 }
 title<-longitudinal_appendix_text('R package warnings',lang)
 if(lang!='en' && title=='R package warnings')missing[[length(missing)+1L]]<-data.frame(language=lang,english='R package warnings')
 ui<-tagList(longitudinal_table_section('Missing-data sensitivity results',mi),
  longitudinal_table_section('Model fit details',tables$compatibility),
  longitudinal_table_section('R package warnings',data.frame(Message=warning)),
  longitudinal_table_section('Coefficients',fit$coef_table,role='main'))
 captured[[lang]]<-as.character(ui)
 stopifnot(grepl(warning,xml2::xml_text(xml2::read_html(captured[[lang]])),fixed=TRUE),identical(before,serialize(tables,NULL)))
}
if(length(missing)) {
 missing<-unique(do.call(rbind,missing));write.csv(missing,file.path(out,'missing.csv'),row.names=FALSE,fileEncoding='UTF-8')
 cat(paste(unique(missing$english),collapse='\n'),'\n');stop('Untranslated status guidance',call.=FALSE)
}
jsonlite::write_json(captured,file.path(out,'captured.json'),auto_unbox=TRUE)
cat('PASS complete-data MI and fitted experimental GEE guidance in eight languages; numeric results and external warning preserved\n')
