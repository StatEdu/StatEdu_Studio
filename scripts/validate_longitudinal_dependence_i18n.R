Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/longitudinal-dependence-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
set.seed(725)
d<-data.frame(id=rep(1:30,each=20),time=rep(1:20,30),x=rnorm(600))
noise<-rnorm(600);d$y<-.5*d$x+noise
serial<-d;serial$y<-.5*d$x+unlist(lapply(split(noise,d$id),function(e)as.numeric(filter(e,.9,method='recursive'))))
cross<-d;cross$y<-.5*d$x+rep(5*sin(1:20),30)+noise
fit<-function(data)plm::plm(y~x,data=data,index=c('id','time'),model='within')
independent_fit<-fit(d);serial_fit<-fit(serial);cross_fit<-fit(cross)
pooled_fit<-plm::plm(y~x,data=d,index=c('id','time'),model='pooling')
without_plm<-longitudinal_check_cross_section_dependence
environment(without_plm)<-list2env(list(requireNamespace=function(package,...) {
 if(identical(package,'plm'))FALSE else base::requireNamespace(package,...)
}),parent=environment(longitudinal_check_cross_section_dependence))
cases<-list(
 serial_short=longitudinal_check_serial_correlation(NULL,'gee',d[1:2,],noise[1:2],'id','time'),
 serial_low=longitudinal_check_serial_correlation(NULL,'gee',d,noise,'id','time'),
 serial_high=longitudinal_check_serial_correlation(NULL,'gee',serial,serial$y-.5*serial$x,'id','time'),
 panel_low=longitudinal_check_serial_correlation(pooled_fit,'panel_fe',d,residuals(pooled_fit),'id','time'),
 panel_high=longitudinal_check_serial_correlation(serial_fit,'panel_fe',serial,residuals(serial_fit),'id','time'),
 cross_not_panel=longitudinal_check_cross_section_dependence(NULL,'gee'),
 cross_no_package=without_plm(NULL,'panel_fe'),
 cross_failed=longitudinal_check_cross_section_dependence(NULL,'panel_fe'),
 cross_low=longitudinal_check_cross_section_dependence(independent_fit,'panel_fe'),
 cross_high=longitudinal_check_cross_section_dependence(cross_fit,'panel_fe'))
stopifnot(!cases$serial_low$Issue,cases$serial_high$Issue,!cases$panel_low$Issue,
 cases$panel_high$Issue,!cases$cross_low$Issue,cases$cross_high$Issue)
before<-serialize(cases,NULL);missing<-list()
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(name in names(cases)) {
 original<-cases[[name]];translated<-longitudinal_appendix_table(original,lang)
 for(column in c('Check','Result','Interpretation','Recommendation')) {
  stopifnot(column %in% names(original));value<-original[[column]]
  if(lang!='en' && nzchar(value) && identical(value,unname(translated[[match(column,names(original))]])))
   missing[[length(missing)+1L]]<-data.frame(language=lang,case=name,column,english=value)
 }
 for(column in c('Statistic','p','Issue'))stopifnot(identical(original[[column]],translated[[match(column,names(original))]]))
 probe<-data.frame(Variable=original$Interpretation,Details=original$Interpretation)
 stopifnot(identical(longitudinal_appendix_table(probe,lang)[[1]],probe$Variable),identical(before,serialize(cases,NULL)))
}
if(length(missing)) {
 missing<-do.call(rbind,missing);write.csv(missing,file.path(out,'missing.csv'),row.names=FALSE,fileEncoding='UTF-8')
 cat(paste(unique(missing$english),collapse='\n'),'\n');stop('Untranslated diagnostic messages',call.=FALSE)
}
saveRDS(cases,file.path(out,'tables.rds'))
cat('PASS ten dependence diagnostic branches in eight languages; statistics, p values, issue flags and user identifiers unchanged\n')



