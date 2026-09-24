Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/longitudinal-hausman-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
set.seed(942);n<-600
d<-data.frame(id=rep(1:100,each=6),time=rep(0:5,100),x1=rnorm(n),x2=rnorm(n))
unit<-rep(rnorm(100),each=6);noise<-rnorm(n)
d$y<-.3*d$time+.3*d$x1+unit+noise
correlated<-d;correlated$x1<-d$x1+2*unit;correlated$y<-.3*d$time+.3*correlated$x1+3*unit+noise
formula<-y~time+x1+x2
without_plm<-longitudinal_check_hausman
environment(without_plm)<-list2env(list(requireNamespace=function(package,...) {
 if(identical(package,'plm'))FALSE else base::requireNamespace(package,...)
}),parent=environment(longitudinal_check_hausman))
cases<-list(
 not_panel=longitudinal_check_hausman(d,formula,'id','time','gee'),
 no_package=without_plm(d,formula,'id','time','panel_re'),
 failed=longitudinal_check_hausman(d,y~missing_column,'id','time','panel_re'),
 fe_not_rejected=longitudinal_check_hausman(d,formula,'id','time','panel_fe'),
 re_not_rejected=longitudinal_check_hausman(d,formula,'id','time','panel_re'),
 fe_rejected=longitudinal_check_hausman(correlated,formula,'id','time','panel_fe'),
 re_rejected=longitudinal_check_hausman(correlated,formula,'id','time','panel_re'),
 fe_design=longitudinal_check_panel_exogeneity('panel_fe'),
 re_design=longitudinal_check_panel_exogeneity('panel_re'))
stopifnot(cases$re_not_rejected$p>=.05,cases$re_rejected$p<.05,
 !cases$fe_rejected$Issue,cases$re_rejected$Issue,
 identical(cases$fe_rejected$p,cases$re_rejected$p),
 identical(cases$fe_rejected$Statistic,cases$re_rejected$Statistic))
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
cat('PASS nine Hausman/design diagnostic branches in eight languages; statistics, p values, issue flags and user identifiers unchanged\n')
