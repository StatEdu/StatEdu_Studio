Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/correlation-plot-errors-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
cases<-list(scatter_requires_continuous=list(data=data.frame(x=1:5),measurements=c(x='continuous')),
 scatter_requires_varying=list(data=data.frame(x=rep(1,5),y=rep(2,5)),measurements=c(x='continuous',y='continuous'),labels=c(x='Yes',y='No')),
 no_matrix_data=list(correlation_matrix=matrix(numeric(),0,0)))
before<-serialize(cases,NULL);captured<-list()
.correlation_seen<-character()
trace('text.default',where=asNamespace('graphics'),tracer=quote(.GlobalEnv$.correlation_seen<-c(.GlobalEnv$.correlation_seen,as.character(labels))),print=FALSE)
tryCatch({
 for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
  options(statedu.app_language=lang);sections<-list()
  for(key in names(cases)) {
   .correlation_seen<-character();path<-file.path(out,paste0(lang,'-',key,'.png'))
   grDevices::png(path,width=1200,height=300,res=120)
   tryCatch({
    graphics::par(family='sans',mar=rep(1,4))
    if(key=='no_matrix_data')draw_correlation_heatmap(cases[[key]])else draw_correlation_scatter_plot(cases[[key]])
   },finally=grDevices::dev.off())
   expected<-statedu_t(paste0('analysis.correlation.',key),lang)
   stopifnot(identical(.correlation_seen,expected),file.info(path)$size>0)
   uri<-paste0('data:image/png;base64,',base64enc::base64encode(path))
   sections[[key]]<-div(h3(key),tags$img(src=uri,style='width:100%;'))
  }
  captured[[lang]]<-as.character(tagList(sections))
  stopifnot(identical(before,serialize(cases,NULL)))
  cat('PASS',lang,'three real graphics error branches; source preserved\n')
 }
},finally=untrace('text.default',where=asNamespace('graphics')))
jsonlite::write_json(captured,file.path(out,'captured.json'),auto_unbox=TRUE)
