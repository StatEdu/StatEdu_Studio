Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(937);d<-as.data.frame(matrix(rnorm(40*13),40,13));names(d)<-paste0('user',1:13)
r<-list(data=d,measurements=setNames(rep('continuous',13),names(d)),labels=setNames(paste0('사용자 ',1:13),names(d)))
out<-'tmp/correlation-plot-limit-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
before<-serialize(r,NULL);captured<-list();.correlation_limit_seen<-character()
trace('mtext',where=asNamespace('graphics'),tracer=quote(.GlobalEnv$.correlation_limit_seen<-c(.GlobalEnv$.correlation_limit_seen,as.character(text))),print=FALSE)
tryCatch({
 for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
  options(statedu.app_language=lang)
  for(n in c(12L,13L)) {
   candidate<-r;candidate$measurements<-r$measurements[seq_len(n)]
   .correlation_limit_seen<-character()
   path<-if(n==13L)file.path(out,paste0(lang,'-limit.png'))else tempfile(fileext='.png')
   grDevices::png(path,width=1600,height=1600,res=120)
   tryCatch(draw_correlation_scatter_plot(candidate),finally=grDevices::dev.off())
   if(n==12L)stopifnot(!length(.correlation_limit_seen))else {
    expected<-sprintf(statedu_t('analysis.correlation.scatter_display_limit',lang),12,13)
    stopifnot(identical(.correlation_limit_seen,expected),grepl('12',expected),grepl('13',expected))
    captured[[lang]]<-as.character(tags$img(src=paste0('data:image/png;base64,',base64enc::base64encode(path)),style='width:100%;'))
   }
  }
  stopifnot(identical(before,serialize(r,NULL)))
  cat('PASS',lang,'12-variable boundary and 13-variable display limit; input labels/data preserved\n')
 }
},finally=untrace('mtext',where=asNamespace('graphics')))
jsonlite::write_json(captured,file.path(out,'captured.json'),auto_unbox=TRUE)
