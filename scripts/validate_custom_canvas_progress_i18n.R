Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
lines<-readLines('R/setup_custom_model_canvas_ui.R',encoding='UTF-8')
starts<-grep('statedu_bootstrap_status_ui(',lines,fixed=TRUE)
expressions<-lapply(starts,function(begin){
 end<-which(seq_along(lines)>begin & grepl('^\\s*\\),\\s*$',lines))[1]
 parse(text=sub(',\\s*$','',paste(lines[begin:end],collapse='\n')))
})
stopifnot(length(expressions)==4L)
job<-list(requested_total=5000L)
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 progress<-list(detail='42% · 2100/5000',percent=42,phase_label='HTMT')
 for(i in seq_along(expressions)){
  html<-as.character(eval(expressions[[i]]));doc<-xml2::read_html(html,encoding='UTF-8');text<-xml2::xml_text(doc)
  stopifnot(grepl('custom_model_canvas_bootstrap_stop',html,fixed=TRUE))
  if(i==1)stopifnot(grepl('5,000',text,fixed=TRUE))
  if(i==2)stopifnot(grepl(progress$detail,text,fixed=TRUE))
  if(language!='en')stopifnot(!grepl('Custom mediation / moderation|Starting worker|Loading results|Rendering results|Stop bootstrap',text))
 }
 cat('PASS:',language,'four actual custom-canvas progress cards; counts, supplied progress and stop ID preserved\n')
}
invisible(parse('R/setup_custom_model_canvas_ui.R'))
