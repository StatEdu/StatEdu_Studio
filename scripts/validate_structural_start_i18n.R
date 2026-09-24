Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
lines<-readLines('R/setup_custom_model_canvas_structural_handlers.R',encoding='UTF-8')
locations<-grep('%s resamples; base-model results are available now.',lines,fixed=TRUE)
stopifnot(length(locations)==2L)
expressions<-lapply(locations,function(i){
 begin<-max(which(seq_along(lines)<i & grepl('statedu_bootstrap_status_ui(',lines,fixed=TRUE)))
 end<-which(seq_along(lines)>i & grepl('^\\s*\\),\\s*$',lines))[1]
 parse(text=sub(',\\s*$','',paste(lines[begin:end],collapse='\n')))
})
failure<-lines[grep('paste0(structural_canvas_reporting_text("The PLS/PLSc bootstrap could not start.',lines,fixed=TRUE)]
stopifnot(length(failure)==1L)
failure<-parse(text=sub(',\\s*$','',failure))
prefix<-'test';nboot<-5000L;job<-list(reps=7000L);error_text<-'사용자 <&> Review %s engine error'
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 app_language_fn<-function()language
 for(i in seq_along(expressions)){
  html<-as.character(eval(expressions[[i]]));doc<-xml2::read_html(html,encoding='UTF-8');text<-xml2::xml_text(doc)
  stopifnot(grepl(if(i==1)'5,000' else '7,000',text,fixed=TRUE))
  stopifnot(grepl(if(i==1)'test_pls_bootstrap_stop' else 'test_effect_bootstrap_stop',html,fixed=TRUE))
  stopifnot(grepl(structural_canvas_reporting_text('Starting',language),text,fixed=TRUE),grepl(structural_canvas_reporting_text('Stop bootstrap',language),text,fixed=TRUE))
  if(language!='en')stopifnot(!grepl('base-model results|bootstrap progress|Stop bootstrap|Starting',text))
 }
 message<-eval(failure);stopifnot(endsWith(message,error_text))
 if(language!='en')stopifnot(!startsWith(message,'The PLS/PLSc'))
 cat('PASS:',language,'actual initial PLS/SEM status cards and start-failure error preservation\n')
}
