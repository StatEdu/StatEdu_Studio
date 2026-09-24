Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
source_lines<-readLines('R/setup_custom_model_canvas_structural_identification_diagnostics.R',encoding='UTF-8')
static<-source_lines[grepl('add\\("(Error|Warning)",.*, "[a-z_]+", "',source_lines)]
codes<-sub('^.*, "([a-z_]+)", "[^"]*".*$','\\1',static)
messages<-sub('^.*, "[a-z_]+", "([^"]*)".*$','\\1',static)
stopifnot(length(codes)==13L,length(unique(codes))==13L)
nodes<-list(list(id='a',role='latent',name='Review 사용자'),list(id='b',role='latent',name='B'))
cycle<-structural_canvas_identification_diagnostics(list(nodes=nodes,edges=list(list(id='ab',from='a',to='b'),list(id='ba',from='b',to='a'))))
duplicate<-structural_canvas_identification_diagnostics(list(nodes=nodes,edges=list(list(id='ab',from='a',to='b',kind='covariance'),list(id='ba',from='b',to='a',kind='covariance'))))
stopifnot('structural_cycle' %in% cycle$Code,'duplicate_covariance' %in% duplicate$Code)
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 translated<-mapply(structural_canvas_identification_issue_message,codes,messages,MoreArgs=list(language=language),USE.NAMES=FALSE)
 if(language=='en')stopifnot(identical(translated,messages)) else stopifnot(all(translated!=messages))
 for(issues in list(cycle,duplicate)){
  text<-structural_canvas_identification_issue_text(issues,language)
  if(language!='en')stopifnot(!grepl('The current automatic identification scheme|Duplicate covariance paths',text))
 }
 cat('PASS:',language,'all 13 static diagnostic codes and actual cycle/duplicate-covariance diagnostics\n')
}
