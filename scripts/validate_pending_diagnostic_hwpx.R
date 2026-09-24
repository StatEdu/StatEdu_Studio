Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
options(statedu.app_language='ja')
normalize<-function(x)gsub('[[:space:]\u00a0]+','',paste(x,collapse=''),perl=TRUE)
failures<-character()
for(fixture in c('structural-normality-i18n','structural-risk-i18n','structural-missing-i18n','complex-custom-i18n')){
 out<-file.path('tmp',fixture);entries<-readRDS(file.path(out,'entries.rds'))
 for(mode in c('current','accumulated'))tryCatch({
  selected<-if(mode=='current')entries else c(entries,list(list(id='second',title='追加した結果',html=entries[[1]]$html)))
  stem<-file.path(out,paste0('ja-',mode))
  write_result_collection_hwpx(selected,paste0(stem,'.hwpx'))
  members<-unzip(paste0(stem,'.hwpx'),list=TRUE)$Name
  hwpx<-normalize(vapply(members[grepl('Contents/section[0-9]+[.]xml$',members)],function(s)xml2::xml_text(xml2::read_xml(unz(paste0(stem,'.hwpx'),s))),character(1)))
  # Compare both captured entries and the previously validated Word document.
  expected<-unlist(lapply(selected,function(entry)xml2::xml_text(xml2::xml_find_all(xml2::read_html(entry$html,encoding='UTF-8'),'//th|//td|//h3|//h4|//h5|//p'))))
  word<-xml2::read_xml(unz(paste0(stem,'.docx'),'word/document.xml'))
  expected<-c(expected,xml2::xml_text(xml2::xml_find_all(word,'//w:p')))
  for(value in expected[nzchar(trimws(expected))])if(!grepl(normalize(value),hwpx,fixed=TRUE))stop('Missing HWPX content: ',value)
  cat('PASS:',fixture,mode,'HWPX snapshot and existing Word content\n');flush.console()
 },error=function(e){failures<<-c(failures,paste(fixture,mode,conditionMessage(e)));cat('FAIL:',tail(failures,1),'\n');flush.console()})
}
if(length(failures))stop(paste(failures,collapse='\n'))
