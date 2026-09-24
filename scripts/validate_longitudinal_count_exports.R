Sys.setlocale('LC_CTYPE','English_United States.utf8')
args<-commandArgs(trailingOnly=TRUE)
if(length(args)) {
 stopifnot(length(args)==2L)
 Sys.setenv(STATEDU_MODULE_CACHE='false')
 source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
 # Reuse actual captured tables without rerunning model fits during export.
 captured_html <- grepl('[.]json$', args[[1L]], ignore.case=TRUE)
 auto_count_tables<-if(captured_html) jsonlite::fromJSON(args[[1L]], simplifyVector=FALSE) else readRDS(args[[1L]])
 out<-args[[2L]]
} else {
 source('scripts/validate_longitudinal_auto_count_i18n.R',encoding='UTF-8')
 out<-'tmp/longitudinal-count-exports'
 captured_html <- FALSE
}
dir.create(out,recursive=TRUE,showWarnings=FALSE)
norm<-function(x)gsub('[[:space:]\u00a0]+','',paste(x,collapse=''),perl=TRUE)
before<-serialize(auto_count_tables,NULL)
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 options(statedu.app_language=lang)
 ui<-if(captured_html) HTML(auto_count_tables[[lang]]) else tagList(lapply(auto_count_tables,function(tab)longitudinal_table_section('Model fit details',tab)),
   longitudinal_table_section('Coefficients',data.frame(Term='User',B=.125,SE=.025),role='main'))
 panel<-xml2::read_html(as.character(ui),encoding='UTF-8')
 main<-xml2::xml_text(xml2::xml_find_all(panel,'//*[contains(@class,"longitudinal-result-panel--main")]'))
 if(lang=='en')baseline<-main else stopifnot(identical(main,baseline))
 if(!lang %in% c('ko','ja'))next
 entry<-list(id=lang,title='Longitudinal',html=as.character(ui))
 for(mode in c('current','accumulated')) {
  entries<-if(mode=='current')list(entry)else list(entry,modifyList(entry,list(id='second')))
  stem<-file.path(out,paste(lang,mode,sep='-'))
  write_result_collection_html(entries,paste0(stem,'.html'))
  write_result_collection_docx(entries,paste0(stem,'.docx'))
  save_result_collection_excel_file(entries,paste0(stem,'.xlsx'))
  write_result_collection_pdf(entries,paste0(stem,'.pdf'))
  write_result_collection_hwpx(entries,paste0(stem,'.hwpx'))
  word<-norm(xml2::xml_text(xml2::read_xml(unz(paste0(stem,'.docx'),'word/document.xml'))))
  members<-unzip(paste0(stem,'.hwpx'),list=TRUE)$Name
  hwpx<-norm(vapply(members[grepl('Contents/section[0-9]+[.]xml$',members)],function(s)xml2::xml_text(xml2::read_xml(unz(paste0(stem,'.hwpx'),s))),character(1)))
  wb<-openxlsx::loadWorkbook(paste0(stem,'.xlsx'))
  excel<-norm(unlist(lapply(seq_along(names(wb)),function(i)as.matrix(openxlsx::read.xlsx(wb,sheet=i,colNames=FALSE)))))
  saved<-norm(xml2::xml_text(xml2::read_html(paste0(stem,'.html'))))
  expected<-xml2::xml_text(xml2::xml_find_all(panel,'//th|//td|//h2|//h3|//h4|//li'))
  for(value in expected[nzchar(trimws(expected))])for(actual in list(word,hwpx,excel,saved))stopifnot(grepl(norm(value),actual,fixed=TRUE))
  stopifnot(identical(before,serialize(auto_count_tables,NULL)),file.info(paste0(stem,'.pdf'))$size>1000)
  jsonlite::write_json(expected,paste0(stem,'-expected.json'))
  cat('PASS count exports:',lang,mode,'\n');flush.console()
 }
}
