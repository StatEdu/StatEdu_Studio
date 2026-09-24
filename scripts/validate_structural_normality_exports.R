Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
options(statedu.app_language='ja')
out<-Sys.getenv('STATEDU_I18N_EXPORT_FIXTURE','tmp/structural-normality-i18n');entries<-readRDS(file.path(out,'entries.rds'))
normalize<-function(x)gsub('[[:space:]\u00a0]+','',paste(x,collapse=''),perl=TRUE)
expected<-unlist(lapply(entries,function(entry)xml2::xml_text(xml2::xml_find_all(xml2::read_html(entry$html,encoding='UTF-8'),'//th|//td|//h3|//h4|//h5|//h6|//p'))))
failures<-character()
for(mode in c('current','accumulated')){
 selected<-if(mode=='current')entries else c(entries,list(list(id='second',title='追加した結果',html=entries[[1]]$html)))
 stem<-file.path(out,paste0('ja-',mode))
 write_result_collection_html(selected,paste0(stem,'.html'))
 write_result_collection_docx(selected,paste0(stem,'.docx'))
 save_result_collection_excel_file(selected,paste0(stem,'.xlsx'))
 write_result_collection_pdf(selected,paste0(stem,'.pdf'))
 word<-normalize(xml2::xml_text(xml2::read_xml(unz(paste0(stem,'.docx'),'word/document.xml'))))
 workbook<-openxlsx::loadWorkbook(paste0(stem,'.xlsx'))
 excel<-normalize(unlist(lapply(seq_along(names(workbook)),function(i)as.matrix(openxlsx::read.xlsx(workbook,sheet=i,colNames=FALSE)))))
 html<-normalize(xml2::xml_text(xml2::read_html(paste0(stem,'.html'),encoding='UTF-8')))
 for(value in expected[nzchar(trimws(expected))])for(actual in list(word,excel,html)){
  if(!grepl(normalize(value),actual,fixed=TRUE))stop('Missing exported content: ',value)
 }
 jsonlite::write_json(expected,paste0(stem,'-expected.json'),auto_unbox=FALSE)
 cat('PASS:',mode,'HTML/Word/Excel including diagnostic notes; PDF generated\n');flush.console()
 tryCatch({
  write_result_collection_hwpx(selected,paste0(stem,'.hwpx'))
  members<-unzip(paste0(stem,'.hwpx'),list=TRUE)$Name
  hwpx<-normalize(vapply(members[grepl('Contents/section[0-9]+[.]xml$',members)],function(s)xml2::xml_text(xml2::read_xml(unz(paste0(stem,'.hwpx'),s))),character(1)))
  for(value in expected[nzchar(trimws(expected))])stopifnot(grepl(normalize(value),hwpx,fixed=TRUE))
  cat('PASS:',mode,'HWPX including diagnostic notes\n')
 },error=function(e){failures<<-c(failures,paste(mode,conditionMessage(e)));cat('FAIL:',tail(failures,1),'\n');flush.console()})
}
if(length(failures))stop(paste(failures,collapse='\n'))
