Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/mm-beta-correlation';entries<-readRDS(file.path(out,'entries.rds'))
normalize<-function(x)gsub('[[:space:]\u00a0]+','',paste(x,collapse=''),perl=TRUE)
for(mode in c('current','accumulated')){
 selected<-entries
 if(mode=='accumulated')selected<-c(list(list(id='prior',title='Prior',html='<p>Earlier result preserved.</p>')),selected)
 stem<-file.path(out,mode)
 write_result_collection_html(selected,paste0(stem,'.html'));write_result_collection_docx(selected,paste0(stem,'.docx'))
 save_result_collection_excel_file(selected,paste0(stem,'.xlsx'));write_result_collection_pdf(selected,paste0(stem,'.pdf'));write_result_collection_hwpx(selected,paste0(stem,'.hwpx'))
 doc<-xml2::read_html(paste(vapply(selected,`[[`,character(1),'html'),collapse='\n'))
 stopifnot(length(xml2::xml_find_all(doc,"//th[normalize-space(.)='β']"))==3)
 expected<-xml2::xml_text(xml2::xml_find_all(doc,"//th|//td|//h3|//h4|//*[contains(concat(' ',normalize-space(@class),' '),' coefficient-note ')]"));expected<-expected[nzchar(trimws(expected))]
 word<-normalize(xml2::xml_text(xml2::read_xml(unz(paste0(stem,'.docx'),'word/document.xml'))))
 members<-unzip(paste0(stem,'.hwpx'),list=TRUE)$Name
 hwpx<-normalize(vapply(members[grepl('Contents/section[0-9]+[.]xml$',members)],function(s)xml2::xml_text(xml2::read_xml(unz(paste0(stem,'.hwpx'),s))),character(1)))
 workbook<-openxlsx::loadWorkbook(paste0(stem,'.xlsx'));excel<-normalize(unlist(lapply(seq_along(names(workbook)),function(i)as.matrix(openxlsx::read.xlsx(workbook,sheet=i,colNames=FALSE)))))
 html<-normalize(xml2::xml_text(xml2::read_html(paste0(stem,'.html'))))
 for(value in expected)for(actual in list(word,hwpx,excel,html))stopifnot(grepl(normalize(value),actual,fixed=TRUE))
 jsonlite::write_json(as.list(expected),paste0(stem,'-expected.json'),auto_unbox=TRUE)
 cat('PASS',mode,'five formats: beta columns, coefficients, correlations and notes preserved\n')
}
