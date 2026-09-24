.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
root<-'output/km-screen-layout-20260915'
km<-readRDS(file.path(root,'analysis.rds'))
d<-read.csv('scripts/fixtures/survival_validation.csv');d$g<-rep(1:6,length.out=nrow(d))
six<-prepare_km_analysis_result(d,'time','status','g',rate_times='0,100,200,300,400,500',plot_types='survival')
old_env<-new.env(parent=.GlobalEnv);sys.source(file.path(root,'before.R'),old_env)
cells<-function(html)xml2::xml_text(xml2::xml_find_all(xml2::read_html(html),'//th|//td'))
entry<-function(result,name){
 before<-serialize(result,NULL)
 panel<-function(f)htmltools::renderTags(f(result,survival_saved_km_plot_ids(result),language='ko'))$html
 stopifnot(identical(cells(panel(survival_km_results_panel)),cells(panel(old_env$survival_km_results_panel))))
 file<-file.path(root,paste0(name,'.html'));write_survival_results_html(result,file,language='ko')
 html<-paste(readLines(file,encoding='UTF-8'),collapse='\n')
 stopifnot(identical(before,serialize(result,NULL)),identical(cells(panel(survival_km_results_panel)),cells(html)))
 list(title=name,html=html,saved_at='2026-09-15')
}
entries<-list(entry(km,'two-groups'),entry(six,'six-groups'))
norm<-function(x)gsub('[[:space:]\u00a0]+','',paste(x,collapse=''),perl=TRUE)
for(mode in c('current','accumulated')) {
 selected<-if(mode=='current')entries[1]else entries
 expected<-paste(vapply(selected,`[[`,character(1),'html'),collapse='\n')
 doc<-xml2::read_html(expected)
 text<-xml2::xml_text(xml2::xml_find_all(doc,'//th|//td|//h3|//h4|//h5|//p'))
 text<-text[nzchar(trimws(text))]
 stem<-file.path(root,mode)
 write_result_collection_html(selected,paste0(stem,'.html'))
 write_result_collection_docx(selected,paste0(stem,'.docx'))
 save_result_collection_excel_file(selected,paste0(stem,'.xlsx'))
 write_result_collection_pdf(selected,paste0(stem,'.pdf'))
 write_result_collection_hwpx(selected,paste0(stem,'.hwpx'))
 word<-norm(xml2::xml_text(xml2::read_xml(unz(paste0(stem,'.docx'),'word/document.xml'))))
 members<-unzip(paste0(stem,'.hwpx'),list=TRUE)$Name
 hwpx<-norm(vapply(members[grepl('Contents/section[0-9]+[.]xml$',members)],function(s)xml2::xml_text(xml2::read_xml(unz(paste0(stem,'.hwpx'),s))),character(1)))
 workbook<-openxlsx::loadWorkbook(paste0(stem,'.xlsx'))
 excel<-norm(unlist(lapply(seq_along(names(workbook)),function(i)as.matrix(openxlsx::read.xlsx(workbook,sheet=i,colNames=FALSE)))))
 html<-norm(xml2::xml_text(xml2::read_html(paste0(stem,'.html'))))
 for(value in text)for(actual in list(word,hwpx,excel,html))stopifnot(grepl(norm(value),actual,fixed=TRUE))
 image_items<-unlist(lapply(selected,result_entry_images),recursive=FALSE)
 expected_hashes<-vapply(image_items,function(x)digest::digest(file=x$path,algo='sha256'),character(1))
 for(ext in c('docx','hwpx','xlsx')) {
  target<-paste0(stem,'-',ext,'-unpacked');dir.create(target,showWarnings=FALSE)
  unzip(paste0(stem,'.',ext),exdir=target)
  images<-list.files(target,pattern='[.](png|PNG)$',recursive=TRUE,full.names=TRUE)
  hashes<-vapply(images,function(x)digest::digest(file=x,algo='sha256'),character(1))
  stopifnot(all(expected_hashes %in% hashes))
 }
 jsonlite::write_json(list(text=as.list(text),image_count=length(expected_hashes)),paste0(stem,'-expected.json'),auto_unbox=TRUE)
 cat('PASS:',mode,'all text in HTML/Word/HWPX/Excel; original PNG bytes in Word/HWPX/Excel; PDF generated\n')
}
