.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
root <- 'output/rm-df-display-20260915'
dir.create(root,recursive=TRUE,showWarnings=FALSE)
compact <- function(x)gsub('[[:space:]]+','',x)
text <- function(x)compact(xml2::xml_text(xml2::read_html(as.character(htmltools::renderTags(x)$html))))
for(value in c('11.875 (1, 294)','1376.059 (1.942, 570.948)','-.596 (2, 100)','0 (1, 2)')) {
  stopifnot(identical(text(result_cell_content(value,column='F(df1,df2)')),compact(value)))
}
stopifnot(identical(text(result_cell_content('11.875 (1, 294)',marker='a',column='F(df1,df2)')),'11.875(1,294)a'))
entries <- lapply(c('covariate','eight_times'),function(name) {
  r <- readRDS(file.path('outputs/spss_phase28_20260906',name,'analysis.rds'))
  before <- serialize(r,NULL)
  pub <- mixed_rm_publication_tables(r)$main
  expected <- pub[['F(df1,df2)']]
  stopifnot(length(expected)>0,all(grepl('[(].+,.+[)]',expected)))
  file <- file.path(root,paste0(name,'.html'))
  write_mixed_rm_anova_results_html(r,file)
  html <- paste(readLines(file,encoding='UTF-8'),collapse='\n')
  panel <- htmltools::renderTags(mixed_rm_anova_results_ui(r))$html
  cells <- function(x)xml2::xml_text(xml2::xml_find_all(xml2::read_html(x),'//th|//td'))
  stopifnot(identical(compact(cells(panel)),compact(cells(html))),identical(before,serialize(r,NULL)))
  for(value in expected)stopifnot(any(compact(cells(html))==compact(value)))
  list(id=name,title=paste('RM ANOVA',name),html=html,saved_at='2026-09-15',expected_df=expected)
})
norm <- function(x)gsub('[[:space:]\u00a0]+','',paste(x,collapse=''),perl=TRUE)
for(mode in c('current','accumulated')) {
  selected <- if(mode=='current')entries[1]else entries
  stem <- file.path(root,mode)
  write_result_collection_html(selected,paste0(stem,'.html'))
  write_result_collection_docx(selected,paste0(stem,'.docx'))
  save_result_collection_excel_file(selected,paste0(stem,'.xlsx'))
  write_result_collection_pdf(selected,paste0(stem,'.pdf'))
  write_result_collection_hwpx(selected,paste0(stem,'.hwpx'))
  word <- norm(xml2::xml_text(xml2::read_xml(unz(paste0(stem,'.docx'),'word/document.xml'))))
  members <- unzip(paste0(stem,'.hwpx'),list=TRUE)$Name
  hw <- norm(vapply(members[grepl('Contents/section[0-9]+[.]xml$',members)],function(s)xml2::xml_text(xml2::read_xml(unz(paste0(stem,'.hwpx'),s))),character(1)))
  wb <- openxlsx::loadWorkbook(paste0(stem,'.xlsx'))
  excel <- norm(unlist(lapply(seq_along(names(wb)),function(i)as.matrix(openxlsx::read.xlsx(wb,sheet=i,colNames=FALSE)))))
  expected <- unlist(lapply(selected,`[[`,'expected_df'))
  for(value in expected)for(actual in list(word,hw,excel))stopifnot(grepl(norm(value),actual,fixed=TRUE))
  jsonlite::write_json(as.list(expected),paste0(stem,'-expected.json'),auto_unbox=TRUE)
}
cat('PASS: exact integer/fractional df and markers; screen/saved cells equal; analysis unchanged; current/accumulated five exports generated with df verified in Word HWPX Excel\n')
