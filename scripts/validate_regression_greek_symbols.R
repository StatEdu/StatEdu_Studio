.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
statedu_apply_preferences(statedu_initial_preferences())
root <- 'output/regression-greek-20260915'
dir.create(root, recursive=TRUE, showWarnings=FALSE)
set.seed(915)
d <- data.frame(x=rnorm(100), z=rnorm(100), y=rnorm(100))
info <- data.frame(name=names(d), measurement='continuous')
r <- prepare_hierarchical_analysis_results(d,'y','x','z',character(0),variable_info=info,residual_diagnostics=FALSE,auto_method=FALSE)$results
before <- unserialize(serialize(r,NULL))
entries <- lapply(c('standard','wide'), function(style) {
  args <- list(results=r, variable_table=info, output_table_style=style)
  panel <- htmltools::renderTags(do.call(hierarchical_results_panel,args))$html
  file <- file.path(root,paste0(style,'.html'))
  do.call(write_hierarchical_results_html,c(args,list(file=file)))
  html <- paste(readLines(file,encoding='UTF-8'),collapse='\n')
  cells <- function(x) xml2::xml_text(xml2::xml_find_all(xml2::read_html(x),'//th|//td'))
  stopifnot(identical(cells(panel),cells(html)), any(grepl('\u03B2',cells(html),fixed=TRUE)),
            any(grepl('\u0394 R',cells(html),fixed=TRUE)), !any(cells(html)=='beta'),
            !any(grepl('Delta R',cells(html),fixed=TRUE)))
  list(id=style,title=paste('Regression',style),html=html,saved_at='2026-09-15')
})
stopifnot(isTRUE(all.equal(before,r,tolerance=0,check.environment=FALSE)))
for(mode in c('current','accumulated')) {
  selected <- if(mode=='current') entries[1] else entries
  stem <- file.path(root,mode)
  write_result_collection_html(selected,paste0(stem,'.html'))
  write_result_collection_docx(selected,paste0(stem,'.docx'))
  save_result_collection_excel_file(selected,paste0(stem,'.xlsx'))
  write_result_collection_pdf(selected,paste0(stem,'.pdf'))
  write_result_collection_hwpx(selected,paste0(stem,'.hwpx'))
  word <- xml2::xml_text(xml2::read_xml(unz(paste0(stem,'.docx'),'word/document.xml')))
  members <- unzip(paste0(stem,'.hwpx'),list=TRUE)$Name
  hwpx <- paste(vapply(members[grepl('Contents/section[0-9]+[.]xml$',members)],function(s)xml2::xml_text(xml2::read_xml(unz(paste0(stem,'.hwpx'),s))),character(1)),collapse='')
  wb <- openxlsx::loadWorkbook(paste0(stem,'.xlsx'))
  excel <- paste(unlist(lapply(seq_along(names(wb)),function(i)as.matrix(openxlsx::read.xlsx(wb,sheet=i,colNames=FALSE)))),collapse=' ')
  html <- xml2::xml_text(xml2::read_html(paste0(stem,'.html')))
  for(value in list(word,hwpx,excel,html)) stopifnot(grepl('\u03B2',value,fixed=TRUE),grepl('\u0394',value,fixed=TRUE),!grepl('Delta R',value,fixed=TRUE))
}
saveRDS(entries,file.path(root,'entries.rds'))
cat('PASS: standard/wide screen and saved cells equal; analysis unchanged; current/accumulated HTML Word HWPX Excel symbols verified; PDFs generated for separate verification\n')
