if (.Platform$OS.type=='windows') Sys.setlocale('LC_CTYPE','Korean_Korea.utf8')
.libPaths(R.home('library'))
Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
statedu_apply_preferences(statedu_initial_preferences())
root <- 'output/descriptive-portrait-20260915'
dir.create(root,recursive=TRUE,showWarnings=FALSE)
d <- read.table('docs/evidence/release_1_3_0/pls/smartpls_4_1_1_8_hs_first100/HolzingerSwineford1939_first100_x1_x9.txt',header=TRUE,sep=';')
stopifnot(ncol(d)==9L,all(vapply(d,is.numeric,logical(1))))
d <- d[rep(seq_len(nrow(d)), length.out=412L), , drop=FALSE]
names(d) <- paste0('환자안전역량',seq_len(ncol(d)),'_지식',seq_len(ncol(d)))
info <- data.frame(name=names(d),measurement='continuous')
r <- prepare_frequencies_results(d,names(d),variable_info=info)
r$options <- list(n_percent=TRUE,mean_sd=FALSE,min_max=TRUE,median_iqr=TRUE,skew_kurtosis=TRUE)
before <- serialize(r,NULL)
panel <- htmltools::renderTags(frequencies_results_ui(r))$html
html <- saved_frequencies_results_html(r)
cells <- function(x) trimws(xml2::xml_text(xml2::xml_find_all(xml2::read_html(x),'//th|//td')))
stopifnot(identical(cells(panel),cells(html)),identical(before,serialize(r,NULL)))
entry <- list(id='descriptive',title='Descriptive statistics',html=html,saved_at='2026-09-15')
tables <- result_entry_tables(entry)
combined <- frequency_continuous_main_table(r,modifyList(r$options,list(mean_sd=TRUE)))
separate <- frequency_continuous_main_table(r,modifyList(r$options,list(mean_sd=FALSE)))
stopifnot(ncol(combined)==9L,"M ± SD" %in% names(combined),!any(c("M","SD") %in% names(combined)),
  identical(combined[["M ± SD"]],as.character(r$descriptive_table[["M ± SD"]])),
  ncol(separate)==10L,all(c("M","SD") %in% names(separate)))
stopifnot(length(tables)==1, tables[[1]]$orientation=='portrait')
stopifnot(attr(separate,'compact_column_widths')[1]>13.5,
  attr(separate,'compact_column_widths')[2]>6.8,
  identical(attr(separate,'right_align_columns'),'n'))
r_combined <- r
r_combined$options$mean_sd <- TRUE
combined_entry <- modifyList(entry,list(id='combined',html=saved_frequencies_results_html(r_combined)))
norm <- function(x) gsub('[[:space:]\u00a0]+','',paste(x,collapse=''),perl=TRUE)
expected <- cells(html)
for(mode in c('current','accumulated')) {
  selected <- if(mode=='current')list(entry) else list(entry,combined_entry)
  stem <- file.path(root,mode)
  write_result_collection_html(selected,paste0(stem,'.html'))
  write_result_collection_docx(selected,paste0(stem,'.docx'))
  save_result_collection_excel_file(selected,paste0(stem,'.xlsx'))
  write_result_collection_pdf(selected,paste0(stem,'.pdf'))
  write_result_collection_hwpx(selected,paste0(stem,'.hwpx'))
  word <- xml2::read_xml(unz(paste0(stem,'.docx'),'word/document.xml'))
  stopifnot(!any(xml2::xml_attr(xml2::xml_find_all(word,'//*[local-name()="pgSz"]'),'orient')=='landscape',na.rm=TRUE))
  members <- unzip(paste0(stem,'.hwpx'),list=TRUE)$Name
  hw <- lapply(members[grepl('Contents/section[0-9]+[.]xml$',members)],function(s)xml2::read_xml(unz(paste0(stem,'.hwpx'),s)))
  for(doc in hw) {
    pages <- xml2::xml_find_all(doc,'//*[local-name()="pagePr"]')
    stopifnot(all(as.numeric(xml2::xml_attr(pages,'width'))<as.numeric(xml2::xml_attr(pages,'height'))))
  }
  wb <- openxlsx::loadWorkbook(paste0(stem,'.xlsx'))
  excel <- norm(unlist(lapply(seq_along(names(wb)),function(i)as.matrix(openxlsx::read.xlsx(wb,sheet=i,colNames=FALSE)))))
  for(value in expected)for(actual in list(norm(xml2::xml_text(word)),norm(vapply(hw,xml2::xml_text,character(1))),excel))stopifnot(grepl(norm(value),actual,fixed=TRUE))
  for(sheet in wb$worksheets)stopifnot(!grepl('orientation="landscape"',sheet$pageSetup,fixed=TRUE))
}
writeLines(html,file.path(root,'screen-snapshot.html'),useBytes=TRUE)
cat('PASS: screen/saved cells and analysis unchanged; portrait metadata; current/accumulated five exports generated and document/workbook content verified\n')
