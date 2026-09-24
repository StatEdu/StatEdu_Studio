Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out <- 'tmp/mediation-final-vif';dir.create(out,recursive=TRUE,showWarnings=FALSE)
entries <- list()
for (case in c('simple','parallel','serial','moderation','moderated_mediation','wide','missing','percentile')) {
 result <- readRDS(file.path('outputs/spss_phase36_20260907',case,'analysis.rds'))
 for (path in result$path_results) {
  if (!mediation_moderation_final_model_vif(path)) next
  table <- mediation_moderation_display_coefficient_table(path,include_vif=TRUE)
  stopifnot(identical(tail(names(table),2),c('f2','VIF')),is.na(table$VIF[table$Term=='(Intercept)']))
  design <- model.matrix(path$model)
  predictors <- setdiff(colnames(design),'(Intercept)')
  expected <- vapply(predictors,function(term) {
   others <- design[,setdiff(colnames(design),term),drop=FALSE]
   fit <- lm.fit(others,design[,term]);rss<-sum(fit$residuals^2);tss<-sum((design[,term]-mean(design[,term]))^2)
   tss/rss
  },numeric(1))
  stopifnot(isTRUE(all.equal(unname(table$VIF[match(mediation_moderation_clean_term(predictors),table$Term)]),unname(expected),tolerance=1e-8)))
 }
 style <- if(case=='wide')'wide' else 'standard'
 html <- mediation_moderation_saved_results_html(result,language='ko',output_table_style=style)
 doc <- xml2::read_html(html)
 vif_tables <- xml2::xml_find_all(doc,"//table[.//th[normalize-space(.)='VIF']]")
 stopifnot(length(vif_tables)>0,grepl('VIF = variance inflation factor',html,fixed=TRUE))
 # Both standard and wide renderers retain f2 before VIF.
 for(tbl in vif_tables) {
  headers<-xml2::xml_text(xml2::xml_find_all(tbl,'.//th'))
  stopifnot(match('f²',headers)<match('VIF',headers))
 }
 writeLines(html,file.path(out,paste0(case,'.html')),useBytes=TRUE)
 if(case %in% c('simple','moderation','moderated_mediation')) entries[[length(entries)+1L]] <- list(id=case,title=case,html=html,saved_at='2026-09-16')
 cat('PASS:',case,'final-model VIF matches independent auxiliary regressions; column order and note\n')
}
saveRDS(entries,file.path(out,'entries.rds'))
if (identical(Sys.getenv('STATEDU_VIF_EXPORTS'),'true')) {
 normalize <- function(x)gsub('[[:space:]\u00a0]+','',paste(x,collapse=''),perl=TRUE)
 for(mode in c('current','accumulated')) {
  selected<-if(mode=='current')entries[1] else entries
  stem<-file.path(out,mode)
  write_result_collection_html(selected,paste0(stem,'.html'))
  write_result_collection_docx(selected,paste0(stem,'.docx'))
  save_result_collection_excel_file(selected,paste0(stem,'.xlsx'))
  write_result_collection_hwpx(selected,paste0(stem,'.hwpx'))
  write_result_collection_pdf(selected,paste0(stem,'.pdf'))
  doc<-xml2::read_html(paste(vapply(selected,`[[`,character(1),'html'),collapse='\n'))
  expected<-xml2::xml_text(xml2::xml_find_all(doc,'//th|//td|//*[contains(concat(" ",normalize-space(@class)," ")," coefficient-note ")]'))
  expected<-expected[nzchar(trimws(expected))]
  word<-normalize(xml2::xml_text(xml2::read_xml(unz(paste0(stem,'.docx'),'word/document.xml'))))
  members<-unzip(paste0(stem,'.hwpx'),list=TRUE)$Name
  hwpx<-normalize(vapply(members[grepl('Contents/section[0-9]+[.]xml$',members)],function(s)xml2::xml_text(xml2::read_xml(unz(paste0(stem,'.hwpx'),s))),character(1)))
  workbook<-openxlsx::loadWorkbook(paste0(stem,'.xlsx'))
  excel<-normalize(unlist(lapply(seq_along(names(workbook)),function(i)as.matrix(openxlsx::read.xlsx(workbook,sheet=i,colNames=FALSE)))))
  html<-normalize(xml2::xml_text(xml2::read_html(paste0(stem,'.html'))))
  for(value in expected)for(actual in list(word,hwpx,excel,html))stopifnot(grepl(normalize(value),actual,fixed=TRUE))
  jsonlite::write_json(as.list(expected),paste0(stem,'-expected.json'),auto_unbox=TRUE)
  cat('PASS:',mode,'all cells and coefficient notes retained in HTML/Word/HWPX/Excel; PDF generated\n')
 }
}
