Sys.setlocale('LC_ALL','Korean_Korea.utf8')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
models<-readRDS('outputs/spss_phase32_20260906/three_blocks/analysis.rds')
for(sr2 in c(FALSE,TRUE))for(f2 in c(FALSE,TRUE))for(vif in c(FALSE,TRUE)) {
 for(kind in c('regression','hierarchical')) {
  ui<-if(kind=='hierarchical') hierarchical_results_panel(models,show_sr2=sr2,show_f2=f2,show_vif=vif) else regression_coefficient_result_block(models[[3]],show_sr2=sr2,show_f2=f2,show_vif=vif)
  doc<-xml2::read_html(tags_to_html(ui))
  headers<-paste(xml2::xml_text(xml2::xml_find_all(doc,".//table[contains(@class,'coefficient-table')]//th")),collapse=' ')
  notes<-paste(xml2::xml_text(xml2::xml_find_all(doc,".//*[contains(concat(' ',normalize-space(@class),' '),' coefficient-note ')]")),collapse=' ')
  stopifnot(grepl('sr²',headers,fixed=TRUE)==sr2,grepl('sr²',notes,fixed=TRUE)==sr2,
    grepl('f²',headers,fixed=TRUE)==f2,grepl('f² =',notes,fixed=TRUE)==f2,
    grepl('VIF',headers,fixed=TRUE)==vif,grepl('VIF =',notes,fixed=TRUE)==vif,
    grepl('Tol =',notes,fixed=TRUE)==vif)
 }
}
cat('PASS: table/header and note visibility match for 16 regression/hierarchical option combinations.\n')
