Sys.setlocale('LC_ALL','Korean_Korea.utf8')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
options(statedu.app_language='ko')
r <- readRDS('outputs/spss_phase32_20260906/three_blocks/analysis.rds')
r <- lapply(r,function(x) {x$residual_diagnostics<-TRUE;x})
# Layout fixture: match the nine-column HC3-style output and exercise a long table.
r[[3]]$coef_table$beta <- NULL
long_rows <- r[[3]]$coef_table[rep(2L, 42L), , drop=FALSE]
long_rows$Term <- paste0('Long predictor label ', seq_len(nrow(long_rows)))
r[[3]]$coef_table <- rbind(r[[3]]$coef_table,long_rows)
overview <- model_overview_data_frame(r)
stopifnot(nrow(overview)==3,ncol(overview)==4,identical(names(overview),c('Model','N','Analysis','Reason')))
content <- hierarchical_results_panel(r,show_f2=TRUE,show_vif=TRUE,output_table_style='standard')
# Exported diagnostic plots use the same per-model two-card block as the application.
plots <- lapply(r,function(x)saved_plot_result_block(x,'Outcome'))
content <- tagList(content,plots)
# Exercise captured Shiny plot wrappers as well as direct image exports.
for (i in seq_along(plots)) {
  for (j in 1:2) {
    card <- plots[[i]]$children[[2]]$children[[j]]
    card$children[[2]] <- div(class='shiny-plot-output',style='width:100%;height:420px;',card$children[[2]])
    plots[[i]]$children[[2]]$children[[j]] <- card
  }
}
content <- tagList(hierarchical_results_panel(r,show_f2=TRUE,show_vif=TRUE,output_table_style='standard'),plots)
html <- saved_result_sheet_document('StatEdu Studio',content)
doc <- xml2::read_html(html)
stopifnot(length(xml2::xml_find_all(doc,".//*[@data-pdf-residual-pair='true']"))==3L)
for(pair in xml2::xml_find_all(doc,".//*[@data-pdf-residual-pair='true']"))stopifnot(length(xml2::xml_find_all(pair,'.//img'))==2L)
model <- xml2::xml_find_first(doc,".//table[contains(@class,'combined-model-overview-table')]")
stopifnot(length(xml2::xml_find_all(model,'.//tbody/tr'))==3L)
coefficient_sheets <- xml2::xml_find_all(doc, ".//*[@data-result-table-sheet='true'][.//table[contains(@class,'coefficient-table')]]")
stopifnot(length(coefficient_sheets)==3L,all(xml2::xml_attr(coefficient_sheets,'data-result-table-orientation')=='portrait'))
source_doc <- xml2::read_html(tags_to_html(content))
cells <- function(d) xml2::xml_text(xml2::xml_find_all(d,'.//table//th|.//table//td'))
stopifnot(identical(cells(source_doc),cells(doc)))
dir.create('tmp/pdfs/cover-preview',recursive=TRUE,showWarnings=FALSE)
dir.create('output/pdf',recursive=TRUE,showWarnings=FALSE)
writeLines(html,'tmp/pdfs/cover-preview/regression-layout.html',useBytes=TRUE)
write_pdf_from_html(html,'output/pdf/regression-layout-check.pdf')
cat('PASS: transposed overview; 3 model plot pairs; all table cells preserved.\n')
