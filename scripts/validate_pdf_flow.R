Sys.setlocale('LC_ALL','Korean_Korea.utf8')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'outputs/spss_phase18_20260906';dir.create(out,showWarnings=FALSE)
make_table<-function(id,n,orientation='portrait') {
 tab<-tags$table(class='result-table-contract-table',style='width:100%;border-collapse:collapse;',
 tags$thead(tags$tr(tags$th(paste0(id,'_HEADER')),tags$th('Value'))),
 tags$tbody(lapply(seq_len(n),function(i)tags$tr(tags$td(sprintf('%s_ROW_%03d',id,i)),tags$td(sprintf('%.4f',i/7))))))
 div(class='regression-results',div(class='regression-result-panel',h3(id),
 div(class=paste('result-table-sheet result-table-with-note',paste0('result-table-sheet--',orientation)),
 `data-result-table-sheet`='true',`data-result-table-orientation`=orientation,tab,
 div(class='coefficient-note',paste0(id,'_END_NOTE')))))
}
content<-tagList(make_table('SHORT_A',3),make_table('SHORT_B',3),make_table('KEEP_C',20),make_table('LONG_P',110),make_table('LAND_A',3,'landscape'),make_table('LAND_B',3,'landscape'),make_table('LONG_L',95,'landscape'),make_table('FINAL_A',3),make_table('FINAL_B',3))
html<-saved_result_sheet_document('Pagination test',content)
writeLines(html,file.path(out,'pagination.html'),useBytes=TRUE)
write_pdf_from_html(html,file.path(out,'pagination.pdf'))
r<-readRDS('outputs/spss_phase13_20260906/reml_un.rds')
write_longitudinal_results_pdf(r,file.path(out,'longitudinal.pdf'))
cat('Pagination fixture and longitudinal PDFs generated.\n')
