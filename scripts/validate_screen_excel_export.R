Sys.setlocale('LC_ALL','Korean_Korea.utf8')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'outputs/spss_phase17_20260906';dir.create(out,showWarnings=FALSE)
for(mode in c('reml_un','reml_ar1')) {
 r<-readRDS(paste0('outputs/spss_phase13_20260906/',mode,'.rds'))
 save_longitudinal_excel_file(r,file.path(out,paste0(mode,'.xlsx')))
 e<-list(title='Longitudinal',html=saved_longitudinal_results_html(r))
 expected<-lapply(result_entry_tables(e),function(t) list(title=t$title,orientation=t$orientation,notes=t$notes,cells=lapply(t$screen$cells,function(c) c(c,list(value=t$screen$values[c$row,c$col])))))
 jsonlite::write_json(expected,file.path(out,paste0(mode,'_expected.json')),auto_unbox=TRUE)
}
fixture<-'<html><body><h3>Merged</h3><div data-result-table-sheet="true"><table data-result-table-orientation="landscape"><thead><tr><th rowspan="2"></th><th colspan="2">A</th></tr><tr><th>B</th><th>C</th></tr></thead><tbody><tr><td>x</td><td>0.0000</td><td>&lt;.001</td></tr></tbody></table></div></body></html>'
save_screen_excel_file(fixture,file.path(out,'merged.xlsx'))
cat('2 longitudinal exports and merged-cell fixture created.\n')
