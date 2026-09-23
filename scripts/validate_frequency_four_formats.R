Sys.setlocale('LC_ALL','Korean_Korea.utf8')
source('scripts/validate_frequencies_screen_table_contract.R',encoding='UTF-8')
out<-'outputs/spss_phase19_20260906';dir.create(out,showWarnings=FALSE)
cases<-list(mixed=result,categorical=categorical_only,continuous=continuous_only)
d<-fixture;d$group[2]<-NA;d$outcome[3]<-NA;vi<-variable_info;vi$var_label<-c('집단','측정 결과','점수')
x<-prepare_frequencies_results(d,names(d),variable_info=vi);x$options<-result$options;cases$missing_labels<-x
x<-result;x$options$bar<-TRUE;x$options$histogram<-TRUE;cases$plots<-x
for(name in names(cases)) {
 r<-cases[[name]];dir<-file.path(out,name);dir.create(dir,showWarnings=FALSE)
 html<-saved_frequencies_results_html(r);writeLines(html,file.path(dir,'result.html'),useBytes=TRUE)
 a<-xml2::read_html(as.character(htmltools::renderTags(frequencies_results_ui(r))$html));b<-xml2::read_html(html)
 cells<-function(d)vapply(xml2::xml_find_all(d,'.//table//th|.//table//td'),result_html_text,character(1))
 stopifnot(identical(cells(a),cells(b)))
 e<-list(title='Frequencies',html=html,saved_at='2026-09-06')
 write_frequencies_results_pdf(r,file.path(dir,'result.pdf'))
 save_frequencies_excel_file(r,file.path(dir,'result.xlsx'))
 write_result_collection_docx(list(e),file.path(dir,'result.docx'))
 tables<-result_entry_tables(e)
 expected<-list(tables=lapply(tables,function(t)list(title=t$title,orientation=t$orientation,notes=t$notes,cells=lapply(t$screen$cells,function(c)c(c,list(value=t$screen$values[c$row,c$col]))))),
  images=as.list(xml2::xml_attr(xml2::xml_find_all(b,'.//img'),'alt')))
 jsonlite::write_json(expected,file.path(dir,'expected.json'),auto_unbox=TRUE)
 cat(name,length(tables),'tables',length(expected$images),'plots: four files generated; screen/HTML cells matched\n')
}
