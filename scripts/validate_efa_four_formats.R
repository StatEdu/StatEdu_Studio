Sys.setlocale('LC_ALL','Korean_Korea.utf8')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules();options(statedu.app_language='ko')
out<-'outputs/spss_phase34_20260906';dir.create(out,showWarnings=FALSE)
set.seed(20260919);n<-240;f1<-rnorm(n);f2<-.3*f1+rnorm(n)
d<-as.data.frame(sapply(1:30,function(i)if(i%%2)f1+rnorm(n,sd=.6) else f2+rnorm(n,sd=.6)));names(d)<-paste0('item',1:30)
info<-data.frame(name=names(d),measurement='continuous',var_label=paste('문항',1:30))
make<-function(data=d,k=8,extra=list(),vi=info)prepare_factor_analysis_results(data,names(d)[1:k],variable_info=vi,options=modifyList(list(normality=FALSE,method='pa',rotation='varimax',criterion='fixed',n_factors=2,sort_loadings=TRUE,hide_small_loadings=TRUE,highlight_problem_values=TRUE,subfactor_reliability=TRUE),extra))
m<-d;m$item1[1:7]<-NA;m$item3[8:11]<-NA
o<-as.data.frame(lapply(d,function(x)as.integer(cut(x,quantile(x,seq(0,1,length.out=6)),include.lowest=TRUE))));oi<-info;oi$measurement<-'ordered'
cases<-list(varimax=make(),oblimin=make(extra=list(rotation='oblimin')),maximum_likelihood=make(extra=list(method='ml',rotation='varimax')),unrotated=make(extra=list(rotation='none',sort_loadings=FALSE,hide_small_loadings=FALSE,subfactor_reliability=FALSE)),normality=make(extra=list(normality=TRUE)),polychoric=make(o,extra=list(matrix_type='polychoric'),vi=oi),missing=make(m),long_items=make(k=30))
for(name in names(cases)) {
 r<-cases[[name]];folder<-file.path(out,name);dir.create(folder,showWarnings=FALSE)
 write_factor_analysis_results_html(r,file.path(folder,'result.html'))
 html<-paste(readLines(file.path(folder,'result.html'),encoding='UTF-8'),collapse='\n');b<-xml2::read_html(html)
 a<-xml2::read_html(as.character(htmltools::renderTags(factor_analysis_results_ui(r))$html))
 cells<-function(doc)vapply(xml2::xml_find_all(doc,'.//table//th|.//table//td'),result_html_text,character(1));stopifnot(identical(cells(a),cells(b)))
 e<-list(title='EFA',html=html,saved_at='2026-09-06')
 write_factor_analysis_results_pdf(r,file.path(folder,'result.pdf'));save_factor_analysis_excel_file(r,file.path(folder,'result.xlsx'));write_result_collection_docx(list(e),file.path(folder,'result.docx'))
  tables<-result_entry_tables(e)
  image_items<-result_entry_images(e)
  orders<-vapply(c(tables,image_items),`[[`,numeric(1),'output_order')
  sheet_indices<-rank(orders,ties.method='first')[seq_along(tables)]
  unlink(vapply(image_items,`[[`,character(1),'path'))
 expected<-list(tables=lapply(tables,function(t)list(title=t$title,orientation=t$orientation,notes=t$notes,cells=lapply(t$screen$cells,function(c)c(c,list(value=t$screen$values[c$row,c$col]))))),images=as.list(xml2::xml_attr(xml2::xml_find_all(b,'.//img'),'alt')))
  for(i in seq_along(expected$tables))expected$tables[[i]]$sheet_index<-unname(sheet_indices[i])
  jsonlite::write_json(expected,file.path(folder,'expected.json'),auto_unbox=TRUE);saveRDS(r,file.path(folder,'analysis.rds'))
 cat(name,length(tables),'tables',length(expected$images),'images\n')
}
