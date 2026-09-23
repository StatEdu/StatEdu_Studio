Sys.setlocale('LC_ALL','Korean_Korea.utf8')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules();options(statedu.app_language='ko')
out<-'outputs/spss_phase36_20260907'
set.seed(20260907);n<-180;d<-data.frame(X=rnorm(n),W=rnorm(n),C=rnorm(n));d$M<-.6*d$X+.35*d$X*d$W+rnorm(n);d$M2<-.4*d$X+.5*d$M+rnorm(n);d$Y<-.25*d$X+.6*d$M+.3*d$M2+.4*d$X*d$W+rnorm(n)
make<-function(m='M',w=character(0),paths=character(0),arrangement='parallel',data=d,extra=list()) do.call(run_mediation_moderation_analysis,modifyList(list(data=data,roles=list(y='Y',x='X',mediators=m,w=w,covariates='C'),mediator_arrangement=arrangement,moderated_paths=paths,boot_r=80L,seed=2026L,analysis_method='process_ols',ci_method='bias_corrected',residual_diagnostics=FALSE,auto_method=FALSE,language='ko',variable_info=data.frame(name=names(data),measurement='continuous',var_label=names(data))),extra))
missing<-d;missing$M[1:9]<-NA
cases<-list(simple=function()make(),parallel=function()make(c('M','M2')),serial=function()make(c('M','M2'),arrangement='serial'),moderation=function()make(character(0),'W','xy'),moderated_mediation=function()make('M','W','xm'),wide=function()make('M','W','xm'),percentile=function()make(extra=list(ci_method='percentile')),missing=function()make(data=missing))
for(name in names(cases)){
 r<-cases[[name]]();folder<-file.path(out,name);dir.create(folder,showWarnings=FALSE);style<-if(name=='wide')'wide' else 'standard'
 write_mediation_moderation_results_html(r,file.path(folder,'result.html'),language='ko',output_table_style=style)
 html<-paste(readLines(file.path(folder,'result.html'),encoding='UTF-8'),collapse='\n');b<-xml2::read_html(html)
 a<-xml2::read_html(as.character(htmltools::renderTags(mediation_moderation_result_ui(r,language='ko',output_table_style=style))$html))
 cells<-function(doc)vapply(xml2::xml_find_all(doc,'.//table//th|.//table//td'),result_html_text,character(1));stopifnot(identical(cells(a),cells(b)))
 e<-list(title='Mediation moderation',html=html,saved_at='2026-09-07')
 write_mediation_moderation_results_pdf(r,file.path(folder,'result.pdf'),language='ko',output_table_style=style)
 save_mediation_moderation_excel_file(r,file.path(folder,'result.xlsx'),language='ko',output_table_style=style)
 write_result_collection_docx(list(e),file.path(folder,'result.docx'))
 tables<-result_entry_tables(e);imgs<-result_entry_images(e)
 orders<-vapply(c(tables,imgs),`[[`,numeric(1),'output_order');indices<-rank(orders,ties.method='first')[seq_along(tables)]
 expected<-list(tables=lapply(tables,function(t)list(title=t$title,orientation=t$orientation,notes=t$notes,cells=lapply(t$screen$cells,function(c)c(c,list(value=t$screen$values[c$row,c$col]))))),images=as.list(vapply(imgs,`[[`,character(1),'title')))
 for(i in seq_along(tables))expected$tables[[i]]$sheet_index<-unname(indices[i])
 jsonlite::write_json(expected,file.path(folder,'expected.json'),auto_unbox=TRUE);saveRDS(r,file.path(folder,'analysis.rds'));unlink(vapply(imgs,`[[`,character(1),'path'))
 cat(name,length(tables),'tables',length(imgs),'figures\n')
}
