Sys.setlocale('LC_ALL','Korean_Korea.utf8')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules();options(statedu.app_language='ko')
out<-'outputs/spss_phase32_20260906';dir.create(out,showWarnings=FALSE)
set.seed(20260917);n<-180
d<-as.data.frame(matrix(rnorm(n*25),ncol=25));names(d)<-paste0('x',1:25)
d$group<-factor(rep(c('대조','처치1','처치2'),60));d$y<-2*d$x1-d$x2+rnorm(n);d$y2<-d$x1+.5*d$x3+rnorm(n)
info<-data.frame(name=names(d),measurement=ifelse(names(d)=='group','category','continuous'),var_label=ifelse(names(d)=='group','집단',names(d)))
make<-function(data=d,dep='y',b1='x1',b2='x2',b3=character(0),diag=FALSE)prepare_hierarchical_analysis_results(data,dep,b1,b2,b3,variable_info=info,residual_diagnostics=diag,auto_method=FALSE)$results
m<-d;m$y[1:7]<-NA;m$x1[8:11]<-NA
cases<-list(two_blocks=make(),three_blocks=make(b3='x3'),categorical=make(b2='group'),diagnostics=make(diag=TRUE),multi_outcome=make(dep=c('y','y2')),wide=make(b3='x3'),missing=make(m),long_coefficients=make(b1=paste0('x',1:12),b2=paste0('x',13:25)))
for(name in names(cases)) {
 r<-cases[[name]];folder<-file.path(out,name);dir.create(folder,showWarnings=FALSE)
 args<-list(results=r,variable_table=info,show_sr2=TRUE,show_f2=TRUE,show_vif=TRUE,output_table_style=if(name=='wide')'wide' else 'standard')
 do.call(write_hierarchical_results_html,c(args,list(file=file.path(folder,'result.html'))))
    html<-paste(readLines(file.path(folder,'result.html'),encoding='UTF-8'),collapse='\n');b<-xml2::read_html(html)
 a<-xml2::read_html(as.character(htmltools::renderTags(do.call(hierarchical_results_panel,args))$html))
 cells<-function(doc)vapply(xml2::xml_find_all(doc,'.//table//th|.//table//td'),result_html_text,character(1))
 stopifnot(identical(cells(a),cells(b)))
 e<-list(title='Regression',html=html,saved_at='2026-09-06')
 do.call(write_hierarchical_results_pdf,c(args,list(file=file.path(folder,'result.pdf'))));do.call(save_hierarchical_excel_file,c(args,list(file=file.path(folder,'result.xlsx'))));write_result_collection_docx(list(e),file.path(folder,'result.docx'))
 tables<-result_entry_tables(e)
 expected<-list(tables=lapply(tables,function(t)list(title=t$title,orientation=t$orientation,notes=t$notes,cells=lapply(t$screen$cells,function(c)c(c,list(value=t$screen$values[c$row,c$col]))))),images=as.list(vapply(xml2::xml_find_all(b,'.//img'),function(img)result_html_text(xml2::xml_find_first(img,'ancestor::div[h4][1]/h4')),character(1))))
 jsonlite::write_json(expected,file.path(folder,'expected.json'),auto_unbox=TRUE);saveRDS(r,file.path(folder,'analysis.rds'))
 cat(name,length(tables),'tables',length(expected$images),'images\n')
}
