Sys.setlocale('LC_ALL','Korean_Korea.utf8')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules();options(statedu.app_language='ko')
out<-'outputs/spss_phase30_20260906';dir.create(out,showWarnings=FALSE)
set.seed(20260916);n<-120;subject<-rnorm(n)
d<-data.frame(t1=subject+rnorm(n))
for(i in 2:8)d[[paste0('t',i)]]<-subject+rnorm(n)+i/3
for(i in 1:4){d[[paste0('o',i)]]<-sample(1:5,n,TRUE);d[[paste0('b',i)]]<-rbinom(n,1,.15+i*.15)}
info<-data.frame(name=names(d),measurement=ifelse(grepl('^o',names(d)),'ordered',ifelse(grepl('^b',names(d)),'binary','continuous')),var_label=paste('측정',seq_along(d)))
make<-function(data=d,groups=list(c('t1','t2')),extra=list())prepare_nonparametric_paired_unified_results(data,groups,variable_info=info,options=modifyList(list(effect_size=TRUE,median_iqr=TRUE,posthoc_adjustment='holm',time_labels=paste('시점',1:max(lengths(groups)))),extra))
m<-d;m$t1[1:7]<-NA;m$t3[8:11]<-NA
z<-d;z$t2<-z$t1;z$t3<-z$t1
cases<-list(wilcoxon=make(),binary=make(groups=list(c('b1','b2'))),friedman=make(groups=list(c('o1','o2','o3'))),cochran=make(groups=list(paste0('b',1:4)),extra=list(posthoc_adjustment='bonferroni')),combined=make(groups=list(c('t1','t2'),c('b1','b2'),c('o1','o2','o3'),paste0('b',1:4))),minimal=make(groups=list(paste0('t',1:3)),extra=list(effect_size=FALSE,median_iqr=FALSE)),missing=make(m,groups=list(paste0('t',1:3))),zero_change=make(z,groups=list(c('t1','t2'),paste0('t',1:3))),eight_times=make(groups=list(paste0('t',1:8))))
for(name in names(cases)) {
 r<-cases[[name]];folder<-file.path(out,name);dir.create(folder,showWarnings=FALSE)
 write_nonparametric_paired_results_html(r,file.path(folder,'result.html'))
 html<-paste(readLines(file.path(folder,'result.html'),encoding='UTF-8'),collapse='\n')
 a<-xml2::read_html(as.character(htmltools::renderTags(nonparametric_paired_results_ui(r))$html));b<-xml2::read_html(html)
 cells<-function(doc)vapply(xml2::xml_find_all(doc,'.//table//th|.//table//td'),result_html_text,character(1));stopifnot(identical(cells(a),cells(b)))
 e<-list(title='Nonparametric paired',html=html,saved_at='2026-09-06')
 write_nonparametric_paired_results_pdf(r,file.path(folder,'result.pdf'));save_nonparametric_paired_excel_file(r,file.path(folder,'result.xlsx'));write_result_collection_docx(list(e),file.path(folder,'result.docx'))
 tables<-result_entry_tables(e)
 expected<-list(tables=lapply(tables,function(t)list(title=t$title,orientation=t$orientation,notes=t$notes,cells=lapply(t$screen$cells,function(c)c(c,list(value=t$screen$values[c$row,c$col]))))),images=as.list(xml2::xml_attr(xml2::xml_find_all(b,'.//img'),'alt')))
 jsonlite::write_json(expected,file.path(folder,'expected.json'),auto_unbox=TRUE);saveRDS(r,file.path(folder,'analysis.rds'))
 cat(name,length(tables),'tables; screen/HTML matched\n')
}
