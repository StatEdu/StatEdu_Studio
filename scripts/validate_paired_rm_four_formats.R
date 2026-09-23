Sys.setlocale('LC_ALL','Korean_Korea.utf8')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules();options(statedu.app_language='ko')
out<-'outputs/spss_phase26_20260906';dir.create(out,showWarnings=FALSE)
set.seed(20260912);subject<-rnorm(180)
d<-as.data.frame(sapply(1:8,function(i)subject+rnorm(180)+i/3));names(d)<-paste0('t',1:8)
for(i in 1:4){d[[paste0('ord',i)]]<-sample(1:5,180,TRUE);d[[paste0('bin',i)]]<-rbinom(180,1,.2+i*.12)}
info<-data.frame(name=names(d),measurement=ifelse(grepl('^ord',names(d)),'ordered',ifelse(grepl('^bin',names(d)),'binary','continuous')),var_label=paste('측정',seq_along(d)))
make<-function(data=d,groups=list(paste0('t',1:3)),extra=list())prepare_paired_rm_results(data,variable_groups=groups,variable_info=info,options=modifyList(list(assumption_check=FALSE,effect_size=TRUE,mean_sd=TRUE,posthoc_adjustment='holm',time_labels=c('사전','중간','사후')),extra))
h<-d;for(i in 1:4)h[[paste0('t',i)]]<-subject+rnorm(180,sd=i^2)+i/3
m<-d;m$t1[1:7]<-NA;m$t3[8:11]<-NA
z<-d;z$t2<-z$t1;z$t3<-z$t1
cases<-list(standard=make(),sphericity=make(h,list(paste0('t',1:4)),list(assumption_check=TRUE)),friedman=make(groups=list(paste0('ord',1:3)),extra=list(mean_sd=FALSE,median_iqr=TRUE)),cochran=make(groups=list(paste0('bin',1:4)),extra=list(posthoc_adjustment='bonferroni')),missing=make(m),mixed=make(groups=list(paste0('t',1:3),paste0('ord',1:3),paste0('bin',1:3)),extra=list(median_iqr=TRUE)),eight_times=make(groups=list(paste0('t',1:8)),extra=list(time_labels=paste0('시점 ',1:8))),zero_change=make(z))
for(name in names(cases)) {
 r<-cases[[name]];folder<-file.path(out,name);dir.create(folder,showWarnings=FALSE)
 write_paired_rm_results_html(r,file.path(folder,'result.html'))
 html<-paste(readLines(file.path(folder,'result.html'),encoding='UTF-8'),collapse='\n')
 a<-xml2::read_html(as.character(htmltools::renderTags(paired_rm_results_ui(r))$html));b<-xml2::read_html(html)
 cells<-function(doc)vapply(xml2::xml_find_all(doc,'.//table//th|.//table//td'),result_html_text,character(1));stopifnot(identical(cells(a),cells(b)))
 e<-list(title='Repeated measures',html=html,saved_at='2026-09-06')
 write_paired_rm_results_pdf(r,file.path(folder,'result.pdf'));save_paired_rm_excel_file(r,file.path(folder,'result.xlsx'));write_result_collection_docx(list(e),file.path(folder,'result.docx'))
 tables<-result_entry_tables(e)
 expected<-list(tables=lapply(tables,function(t)list(title=t$title,orientation=t$orientation,notes=t$notes,cells=lapply(t$screen$cells,function(c)c(c,list(value=t$screen$values[c$row,c$col]))))),images=as.list(xml2::xml_attr(xml2::xml_find_all(b,'.//img'),'alt')))
 jsonlite::write_json(expected,file.path(folder,'expected.json'),auto_unbox=TRUE);saveRDS(r,file.path(folder,'analysis.rds'))
 cat(name,length(tables),'tables; screen/HTML matched\n')
}
