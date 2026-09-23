Sys.setlocale('LC_ALL','Korean_Korea.utf8')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules();options(statedu.app_language='ko')
out<-'outputs/spss_phase29_20260906';dir.create(out,showWarnings=FALSE)
set.seed(20260915);n<-160
d<-data.frame(g2=factor(rep(c('대조','처치'),each=80)),g4=factor(rep(c('낮음','중하','중상','높음'),each=40),levels=c('낮음','중하','중상','높음')))
d$ord<-ordered(d$g4);d$y<-rnorm(n)+rep(0:3,each=40);d$rank<-sample(1:5,n,TRUE)+rep(0:3,each=40)
for(i in 1:8)d[[paste0('score',i)]]<-d$y+rnorm(n,sd=.2+i/10)
info<-data.frame(name=names(d),measurement=c('binary','category','ordered','continuous','ordered',rep('continuous',8)),var_label=c('두 집단','네 집단','순서 집단','측정값','순위 점수',paste('측정 점수',1:8)))
make<-function(data=d,dep='y',fac='g2',extra=list()){
 r<-prepare_ttest_anova_results(data,dep,fac,variable_info=info,options=modifyList(list(force_nonparametric=TRUE,normality_enabled=FALSE,normality_method='none',trend_analysis=FALSE,post_hoc=TRUE,nonparametric_post_hoc_method='holm',ordered_significance=TRUE,effect_size=TRUE,median_iqr=TRUE),extra));r$type<-'nonparametric';r
}
m<-d;m$y[1:7]<-NA;m$g4[8:11]<-NA
same<-d;same$y<-rep(seq_len(40),4)
cases<-list(mann_whitney=make(),kruskal_holm=make(fac='g4'),kruskal_bonferroni=make(dep='rank',fac='g4',extra=list(nonparametric_post_hoc_method='bonferroni')),trend=make(fac='ord',extra=list(trend_analysis=TRUE)),minimal=make(extra=list(effect_size=FALSE,median_iqr=FALSE,ordered_significance=FALSE)),missing=make(m,fac='g4'),equal_groups=make(same,fac='g4'),wide=make(dep=paste0('score',1:8),fac=c('g2','g4')))
for(name in names(cases)) {
 r<-cases[[name]];folder<-file.path(out,name);dir.create(folder,showWarnings=FALSE)
 write_nonparametric_results_html(r,file.path(folder,'result.html'))
 html<-paste(readLines(file.path(folder,'result.html'),encoding='UTF-8'),collapse='\n')
 a<-xml2::read_html(as.character(htmltools::renderTags(ttest_anova_results_ui(r))$html));b<-xml2::read_html(html)
 cells<-function(doc)vapply(xml2::xml_find_all(doc,'.//table//th|.//table//td'),result_html_text,character(1));stopifnot(identical(cells(a),cells(b)))
 e<-list(title='Nonparametric',html=html,saved_at='2026-09-06')
 write_nonparametric_results_pdf(r,file.path(folder,'result.pdf'));save_ttest_anova_excel_file(r,file.path(folder,'result.xlsx'));write_result_collection_docx(list(e),file.path(folder,'result.docx'))
 tables<-result_entry_tables(e)
 expected<-list(tables=lapply(tables,function(t)list(title=t$title,orientation=t$orientation,notes=t$notes,cells=lapply(t$screen$cells,function(c)c(c,list(value=t$screen$values[c$row,c$col]))))),images=as.list(xml2::xml_attr(xml2::xml_find_all(b,'.//img'),'alt')))
 jsonlite::write_json(expected,file.path(folder,'expected.json'),auto_unbox=TRUE);saveRDS(r,file.path(folder,'analysis.rds'))
 cat(name,length(tables),'tables; screen/HTML matched\n')
}
