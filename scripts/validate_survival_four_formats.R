Sys.setlocale('LC_ALL','Korean_Korea.utf8')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules();options(statedu.app_language='ko')
out<-'outputs/spss_phase39_20260907';dir.create(out,showWarnings=FALSE)
d<-read.csv('scripts/fixtures/survival_validation.csv');d$sex<-factor(d$sex)
m<-d;m$age[1:5]<-NA
cr<-d;cr$status[which(cr$status==1)[seq(2,50,by=3)]]<-2
km<-function(data=d,extra=list())do.call(prepare_km_analysis_result,modifyList(list(data=data,time='time',event='status',event_value='1',rate_times='100,200,400',plot_types='survival'),extra))
cox<-function(data=d,extra=list())do.call(prepare_cox_analysis_result,modifyList(list(data=data,time='time',event='status',event_value='1',covariates=c('age','sex')),extra))
comp<-function(regression='none')prepare_competing_risk_result(cr,'time','status',group='sex',rate_times='100,200,400',covariates=if(regression=='none')character(0) else c('age','sex'),regression=regression)
cases<-list(km_ungrouped=function()km(),km_grouped=function()km(extra=list(group=c('sex','ph.ecog'),plot_types=c('survival','event'),plot_versions=c('color','bw'))),life_table=function()km(extra=list(analysis_method='life_table',group='sex')),cox=function()cox(),cox_strata=function()cox(extra=list(covariates='age',strata='sex')),cox_missing=function()cox(m),competing=function()comp(),fine_gray=function()comp('fine_gray'),long_times=function()km(extra=list(group='sex',rate_times=paste(seq(5,500,5),collapse=',')) ))
selected<-commandArgs(trailingOnly=TRUE);if(length(selected))cases<-cases[intersect(names(cases),selected)]
for(name in names(cases)) {
 r<-cases[[name]]();folder<-file.path(out,name);dir.create(folder,showWarnings=FALSE)
 write_survival_results_html(r,file.path(folder,'result.html'),language='ko')
 html<-paste(readLines(file.path(folder,'result.html'),encoding='UTF-8'),collapse='\n');b<-xml2::read_html(html)
 panel<-if(r$type %in% c('km','km_multi'))survival_km_results_panel(r,survival_saved_km_plot_ids(r),language='ko') else if(r$type=='cox')survival_cox_results_panel(r,language='ko') else survival_competing_results_panel(r,language='ko')
 a<-xml2::read_html(as.character(htmltools::renderTags(panel)$html))
 cells<-function(doc)vapply(xml2::xml_find_all(doc,'.//table//th|.//table//td'),result_html_text,character(1));stopifnot(identical(cells(a),cells(b)))
 e<-list(title='Survival',html=html,saved_at='2026-09-07')
 write_survival_results_pdf(r,file.path(folder,'result.pdf'),language='ko');save_survival_excel_file(r,file.path(folder,'result.xlsx'),language='ko');write_result_collection_docx(list(e),file.path(folder,'result.docx'))
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
