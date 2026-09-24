Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/survival-checklist-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
d<-read.csv('scripts/fixtures/survival_validation.csv')
results<-list(km=prepare_km_single_analysis_result(d,'time','status'),cox=prepare_cox_analysis_result(d,'time','status',c('age','sex'),event_value='1'))
idx<-which(d$status==1);d$status[idx[seq(1,length(idx),by=2)]]<-2
results$competing<-prepare_competing_risk_result(d,'time','status',covariates=c('age','sex'),regression='both',rate_times=c(100,250,500))
results$grouped<-results$competing;results$grouped$censoring_group<-'Review 사용자 <&> %s'
results$ph_signal<-results$cox;results$ph_signal$ph_table$p[]<-.001
results$ph_quiet<-results$cox;results$ph_quiet$ph_table$p[]<-.7
results$unresolved<-results$km;results$unresolved$preflight<-survival_preflight(data.frame(t=1:3,e=1:3),list(data_shape='single_record',roles=list(time='t',event='e'),event_of_interest='1',event_map=data.frame(raw_value=as.character(1:3),role=c('event_of_interest','unknown','censored'),label=c('a','b','c'))))
for(kind in names(results))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 options(statedu.app_language=language);r<-results[[kind]];r$time_origin<-'Review';r$time_unit<-'Normality 사용자 <&> %s'
 rows<-survival_reporting_checklist(r,language)
 html<-as.character(tagList(tags$h3(statedu_localized_text(language,'Reporting checklist and interpretation guide','보고 체크리스트와 해석 가이드')),survival_simple_table(rows,table_language=language)))
 doc<-xml2::read_html(html,encoding='UTF-8');evidence<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[3]')))
 stopifnot(identical(evidence,rows$Evidence),identical(evidence[1:2],c(r$time_origin,r$time_unit)))
 if(kind=='grouped')stopifnot(grepl(r$censoring_group,evidence[8],fixed=TRUE))
 stopifnot(grepl(as.character(r$n),evidence[4],fixed=TRUE))
 if(!language%in%c('en','ko')){
  original<-survival_reporting_checklist(r,'en')
  stopifnot(all(rows$Item!=original$Item),all(rows$Status!=original$Status),rows$Evidence[4]!=original$Evidence[4])
  if(nrow(rows)>6)stopifnot(all(rows$Evidence[7:nrow(rows)]!=original$Evidence[7:nrow(rows)]))
  stopifnot(!grepl('event_of_interest|censored|Check unresolved roles',evidence[3]))
 }
 if(language=='ja' && kind%in%c('km','cox','competing','grouped','unresolved'))entries[[kind]]<-list(id=kind,title=kind,html=html)
 cat('PASS:',kind,language,'checklist localization and verbatim user evidence\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
