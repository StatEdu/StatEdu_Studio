Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/competing-overview-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
d<-read.csv('scripts/fixtures/survival_validation.csv');idx<-which(d$status==1);d$status[idx[seq(1,length(idx),by=2)]]<-2
for(group in c('','sex')){
 result<-prepare_competing_risk_result(d,'time','status',group=group,rate_times=c(100,250,500))
 for(kind in c('actual','custom'))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
  options(statedu.app_language=language);r<-result
  if(kind=='custom'){
   r$time_origin<-'Review';r$time_unit<-'Normality';r$event_of_interest<-'None';r$group<-'사용자 <&> %s';r$censoring_group<-'Pooled'
  }
  rows<-survival_competing_overview_table(r,language)
  html<-as.character(survival_simple_table(rows,table_language=language));doc<-xml2::read_html(html,encoding='UTF-8')
  vals<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[2]')))
  stopifnot(identical(vals,as.character(rows$Value)))
  protected<-if(kind=='custom')c(1:9,11) else c(1:7,11)
  if(language=='en')baseline<-vals else{
   stopifnot(identical(vals[protected],baseline[protected]),vals[10]!=baseline[10])
   items<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[1]')))
   stopifnot(!any(c('Interest events','Fine-Gray censoring strata')%in%items))
  }
  main<-as.character(survival_simple_table(survival_competing_rate_table(result),table_role='main',table_language='en'))
  if(language=='en')main_baseline<-main else stopifnot(identical(main,main_baseline))
  if(language=='ja')entries[[paste(group,kind)]]<-list(id=paste(group,kind),title=paste(group,kind),html=html)
  cat('PASS:',group,kind,language,'competing overview, preserved settings and English CIF table\n')
 }
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
