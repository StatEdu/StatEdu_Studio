Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/cox-bootstrap-ties-warnings-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
d<-read.csv('scripts/fixtures/survival_validation.csv');result<-prepare_cox_analysis_result(d,'time','status',c('age','sex'),event_value='1')
cases<-data.frame(valid=c(79,80,89,90,90,90,0),reps=c(rep(100,6),0),ties=c(.099,.099,.099,.099,.1,.1,.1),method=c(rep('breslow',5),'efron','efron'))
codes<-c('adjusted_survival_bootstrap_insufficient','adjusted_survival_bootstrap_attrition','breslow_with_substantial_ties')
expected<-c(codes[1],codes[2],codes[2],'',codes[3],'','')
for(i in seq_len(nrow(cases)))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 r<-result;c<-cases[i,];r$adjusted_survival<-list(bootstrap_reps=c$reps,bootstrap_successful=c$valid,bootstrap_effective_ratio=c$valid/100,ci_available=c$valid>=80)
 r$ties_method<-c$method;r$ties_summary<-data.frame(`Proportion of events at tied times`=c$ties,check.names=FALSE)
 rows<-survival_stability_review(r,language);rows<-rows[rows$Code%in%codes,,drop=FALSE]
 stopifnot(nrow(rows)==as.integer(nzchar(expected[i])))
 if(nrow(rows)){
  stopifnot(rows$Code==expected[i],rows$Level==if(i==1)'high' else 'review')
  html<-as.character(survival_simple_table(rows,table_language=language));doc<-xml2::read_html(html,encoding='UTF-8')
  evidence<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[3]')))
  suffix<-if(i==1)'79/100' else if(i==5)'10.0%' else sprintf('%.1f%%',c$valid)
  stopifnot(endsWith(evidence,suffix))
  if(language!='en')stopifnot(!grepl('Valid adjusted-survival|Events at tied times|Do not report the adjusted|Review causes of replicate|With substantial tied',xml2::xml_text(doc)))
  if(language=='ja')entries[[as.character(i)]]<-list(id=as.character(i),title=as.character(i),html=html)
 }
 main<-as.character(survival_cox_result_html_table(result,language));cells<-xml2::xml_text(xml2::xml_find_all(xml2::read_html(main,encoding='UTF-8'),'//th|//td'))
 if(language=='en')baseline<-cells else stopifnot(identical(cells,baseline))
 cat('PASS:',i,language,'bootstrap/ties branch, severity, counts, percentages and English main table\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
