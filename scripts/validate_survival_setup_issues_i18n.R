Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
catalog<-jsonlite::fromJSON('i18n/ja.json')$translations
keys<-grep('^survival.setup.issue.',names(catalog),value=TRUE)
codes<-sub('^survival.setup.issue.','',keys)
stopifnot(length(codes)==24)
raw<-'Review Normality None 사용자 <&> %s'
actual<-survival_preflight(data.frame(t=c(1,2),e=c(0,1)),list(data_shape='single_record',roles=list()))
stopifnot(all(c('missing_time_role','missing_event_role') %in% actual$issues$code))
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 translated<-survival_issue_text(codes,rep(raw,length(codes)),lang)
 stopifnot(length(translated)==24,identical(survival_issue_text('unregistered',raw,lang),raw))
 if(lang=='en')stopifnot(all(translated==raw))else stopifnot(all(nzchar(translated)),all(translated!=raw))
 if(!lang %in% c('ko','en')) {
  expected<-unname(unlist(jsonlite::fromJSON(paste0('i18n/',lang,'.json'))$translations[keys]))
  stopifnot(identical(translated,expected))
 }
 for(audit in list(list(counts=list(source_rows=137L,analysis_rows=112L,events=43L),issues=data.frame(code=c(codes,'unregistered'),message=raw)),actual)) {
  before<-audit
  r<-list(status='blocked',primary='Input check',preflight=audit)
  doc<-xml2::read_html(as.character(survival_design_recommendation_panel(r,lang)),encoding='UTF-8')
  items<-xml2::xml_text(xml2::xml_find_all(doc,'//div[contains(@class,"survival-design-result-card")]/ul[1]/li'))
  expected<-survival_issue_text(audit$issues$code,audit$issues$message,lang)
  if(lang!='ko')expected<-sprintf('[%s] %s',audit$issues$code,expected)
  stopifnot(identical(trimws(items),expected),identical(audit,before))
 }
 cat('PASS:',lang,'24 known issues + unknown raw text; real missing-role preflight; rendered panel; codes and audit unchanged\n')
}
