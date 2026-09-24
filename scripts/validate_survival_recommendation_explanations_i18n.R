Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
grid<-expand.grid(objective=c('group_comparison','association','prediction','recurrent','competing','state_transition'),data_shape=c('single_record','entry_exit','start_stop','interval_censored'),event_structure=c('single','competing','recurrent','multistate'),competing_estimand=c('','cumulative_incidence','cause_specific','both'),time_dependent=c(FALSE,TRUE),stringsAsFactors=FALSE)
all_results<-lapply(seq_len(nrow(grid)),function(i)survival_recommend(as.list(grid[i,])))
results<-Filter(function(r)r$status=='ready',all_results)
results<-results[!duplicated(vapply(results,function(r)r$rule_ids[1],character(1)))]
stopifnot(setequal(vapply(results,function(r)r$rule_ids[1],character(1)),c('G01','A01','G02','A03','A04','A05','S03')))
results<-c(results,list(list(status='ready',primary='custom',rule_ids='unregistered'),list(status='ready',primary='custom',rule_ids=character())))
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 for(r in results){
  before<-r;en<-survival_recommendation_explanation(r,'en');actual<-survival_recommendation_explanation(r,language)
  stopifnot(identical(names(actual),names(en)),identical(lengths(actual),lengths(en)),identical(r,before))
  source<-unlist(en,use.names=FALSE);translated<-unlist(actual,use.names=FALSE)
  if(language=='en')stopifnot(identical(source,translated)) else stopifnot(all(translated[nzchar(source)]!=source[nzchar(source)]))
  doc<-xml2::read_html(as.character(survival_design_recommendation_panel(r,language)),encoding='UTF-8')
  section<-xml2::xml_find_first(doc,'//div[@class="survival-recommendation-explanation"]')
  for(s in translated[nzchar(translated)])stopifnot(grepl(s,xml2::xml_text(section),fixed=TRUE))
  outputs<-xml2::xml_text(xml2::xml_find_all(section,'.//li'))
  stopifnot(identical(outputs,actual$outputs))
 }
 cat('PASS:',language,'7 engine-selected rules + unknown/empty fallback; localized prose and ordered outputs; rules unchanged\n')
}
