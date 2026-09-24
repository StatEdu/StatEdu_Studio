Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
grid<-expand.grid(objective=c('group_comparison','association','prediction','recurrent','competing','state_transition'),data_shape=c('single_record','entry_exit','start_stop','interval_censored'),event_structure=c('single','competing','recurrent','multistate'),competing_estimand=c('','cumulative_incidence','cause_specific','both'),time_dependent=c(FALSE,TRUE),stringsAsFactors=FALSE)
results<-lapply(seq_len(nrow(grid)),function(i)survival_recommend(as.list(grid[i,])))
results<-results[!duplicated(vapply(results,function(r)paste(r$rule_ids,collapse=','),character(1)))]
unknown<-c('Review','Normality','사용자 <&> %s','unregistered engine detail')
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 for(r in results){
  baseline<-r
  source<-unlist(r[c('primary','alternatives','warnings','confirmations','blocked_by')],use.names=FALSE)
  translated<-survival_recommendation_text(source,language)
  if(language=='en')stopifnot(identical(source,translated)) else stopifnot(all(translated!=source))
  doc<-xml2::read_html(as.character(survival_design_recommendation_panel(r,language)),encoding='UTF-8')
  stopifnot(grepl(survival_recommendation_text(r$primary,language),xml2::xml_text(doc),fixed=TRUE),identical(r,baseline))
  button<-xml2::xml_find_all(doc,'//button[@id="open_recommended_survival_analysis"]')
  stopifnot(length(button)==as.integer(r$status=='ready'))
 }
 stopifnot(identical(survival_recommendation_text(unknown,language),unknown),length(survival_recommendation_text(character(),language))==0)
 cat('PASS:',language,nrow(grid),'engine settings;',length(results),'distinct rule outcomes; translated names/notices; unknown text and routing preserved\n')
}
