Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
check_name<-'None Not applied. 사용자 <&> %s\nWeight type: test'
check_summary<-longitudinal_weight_summary_table(check_name,'sampling','p01_99',base_weights=c(1,2),final_weights=c(2/3,4/3),note='Selected sampling/baseline longitudinal weights were applied.')
weight_checklist<-longitudinal_reporting_checklist(list(weight_summary=check_summary,software_versions=data.frame(Software='R',Version='4.5.3')))
check_row<-which(weight_checklist$Item=='Analysis weights described')
checklist_missing<-character(0)
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 translated<-longitudinal_appendix_table(weight_checklist,lang)
 if(!grepl(check_name,translated[[3]][check_row],fixed=TRUE))stop('Checklist variable changed: ',lang)
 expected_table<-longitudinal_appendix_table(longitudinal_display_weight_summary_table(list(weight_summary=check_summary)),lang)
 expected<-paste(sprintf('%s: %s',expected_table[[1]],expected_table[[2]]),collapse=' ')
 stopifnot(identical(translated[[3]][check_row],expected))
 if(lang!='en') {
  for(i in seq_len(nrow(weight_checklist))) {
   for(j in 1:2)if(identical(translated[[j]][i],weight_checklist[[j]][i]))checklist_missing<-unique(c(checklist_missing,paste(lang,weight_checklist[[j]][i],sep=' | ')))
   if(i!=check_row && weight_checklist$Item[i]!='Software/package version reported' && nzchar(weight_checklist$Details[i]) && identical(translated[[3]][i],weight_checklist$Details[i]))checklist_missing<-unique(c(checklist_missing,paste(lang,weight_checklist$Details[i],sep=' | ')))
  }
 }
 cat('PASS weight checklist:',lang,'\n')
}
if(length(checklist_missing))stop(paste(unique(sub('^[^|]+[|] ', '',checklist_missing)),collapse='\n'))
