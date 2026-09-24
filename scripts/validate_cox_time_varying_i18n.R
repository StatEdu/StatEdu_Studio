Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/cox-time-varying-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
d<-read.csv('scripts/fixtures/survival_validation.csv')
result<-prepare_cox_analysis_result(d,'time','status',c('age','sex'),event_value='1',time_varying_covariate='age',time_varying_times='100, 250, 500')
stopifnot(nrow(result$time_varying_test_table)>0,nrow(result$time_varying_at_times)==3)
custom<-result;custom$time_varying_test_table$Variable<-'Review';custom$time_varying_at_times$Variable<-'사용자 <&> %s'
lines<-readLines('R/result_survival_ui.R',encoding='UTF-8')
notes<-sub('",?\\s*$','',sub('^\\s*"','',lines[grepl('^\\s*"(For this time-varying-coefficient analysis|The model is β)',lines)]))
stopifnot(length(notes)==2)
for(kind in c('actual','custom'))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 options(statedu.app_language=language);r<-if(kind=='actual')result else custom
 tables<-list(survival_cox_time_varying_test_display_table(r),survival_cox_time_specific_hr_table(r))
 titles<-c('Time-varying coefficient analysis','Time-specific hazard ratios')
 panels<-lapply(1:2,function(i)tagList(tags$h4(survival_appendix_title(titles[i],language)),survival_simple_table(tables[[i]],table_language=language)))
 doc<-xml2::read_html(paste(vapply(panels,as.character,character(1)),collapse=''),encoding='UTF-8')
 values<-xml2::xml_text(xml2::xml_find_all(doc,'//td'))
 if(language=='en')baseline<-values else{
  stopifnot(identical(values,baseline),!any(titles%in%xml2::xml_text(xml2::xml_find_all(doc,'//h4'))),
   !any(c('Time function','Interaction B')%in%xml2::xml_text(xml2::xml_find_all(doc,'//th'))))
 }
 full<-xml2::read_html(as.character(survival_cox_results_panel(r,language)),encoding='UTF-8')
 if(language!='en')for(n in notes)stopifnot(!grepl(substr(n,1,60),xml2::xml_text(full),fixed=TRUE))
 if(language%in%c('ja','zh','es','fr','de','vi'))for(n in notes){
  translated<-statedu_localized_text(language,n)
  stopifnot(translated!=n,grepl(substr(translated,1,30),xml2::xml_text(full),fixed=TRUE))
 }
 main<-xml2::xml_text(xml2::xml_find_all(xml2::read_html(as.character(survival_cox_result_html_table(result,language)),encoding='UTF-8'),'//th|//td'))
 if(language=='en')main_baseline<-main else stopifnot(identical(main,main_baseline))
 if(language=='ja')entries[[kind]]<-list(id=kind,title=kind,html=as.character(tagList(panels,lapply(notes,function(n)survival_table_note(statedu_localized_text(language,n))))))
 cat('PASS:',kind,language,'actual time-varying Cox, notes, headers, values and English main table\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
