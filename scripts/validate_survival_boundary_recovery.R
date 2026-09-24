Sys.setlocale('LC_CTYPE','English_United States.utf8')
Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
data <- data.frame(time=c(5,7,9,10),event=c(0,1,2,1),start=c(0,0,7,9),stop=c(5,7,9,10),id=1:4,afterid=c(1,2,2,3),duplicateid=c(1,2,3,2),x=2:5)
base <- list(survival_design_objective='association',survival_design_shape='single_record',survival_design_events='single',survival_design_time_dependent=FALSE,survival_contract_origin='Review 사용자 <&> %s',survival_contract_unit='day',survival_contract_time='time',survival_contract_event='event',survival_event_role_1='censored',survival_event_role_2='event_of_interest',survival_event_role_3='censored',survival_event_map_confirmed=TRUE)
custom <- '사용자 주기 Review <&> %s'
cases <- list(
 multiple_interest=list(bad=list(survival_event_role_3='event_of_interest'),fix=list(survival_event_role_3='censored'),code='event_of_interest_count'),
 custom_unit=list(bad=list(survival_contract_unit='other',survival_contract_custom_unit=''),fix=list(survival_contract_custom_unit=custom),preserve=list(survival_contract_custom_unit=custom),code='missing_time_unit'),
 after_event=list(bad=list(survival_design_shape='start_stop',survival_design_time_dependent=TRUE,survival_contract_start='start',survival_contract_stop='stop',survival_contract_subject_id='afterid'),fix=list(survival_contract_subject_id='id'),code='interval_after_event'),
 duplicate_event=list(bad=list(survival_design_shape='start_stop',survival_design_time_dependent=TRUE,survival_contract_start='start',survival_contract_stop='stop',survival_contract_subject_id='duplicateid'),fix=list(survival_contract_subject_id='id'),code='multiple_subject_events')
)
audit_ui <- function(ui) {
 values <- list(objective=ui$survival_design_objective,event_structure=ui$survival_design_events,data_shape=ui$survival_design_shape,time_origin=ui$survival_contract_origin,time_unit=ui$survival_contract_unit,custom_time_unit=ui$survival_contract_custom_unit,event_map_confirmed=ui$survival_event_map_confirmed)
 for(field in c('time','start','stop','subject_id','event'))values[[field]] <- ui[[paste0('survival_contract_',field)]] %||% ''
 values$event_map <- data.frame(raw_value=c('0','1','2'),role=vapply(1:3,function(i)ui[[paste0('survival_event_role_',i)]],character(1)),label=c('0','1','사용자 Review <&> %s'))
 survival_contract_preflight(data,survival_contract_settings(values))
}
for(name in names(cases)) {
 case <- cases[[name]];ui <- modifyList(base,case$bad)
 bad <- audit_ui(ui);good <- audit_ui(modifyList(ui,case$fix))
 stopifnot(!bad$ok,case$code %in% bad$issues$code,good$ok)
 if(name=='custom_unit')stopifnot(identical(good$settings$time_unit,custom))
 row <- bad$issues[bad$issues$code==case$code,,drop=FALSE][1,]
 case$config <- ui;case$languages <- list()
 for(lang in c('ja','zh','es','fr','de','vi','en','ko'))case$languages[[lang]] <- survival_issue_text(row$code,row$message,lang)
 cases[[name]] <- case
 cat('PASS:',name,'blocked:',paste(bad$issues$code,collapse=', '),'; corrected input accepted\n')
}
jsonlite::write_json(cases,'tmp/survival-boundary-recovery.json',auto_unbox=TRUE,pretty=TRUE)
write.csv(data,'tmp/survival-boundary-recovery.csv',row.names=FALSE)
