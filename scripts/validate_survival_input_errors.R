Sys.setlocale('LC_CTYPE','English_United States.utf8')
Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
data <- data.frame(time=c(5,7,9,10),event=c(0,1,2,1),entry=c(6,1,0,2),start=c(6,0,0,0),stop=c(5,7,9,10),id=1:4,x=2:5)
scenarios <- list(
 missing_time=list(time='',code='missing_time_role'),
 missing_event=list(event='',code='missing_event_role'),
 entry_order=list(data_shape='entry_exit',entry='entry',code='entry_not_before_exit'),
 interval_order=list(data_shape='start_stop',start='start',stop='stop',subject_id='id',code='start_not_before_stop'),
 unknown_event=list(role3='unknown',code='invalid_event_map'),
 unconfirmed=list(event_map_confirmed=FALSE,code='event_map_not_confirmed')
)
expected <- list()
for(name in names(scenarios)) {
 config <- modifyList(list(objective='association',data_shape='single_record',event_structure='single',time='time',event='event',time_origin='Review 사용자 <&> %s',time_unit='day',role3='censored',event_map_confirmed=TRUE),scenarios[[name]])
 config$event_map <- if(nzchar(config$event))data.frame(raw_value=c('0','1','2'),role=c('censored','event_of_interest',config$role3),label=c('0','1','Review 사용자 <&> %s')) else NULL
 audit <- survival_contract_preflight(data,survival_contract_settings(config))
 stopifnot(!audit$ok,config$code %in% audit$issues$code)
 issue <- audit$issues[audit$issues$code==config$code,,drop=FALSE][1,]
 expected[[name]] <- list(config=config,languages=list())
 for(language in c('ja','zh','es','fr','de','vi','en','ko'))expected[[name]]$languages[[language]] <- survival_issue_text(issue$code,issue$message,language)
 cat('PASS:',name,'actual preflight:',paste(audit$issues$code,collapse=', '),'\n')
}
jsonlite::write_json(expected,'tmp/survival-input-errors.json',auto_unbox=TRUE,pretty=TRUE)
write.csv(data,'tmp/survival-input-errors.csv',row.names=FALSE)
