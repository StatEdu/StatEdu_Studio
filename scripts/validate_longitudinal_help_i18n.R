Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
info<-data.frame(name=c('id','time','y','x','wt','aux'),measurement='continuous')
cases<-list(
 list(model='gee',weight=character(),type='none',strategy='complete',keys='empty'),
 list(model='gee',weight='wt',type='none',strategy='complete',keys='none'),
 list(model='gee',weight='wt',type='sampling',strategy='complete',keys='gee'),
 list(model='panel_fe',weight='wt',type='sampling',strategy='complete',keys='panel'),
 list(model='panel_re',weight='wt',type='sampling',strategy='complete',keys='panel'),
 list(model='lmm',weight='wt',type='none',strategy='available',keys=c('disabled','mixed')),
 list(model='glmm',weight='wt',type='none',strategy='available',keys=c('disabled','mixed')),
 list(model='gee',weight=character(),type='none',strategy='ipw',keys=c('empty','auxiliary')),
 list(model='gee',weight=character(),type='none',strategy='wgee',keys=c('empty','auxiliary')))
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 for(case in cases) {
  state<-longitudinal_setup_state(info$name,info,outcome='y',id='id',time='time',predictors='x',weight=case$weight,
   model_type=case$model,weight_type=case$type,missing_strategy=case$strategy,ipw_auxiliary='aux',language=lang)
  # The selected-weight/no-weight hint is a legacy/transient state; normal
  # setup resolution selects a supported weighted option automatically.
  if(identical(case$keys,'none'))state$weight_type<-'none'
  before<-serialize(state,NULL)
  doc<-xml2::read_html(as.character(longitudinal_setup_panel(state)),encoding='UTF-8');text<-xml2::xml_text(doc)
  for(key in case$keys) {
   message<-statedu_t(paste0('longitudinal.help.',key),lang)
   if(!grepl(message,text,fixed=TRUE))stop(paste(lang,case$model,case$strategy,key,state$missing_strategy))
   if(lang!='en')stopifnot(message!=statedu_t(paste0('longitudinal.help.',key),'en'))
  }
  stopifnot(identical(before,serialize(state,NULL)))
 }
 for(model in c('gee','lmm','glmm','panel_fe','panel_re')) {
  for(label in longitudinal_check_catalog(model)$label)if(lang!='en' && longitudinal_check_label(label,lang)==label && !label %in% 'Hausman FE vs RE')stop(paste(lang,'check label',label))
  for(strategy in unname(longitudinal_missing_strategy_choices(model)))if(lang!='en')stopifnot(
   longitudinal_missing_strategy_detail_ui(strategy,model,lang)!=longitudinal_missing_strategy_detail_ui(strategy,model,'en'))
  for(type in unname(longitudinal_weight_type_choices(model,TRUE)))if(lang!='en')stopifnot(
   longitudinal_weight_type_detail_ui(type,model,lang)!=longitudinal_weight_type_detail_ui(type,model,'en'))
 }
 for(trim in unname(longitudinal_weight_trim_choices()))if(lang!='en')stopifnot(
  longitudinal_weight_trim_detail_ui(trim,lang)!=longitudinal_weight_trim_detail_ui(trim,'en'))
 cat('PASS help and check labels:',lang,'\n')
}
