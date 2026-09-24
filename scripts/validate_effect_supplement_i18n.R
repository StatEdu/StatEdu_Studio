Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
find_render<-function(x,key){
 if(missing(x)||!is.call(x))return(NULL)
 if(identical(x[[1]],as.name('<-'))&&grepl(paste0('"',key,'"'),paste(deparse(x[[2]]),collapse=''),fixed=TRUE))return(x[[3]][[2]])
 for(item in as.list(x)[-1]){r<-find_render(item,key);if(!is.null(r))return(r)}
 NULL
}
out<-'tmp/effect-supplement-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
for(kind in c('wide','ci','long')){
 table<-data.frame(Outcome=c('Review','사용자 <&> %s'),Predictor=c('Normality','Primary'),check.names=FALSE)
 if(kind=='wide')for(effect in c('Direct','Indirect','Total'))for(suffix in c('beta','p','BH-adjusted p'))table[[paste(effect,suffix)]]<-c('.123','.456')
 if(kind=='ci')for(effect in c('Direct','Indirect','Total')){
  table[[paste(effect,'beta 95% CI')]]<-c('.100 ~ .300','')
  table[[paste(effect,'CI source')]]<-c('Model-based 95% CI','Not estimated - insufficient valid standardized bootstrap replicates')
 }
 if(kind=='long'){
  table$Effect<-c('Direct','Total');table$B<-c('.123','.456');table[['B 95% CI']]<-c('.100 ~ .300','');table$beta<-c('.234','.567');table$p<-c('.001','.020');table[['BH-adjusted p']]<-c('.002','.040');table[['CI source']]<-'Model-based 95% CI';table[['Inference source']]<-'Model-based normal-theory';table[['Valid bootstrap']]<-'';table[['Bootstrap status']]<-'Not requested';table[['BH family']]<-'External family Review';table$SE<-c('.111','.222')
 }
 source<-table
 expr<-find_render(body(structural_canvas_register_result_outputs),if(kind=='ci')'_result_structural_effect_ci'else'_result_structural_effects')
 for(language in c('en','ko','ja','zh','es','fr','de','vi')){
  env<-new.env(parent=globalenv());env$analysis_type<-'cbsem';env$appendix_result_table<-function(key)table;env$ui_language<-function()language;env$app_language_fn<-function()language
  render<-function()NULL;body(render)<-expr;environment(render)<-env
  html<-as.character(render());doc<-xml2::read_html(html,encoding='UTF-8');heading<-xml2::xml_text(xml2::xml_find_first(doc,'//h5'));notes<-xml2::xml_text(xml2::xml_find_all(doc,'//p'));cells<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//td')));headers<-xml2::xml_text(xml2::xml_find_all(doc,'//th'))
  stopifnot(all(paste(table$Predictor,'→',table$Outcome)%in%cells),identical(table,source))
  if(language=='en'){en_heading<-heading;en_notes<-notes}else{
   stopifnot(heading!=en_heading,all(notes!=en_notes))
   if(kind!='long')stopifnot(!any(c('Path','Direct effect','Indirect effect','Total effect')%in%headers))
   if(kind=='ci')stopifnot(!grepl('direct effects|indirect effects|total effects|Confidence-interval sources',notes[[1]]))
   stopifnot(!any(c('CI source','Inference source','Valid bootstrap','BH family')%in%headers))
   stopifnot(!any(c('Direct','Total','Model-based 95% CI','Model-based normal-theory','Not requested','Not estimated - insufficient valid standardized bootstrap replicates')%in%cells))
  }
  for(value in if(kind=='ci')'.100 ~ .300'else c('.123','.456'))stopifnot(value%in%cells)
  if(language=='ja')entries[[kind]]<-list(id=kind,title=kind,html=html)
  cat('PASS:',kind,language,'section, grouped headings, notes, user paths and numerical strings\n')
 }
}
stopifnot(is.null(structural_canvas_effect_summary_html_table(data.frame())),is.null(structural_canvas_effect_ci_source_note(data.frame())))
saveRDS(unname(entries),file.path(out,'entries.rds'))
