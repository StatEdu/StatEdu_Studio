Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/multilingual-program-audit';dir.create(out,recursive=TRUE,showWarnings=FALSE)
builders<-list();definitions<-parse('R/app_server.R',encoding='UTF-8')
collect<-function(x) {
 if(!is.call(x)&&!is.expression(x))return()
 if(is.call(x)&&identical(x[[1]],as.name('lazy_ui'))&&length(x)>=3L&&is.character(x[[2]]))builders[[x[[2]]]]<<-x[[3]]
 for(i in seq_along(x))if(!identical(x[[i]],quote(expr=))&&!is.symbol(x[[i]]))collect(x[[i]])
}
collect(definitions)
# User scope decision: the legacy mediation/moderation screen is retired.
builders[['lazy_analysis_mediation_moderation']]<-NULL
app_version<-saved_results_app_version()
session<-list(userData=list(merge_ui_values=list(),survival_design_revision=function()0,survival_design_values=list()))
structural_recommendation_selection<-shiny::reactiveValues(objective='measurement',construct='common_factor',indicator='continuous')
render_about_document<-function(key,value) {
 spec<-about_document_specs(app_language())[[key]]
 tab_panel_content(about_markdown_tab_panel(spec$title,value,spec$path,spec$subtitle,app_language()))
}
records<-list();candidates<-list();visible_candidates<-list();current_panel<-''
original_localized<-statedu_localized_text
statedu_localized_text<-function(language,en,ko=en) {
 value<-original_localized(language,en,ko)
 if(length(en)==1L&&length(value)==1L&&!is.na(en)&&!is.na(value)&&!language%in%c('en','ko')&&identical(en,value)&&grepl('[A-Za-z]{3,}.*[ ]+[A-Za-z]{3,}',en))
  candidates[[length(candidates)+1L]]<<-data.frame(panel=current_panel,language,english=en)
 value
}
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 app_language<-local({value<-lang;function()value});options(statedu.app_language=lang)
 for(id in names(builders)) {
  current_panel<-id
  start_candidates<-length(candidates)
  result<-tryCatch({html<-as.character(htmltools::renderTags(eval(builders[[id]])())$html);doc<-xml2::read_html(html)
   xml2::xml_remove(xml2::xml_find_all(doc,'//script|//style'))
   text<-xml2::xml_text(doc)
   if(length(candidates)>start_candidates)for(k in seq.int(start_candidates+1L,length(candidates))) {
    entry<-candidates[[k]]
    if(grepl(entry$english,text,fixed=TRUE))visible_candidates[[length(visible_candidates)+1L]]<-entry
   }
   visible<-xml2::xml_text(xml2::xml_find_all(doc,'//label|//button|//h2|//h3|//h4|//h5|//option'))
   data.frame(panel=id,language=lang,status='rendered',controls=length(xml2::xml_find_all(doc,'//input|//select|//textarea')),korean_label_candidates=if(lang!='ko')sum(grepl('[가-힣]',visible))else 0,error='')
  },error=function(e)data.frame(panel=id,language=lang,status='error',controls=0,korean_label_candidates=0,error=conditionMessage(e)))
  records[[length(records)+1L]]<-result
 }
 cat('RENDER',lang,length(builders),'registered panels\n')
}
report<-do.call(rbind,records);write.csv(report,file.path(out,'all-panels.csv'),row.names=FALSE,fileEncoding='UTF-8')
if(length(candidates))write.csv(unique(do.call(rbind,candidates)),file.path(out,'panel-fallback-candidates.csv'),row.names=FALSE,fileEncoding='UTF-8')
if(length(visible_candidates))write.csv(unique(do.call(rbind,visible_candidates)),file.path(out,'visible-fallback-candidates.csv'),row.names=FALSE,fileEncoding='UTF-8')
print(table(report$status));print(report[report$status!='rendered',c('panel','language','error')],row.names=FALSE)
cat('Panel render audit is not browser interaction or complete translation proof.\n')
